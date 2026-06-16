(* Sink/source serialization driven by a [Codec.t] type description.

   The Marshal library is the {b side-effect} side of the codec
   ecosystem: encoding writes tokens to a caller-provided sink (a
   [Buffer.t], an [out_channel], …); decoding reads from a
   caller-provided input (typically a parsed AST). No intermediate
   value is allocated by the framework itself.

   The companion {!Codec} library covers the pure value-conversion
   side (OCaml value <-> Yojson.Safe.t and similar). Same
   {!Codec.t} GADT, different semantics. *)

module Writer = struct
  module type S = sig
    type out

    val null   : out -> unit
    val bool   : out -> bool -> unit
    val int    : out -> int -> unit
    val int32  : out -> int32 -> unit
    val int64  : out -> int64 -> unit
    val float  : out -> float -> unit
    val char   : out -> char -> unit
    val string : out -> string -> unit

    val begin_array : out -> unit
    val array_sep   : out -> unit
    val end_array   : out -> unit

    val begin_object : out -> unit
    val key          : out -> first:bool -> string -> unit
    val end_object   : out -> unit

    val variant_constant : out -> string -> unit
    val variant_payload  : out -> string -> (out -> unit) -> unit
  end
end

module Reader = struct
  module type S = sig
    type input

    val null    : input -> (unit,   Codec.error) result
    val bool    : input -> (bool,   Codec.error) result
    val int     : input -> (int,    Codec.error) result
    val int32   : input -> (int32,  Codec.error) result
    val int64   : input -> (int64,  Codec.error) result
    val float   : input -> (float,  Codec.error) result
    val char    : input -> (char,   Codec.error) result
    val string  : input -> (string, Codec.error) result
    val array   : input -> (input list, Codec.error) result
    val object_ : input -> ((string * input) list, Codec.error) result
  end
end

(* -- Internal: serialize traversal --------------------------------------- *)

exception Bail of Codec.error

let bail e = raise (Bail e)

let with_path prefix f =
  try f () with Bail e -> raise (Bail (Codec.Error.prepend_path prefix e))

let serialize_via (type s)
    (module W : Writer.S with type out = s)
    (codec : 'a Codec.t) (value : 'a) (out : s)
  : unit =
  let rec go : type a. a Codec.t -> a -> unit = fun repr v ->
    match repr with
    | Codec.Unit   -> W.null out
    | Codec.Bool   -> W.bool out v
    | Codec.Int    -> W.int out v
    | Codec.Int32  -> W.int32 out v
    | Codec.Int64  -> W.int64 out v
    | Codec.Float  -> W.float out v
    | Codec.Char   -> W.char out v
    | Codec.String -> W.string out v
    | Codec.Option r ->
      (match v with None -> W.null out | Some x -> go r x)
    | Codec.Tuple2 (r1, r2) ->
      let a, b = v in
      W.begin_array out;
      go r1 a; W.array_sep out;
      go r2 b;
      W.end_array out
    | Codec.Tuple3 (r1, r2, r3) ->
      let a, b, c = v in
      W.begin_array out;
      go r1 a; W.array_sep out;
      go r2 b; W.array_sep out;
      go r3 c;
      W.end_array out
    | Codec.Tuple4 (r1, r2, r3, r4) ->
      let a, b, c, d = v in
      W.begin_array out;
      go r1 a; W.array_sep out;
      go r2 b; W.array_sep out;
      go r3 c; W.array_sep out;
      go r4 d;
      W.end_array out
    | Codec.Tuple5 (r1, r2, r3, r4, r5) ->
      let a, b, c, d, e = v in
      W.begin_array out;
      go r1 a; W.array_sep out;
      go r2 b; W.array_sep out;
      go r3 c; W.array_sep out;
      go r4 d; W.array_sep out;
      go r5 e;
      W.end_array out
    | Codec.Tuple6 (r1, r2, r3, r4, r5, r6) ->
      let a, b, c, d, e, f = v in
      W.begin_array out;
      go r1 a; W.array_sep out;
      go r2 b; W.array_sep out;
      go r3 c; W.array_sep out;
      go r4 d; W.array_sep out;
      go r5 e; W.array_sep out;
      go r6 f;
      W.end_array out
    | Codec.Collection { iter; element_codec; _ } ->
      W.begin_array out;
      let first = ref true in
      iter (fun elem ->
        if !first then first := false else W.array_sep out;
        go element_codec elem
      ) v;
      W.end_array out
    | Codec.Record fields ->
      W.begin_object out;
      let _ : bool = serialize_fields fields v true in
      W.end_object out
    | Codec.Variant { vname; cases } ->
      serialize_variant vname cases v
    | Codec.Map { repr; backward; _ } ->
      go repr (backward v)
    | Codec.Lazy l ->
      go (Lazy.force l) v
  and serialize_fields : type f r.
    (f, r) Codec.fields -> r -> bool -> bool =
    fun fields v first ->
    match fields with
    | Codec.F0 _ -> first
    | Codec.Field { rest; name; repr; get; _ } ->
      let first = serialize_fields rest v first in
      W.key out ~first name;
      go repr (get v);
      false
  and serialize_variant : type v. string -> v Codec.case list -> v -> unit =
    fun vname cases v ->
    let rec loop = function
      | [] -> bail (Codec.Error.make [ vname ] "no matching case for variant value")
      | Codec.Case { name; repr; destruct; _ } :: rest ->
        (match destruct v with
         | None -> loop rest
         | Some payload ->
           W.variant_payload out name (fun _ ->
             with_path name (fun () -> go repr payload)))
      | Codec.Case0 { name; match_; _ } :: rest ->
        if match_ v then W.variant_constant out name else loop rest
    in
    loop cases
  in
  go codec value

let serialize (type s)
    (codec : 'a Codec.t) (value : 'a)
    ~(writer : (module Writer.S with type out = s))
    (out : s)
  : (unit, Codec.error) result =
  try Ok (serialize_via writer codec value out) with Bail e -> Error e

(* -- Internal: deserialize traversal ------------------------------------- *)

let ( let* ) = Result.bind
let ( let+ ) r f = Result.map f r

let deserialize_via (type i)
    (module R : Reader.S with type input = i)
    (codec : 'a Codec.t) (input : i)
  : ('a, Codec.error) result =
  let rec go : type a. a Codec.t -> i -> (a, Codec.error) result = fun repr input ->
    match repr with
    | Codec.Unit   -> R.null input
    | Codec.Bool   -> R.bool input
    | Codec.Int    -> R.int input
    | Codec.Int32  -> R.int32 input
    | Codec.Int64  -> R.int64 input
    | Codec.Float  -> R.float input
    | Codec.Char   -> R.char input
    | Codec.String -> R.string input
    | Codec.Option r ->
      (match R.null input with
       | Ok () -> Ok None
       | Error _ -> let+ v = go r input in Some v)
    | Codec.Tuple2 (r1, r2) ->
      (match R.array input with
       | Ok [ j1; j2 ] ->
         let* a = Codec.Error.with_path "0" (go r1 j1) in
         let+ b = Codec.Error.with_path "1" (go r2 j2) in
         (a, b)
       | Ok _ -> Error (Codec.Error.make [] "tuple2: wrong arity")
       | Error _ as e -> e)
    | Codec.Tuple3 (r1, r2, r3) ->
      (match R.array input with
       | Ok [ j1; j2; j3 ] ->
         let* a = Codec.Error.with_path "0" (go r1 j1) in
         let* b = Codec.Error.with_path "1" (go r2 j2) in
         let+ c = Codec.Error.with_path "2" (go r3 j3) in
         (a, b, c)
       | Ok _ -> Error (Codec.Error.make [] "tuple3: wrong arity")
       | Error _ as e -> e)
    | Codec.Tuple4 (r1, r2, r3, r4) ->
      (match R.array input with
       | Ok [ j1; j2; j3; j4 ] ->
         let* a = Codec.Error.with_path "0" (go r1 j1) in
         let* b = Codec.Error.with_path "1" (go r2 j2) in
         let* c = Codec.Error.with_path "2" (go r3 j3) in
         let+ d = Codec.Error.with_path "3" (go r4 j4) in
         (a, b, c, d)
       | Ok _ -> Error (Codec.Error.make [] "tuple4: wrong arity")
       | Error _ as e -> e)
    | Codec.Tuple5 (r1, r2, r3, r4, r5) ->
      (match R.array input with
       | Ok [ j1; j2; j3; j4; j5 ] ->
         let* a = Codec.Error.with_path "0" (go r1 j1) in
         let* b = Codec.Error.with_path "1" (go r2 j2) in
         let* c = Codec.Error.with_path "2" (go r3 j3) in
         let* d = Codec.Error.with_path "3" (go r4 j4) in
         let+ e = Codec.Error.with_path "4" (go r5 j5) in
         (a, b, c, d, e)
       | Ok _ -> Error (Codec.Error.make [] "tuple5: wrong arity")
       | Error _ as e -> e)
    | Codec.Tuple6 (r1, r2, r3, r4, r5, r6) ->
      (match R.array input with
       | Ok [ j1; j2; j3; j4; j5; j6 ] ->
         let* a = Codec.Error.with_path "0" (go r1 j1) in
         let* b = Codec.Error.with_path "1" (go r2 j2) in
         let* c = Codec.Error.with_path "2" (go r3 j3) in
         let* d = Codec.Error.with_path "3" (go r4 j4) in
         let* e = Codec.Error.with_path "4" (go r5 j5) in
         let+ f = Codec.Error.with_path "5" (go r6 j6) in
         (a, b, c, d, e, f)
       | Ok _ -> Error (Codec.Error.make [] "tuple6: wrong arity")
       | Error _ as e -> e)
    | Codec.Collection { builder; element_codec; _ } ->
      (match R.array input with
       | Error _ as e -> e
       | Ok items ->
         let sink, finalize = builder () in
         let exception B of Codec.error in
         (try
            List.iteri (fun i j ->
              match Codec.Error.with_path (string_of_int i) (go element_codec j) with
              | Ok v -> sink v
              | Error e -> raise (B e)
            ) items;
            Ok (finalize ())
          with B e -> Error e))
    | Codec.Record fields ->
      (match R.object_ input with
       | Error _ as e -> e
       | Ok assoc -> deserialize_fields fields assoc)
    | Codec.Variant { vname; cases } ->
      deserialize_variant vname cases input
    | Codec.Map { repr; forward; _ } ->
      let+ v = go repr input in forward v
    | Codec.Lazy l ->
      go (Lazy.force l) input
  and deserialize_fields : type f r.
    (f, r) Codec.fields -> (string * i) list -> (f, Codec.error) result =
    fun fields assoc ->
    match fields with
    | Codec.F0 { constructor; _ } -> Ok constructor
    | Codec.Field { rest; name; repr; default; _ } ->
      let* f = deserialize_fields rest assoc in
      (match List.assoc_opt name assoc with
       | None ->
         (match default with
          | Some d -> Ok (f d)
          | None ->
            Error (Codec.Error.make [ name ] "missing required field"
                     ~expected:(Printf.sprintf "field '%s'" name)))
       | Some j ->
         match R.null j, default with
         | Ok (), Some d -> Ok (f d)
         | _ ->
           let+ v = Codec.Error.with_path name (go repr j) in f v)
  and deserialize_variant : type v.
    string -> v Codec.case list -> i -> (v, Codec.error) result =
    fun vname cases input ->
    match R.string input with
    | Ok name ->
      let rec find = function
        | [] ->
          Error (Codec.Error.make [ vname ]
                   (Printf.sprintf "unknown variant case '%s'" name))
        | Codec.Case0 { name = n; value; _ } :: _ when String.equal n name ->
          Ok value
        | _ :: rest -> find rest
      in
      find cases
    | Error _ ->
      (match R.array input with
       | Ok [ name_j; arg_j ] ->
         let* name = Codec.Error.with_path "variant tag" (R.string name_j) in
         let rec find = function
           | [] ->
             Error (Codec.Error.make [ vname ]
                      (Printf.sprintf "unknown variant case '%s'" name))
           | Codec.Case { name = n; repr; construct; _ } :: _
             when String.equal n name ->
             let+ v = Codec.Error.with_path name (go repr arg_j) in
             construct v
           | _ :: rest -> find rest
         in
         find cases
       | _ ->
         Error (Codec.Error.make [ vname ]
                  "variant must be a string (constant) or a [name, arg] array"))
  in
  go codec input

let deserialize (type i)
    (codec : 'a Codec.t)
    ~(reader : (module Reader.S with type input = i))
    (input : i)
  : ('a, Codec.error) result =
  deserialize_via reader codec input
