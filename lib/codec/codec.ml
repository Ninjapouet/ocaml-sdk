module Error = struct
  type path = string list

  type t = {
    path : path;
    message : string;
    expected : string option;
    got : string option;
  }

  let make ?expected ?got path message =
    { path; message; expected; got }

  let pp ppf { path; message; expected; got } =
    (match path with
     | [] -> ()
     | p -> Fmt.pf ppf "%s: " (String.concat "." p));
    Fmt.string ppf message;
    match expected, got with
    | Some e, Some g -> Fmt.pf ppf " (expected %s, got %s)" e g
    | Some e, None   -> Fmt.pf ppf " (expected %s)" e
    | None,   Some g -> Fmt.pf ppf " (got %s)" g
    | None,   None   -> ()

  let to_string = Fmt.to_to_string pp

  let prepend_path prefix e =
    { e with path = prefix :: e.path }

  let with_path prefix result =
    Result.map_error (prepend_path prefix) result

  exception Codec_error of t
end

type error = Error.t

(* -- Type representation GADT --------------------------------------------- *)

type _ t =
  | Unit : unit t
  | Bool : bool t
  | Int : int t
  | Int32 : int32 t
  | Int64 : int64 t
  | Float : float t
  | Char : char t
  | String : string t
  | Option : 'a t -> 'a option t
  | Tuple2 : 'a t * 'b t -> ('a * 'b) t
  | Tuple3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tuple4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Tuple5 : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) t
  | Tuple6 : 'a t * 'b t * 'c t * 'd t * 'e t * 'f t -> ('a * 'b * 'c * 'd * 'e * 'f) t
  | Collection : ('container, 'elem) collection_desc -> 'container t
  | Record : ('r, 'r) fields -> 'r t
  | Variant : 'v variant_desc -> 'v t
  | Map : ('a, 'b) map_desc -> 'b t
  | Lazy : 'a t lazy_t -> 'a t

and ('container, 'elem) collection_desc = {
  iter : ('elem -> unit) -> 'container -> unit;
  builder : unit -> ('elem -> unit) * (unit -> 'container);
  element_codec : 'elem t;
}

and (_, _) fields =
  | F0 : { record_name : string; constructor : 'f } -> ('f, 'r) fields
  | Field : {
      rest : ('a -> 'f, 'r) fields;
      name : string;
      repr : 'a t;
      get : 'r -> 'a;
      default : 'a option;
    } -> ('f, 'r) fields

and 'v case =
  | Case : {
      name : string;
      repr : 'a t;
      destruct : 'v -> 'a option;
      construct : 'a -> 'v;
    } -> 'v case
  | Case0 : {
      name : string;
      value : 'v;
      match_ : 'v -> bool;
    } -> 'v case

and 'v variant_desc = {
  vname : string;
  cases : 'v case list;
}

and ('a, 'b) map_desc = {
  repr : 'a t;
  forward : 'a -> 'b;
  backward : 'b -> 'a;
}

(* -- Primitives ----------------------------------------------------------- *)

let unit = Unit
let bool = Bool
let int = Int
let int32 = Int32
let int64 = Int64
let float = Float
let char = Char
let string = String

let option r = Option r

let tuple2 a b = Tuple2 (a, b)
let tuple3 a b c = Tuple3 (a, b, c)
let tuple4 a b c d = Tuple4 (a, b, c, d)
let tuple5 a b c d e = Tuple5 (a, b, c, d, e)
let tuple6 a b c d e f = Tuple6 (a, b, c, d, e, f)

let map forward backward repr = Map { repr; forward; backward }
let lazy_ l = Lazy l

(* -- Collection combinators ----------------------------------------------- *)

let collection ~iter ~builder element_codec =
  Collection { iter; builder; element_codec }

let list elem =
  Collection {
    iter = List.iter;
    builder = (fun () ->
      let acc = ref [] in
      ((fun x -> acc := x :: !acc),
       (fun () -> List.rev !acc)));
    element_codec = elem;
  }

let array elem =
  Collection {
    iter = Array.iter;
    builder = (fun () ->
      let acc = ref [] in
      ((fun x -> acc := x :: !acc),
       (fun () -> Array.of_list (List.rev !acc))));
    element_codec = elem;
  }

let seq elem =
  Collection {
    iter = Seq.iter;
    builder = (fun () ->
      let acc = ref [] in
      ((fun x -> acc := x :: !acc),
       (fun () -> List.to_seq (List.rev !acc))));
    element_codec = elem;
  }

let queue elem =
  Collection {
    iter = Queue.iter;
    builder = (fun () ->
      let q = Queue.create () in
      ((fun x -> Queue.add x q),
       (fun () -> q)));
    element_codec = elem;
  }

let hashtbl key value =
  Collection {
    iter = (fun f h -> Hashtbl.iter (fun k v -> f (k, v)) h);
    builder = (fun () ->
      let h = Hashtbl.create 16 in
      ((fun (k, v) -> Hashtbl.add h k v),
       (fun () -> h)));
    element_codec = Tuple2 (key, value);
  }

(* -- Functorial wrappers for Map.Make / Set.Make -------------------------- *)

module Map = struct
  module type S = sig
    type key
    type +!'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
  end

  module Make (M : S) = struct
    let codec key_codec value_codec =
      Collection {
        iter = (fun f m -> M.iter (fun k v -> f (k, v)) m);
        builder = (fun () ->
          let acc = ref M.empty in
          ((fun (k, v) -> acc := M.add k v !acc),
           (fun () -> !acc)));
        element_codec = Tuple2 (key_codec, value_codec);
      }
  end
end

module Set = struct
  module type S = sig
    type elt
    type t
    val empty : t
    val add : elt -> t -> t
    val iter : (elt -> unit) -> t -> unit
  end

  module Make (S : S) = struct
    let codec elt_codec =
      Collection {
        iter = S.iter;
        builder = (fun () ->
          let acc = ref S.empty in
          ((fun x -> acc := S.add x !acc),
           (fun () -> !acc)));
        element_codec = elt_codec;
      }
  end
end

(* -- Record builder ------------------------------------------------------- *)

let record name constructor = F0 { record_name = name; constructor }

let field ?default name repr get rest =
  Field { rest; name; repr; get; default }

let field_opt name repr get rest =
  Field { rest; name; repr = Option repr; get; default = Some None }

let seal fields = Record fields

(* -- Variant builder ------------------------------------------------------ *)

let case name repr destruct construct =
  Case { name; repr; destruct; construct }

let case0 name value =
  Case0 { name; value; match_ = (fun v -> v == value) }

let variant vname cases = Variant { vname; cases }

type 'a codec = 'a t

(* -- Value-conversion API -------------------------------------------------

   [Writer.S] is a module of CONSTRUCTORS that build a value of type
   [t] from OCaml primitives; [Reader.S] is a module of EXTRACTORS that
   pull OCaml primitives from a value of type [t]. Both are pure and
   value-based — no side effects, no sink.

   Use this for "I have an OCaml value, I want a Yojson.Safe.t"-style
   conversions, or any other value-to-value transformation. For
   sink-based serialization (write to a Buffer/channel without
   materializing an intermediate value), use the [Marshal] library. *)

module Writer = struct
  module type S = sig
    type t
    val null   : t
    val bool   : bool -> t
    val int    : int -> t
    val int32  : int32 -> t
    val int64  : int64 -> t
    val float  : float -> t
    val char   : char -> t
    val string : string -> t
    val list   : t list -> t
    val record : (string * t) list -> t
    val variant_constant : string -> t
    val variant_payload  : string -> t -> t
  end
end

module Reader = struct
  module type S = sig
    type t
    val null    : t -> (unit,   error) result
    val bool    : t -> (bool,   error) result
    val int     : t -> (int,    error) result
    val int32   : t -> (int32,  error) result
    val int64   : t -> (int64,  error) result
    val float   : t -> (float,  error) result
    val char    : t -> (char,   error) result
    val string  : t -> (string, error) result
    val list    : t -> (t list, error) result
    val record  : t -> ((string * t) list, error) result
  end
end

(* -- Internal: GADT traversal for encode --------------------------------- *)

(* Encoding can fail only on an unmatched variant case; use an
   exception internally and convert at the API boundary. *)
exception Bail of error

let bail e = raise (Bail e)

let with_path prefix f =
  try f () with Bail e -> raise (Bail (Error.prepend_path prefix e))

let encode_via (type b)
    (module W : Writer.S with type t = b)
    (codec : 'a t) (value : 'a)
  : b =
  let rec go : type a. a t -> a -> b = fun repr v ->
    match repr with
    | Unit   -> W.null
    | Bool   -> W.bool v
    | Int    -> W.int v
    | Int32  -> W.int32 v
    | Int64  -> W.int64 v
    | Float  -> W.float v
    | Char   -> W.char v
    | String -> W.string v
    | Option r ->
      (match v with None -> W.null | Some x -> go r x)
    | Tuple2 (r1, r2) ->
      let a, b = v in
      W.list [ go r1 a; go r2 b ]
    | Tuple3 (r1, r2, r3) ->
      let a, b, c = v in
      W.list [ go r1 a; go r2 b; go r3 c ]
    | Tuple4 (r1, r2, r3, r4) ->
      let a, b, c, d = v in
      W.list [ go r1 a; go r2 b; go r3 c; go r4 d ]
    | Tuple5 (r1, r2, r3, r4, r5) ->
      let a, b, c, d, e = v in
      W.list [ go r1 a; go r2 b; go r3 c; go r4 d; go r5 e ]
    | Tuple6 (r1, r2, r3, r4, r5, r6) ->
      let a, b, c, d, e, f = v in
      W.list [ go r1 a; go r2 b; go r3 c; go r4 d; go r5 e; go r6 f ]
    | Collection { iter; element_codec; _ } ->
      let items = ref [] in
      iter (fun elem -> items := go element_codec elem :: !items) v;
      W.list (List.rev !items)
    | Record fields ->
      W.record (encode_fields fields v [])
    | Variant { vname; cases } ->
      encode_variant vname cases v
    | Map { repr; backward; _ } ->
      go repr (backward v)
    | Lazy l ->
      go (Lazy.force l) v
  and encode_fields : type f r.
    (f, r) fields -> r -> (string * b) list -> (string * b) list =
    fun fields v acc ->
    match fields with
    | F0 _ -> acc
    | Field { rest; name; repr; get; _ } ->
      let item = (name, go repr (get v)) in
      encode_fields rest v (item :: acc)
  and encode_variant : type v. string -> v case list -> v -> b =
    fun vname cases v ->
    let rec loop = function
      | [] -> bail (Error.make [ vname ] "no matching case for variant value")
      | Case { name; repr; destruct; _ } :: rest ->
        (match destruct v with
         | None -> loop rest
         | Some payload ->
           W.variant_payload name
             (with_path name (fun () -> go repr payload)))
      | Case0 { name; match_; _ } :: rest ->
        if match_ v then W.variant_constant name else loop rest
    in
    loop cases
  in
  go codec value

let encode (type b)
    (codec : 'a t) (value : 'a)
    ~(writer : (module Writer.S with type t = b))
  : (b, error) result =
  try Ok (encode_via writer codec value) with Bail e -> Error e

(* -- Internal: GADT traversal for decode --------------------------------- *)

let ( let* ) = Result.bind
let ( let+ ) r f = Result.map f r

let decode_via (type b)
    (module R : Reader.S with type t = b)
    (codec : 'a t) (input : b)
  : ('a, error) result =
  let rec go : type a. a t -> b -> (a, error) result = fun repr input ->
    match repr with
    | Unit   -> R.null input
    | Bool   -> R.bool input
    | Int    -> R.int input
    | Int32  -> R.int32 input
    | Int64  -> R.int64 input
    | Float  -> R.float input
    | Char   -> R.char input
    | String -> R.string input
    | Option r ->
      (match R.null input with
       | Ok () -> Ok None
       | Error _ -> let+ v = go r input in Some v)
    | Tuple2 (r1, r2) ->
      (match R.list input with
       | Ok [ j1; j2 ] ->
         let* a = Error.with_path "0" (go r1 j1) in
         let+ b = Error.with_path "1" (go r2 j2) in
         (a, b)
       | Ok _ -> Error (Error.make [] "tuple2: wrong arity")
       | Error _ as e -> e)
    | Tuple3 (r1, r2, r3) ->
      (match R.list input with
       | Ok [ j1; j2; j3 ] ->
         let* a = Error.with_path "0" (go r1 j1) in
         let* b = Error.with_path "1" (go r2 j2) in
         let+ c = Error.with_path "2" (go r3 j3) in
         (a, b, c)
       | Ok _ -> Error (Error.make [] "tuple3: wrong arity")
       | Error _ as e -> e)
    | Tuple4 (r1, r2, r3, r4) ->
      (match R.list input with
       | Ok [ j1; j2; j3; j4 ] ->
         let* a = Error.with_path "0" (go r1 j1) in
         let* b = Error.with_path "1" (go r2 j2) in
         let* c = Error.with_path "2" (go r3 j3) in
         let+ d = Error.with_path "3" (go r4 j4) in
         (a, b, c, d)
       | Ok _ -> Error (Error.make [] "tuple4: wrong arity")
       | Error _ as e -> e)
    | Tuple5 (r1, r2, r3, r4, r5) ->
      (match R.list input with
       | Ok [ j1; j2; j3; j4; j5 ] ->
         let* a = Error.with_path "0" (go r1 j1) in
         let* b = Error.with_path "1" (go r2 j2) in
         let* c = Error.with_path "2" (go r3 j3) in
         let* d = Error.with_path "3" (go r4 j4) in
         let+ e = Error.with_path "4" (go r5 j5) in
         (a, b, c, d, e)
       | Ok _ -> Error (Error.make [] "tuple5: wrong arity")
       | Error _ as e -> e)
    | Tuple6 (r1, r2, r3, r4, r5, r6) ->
      (match R.list input with
       | Ok [ j1; j2; j3; j4; j5; j6 ] ->
         let* a = Error.with_path "0" (go r1 j1) in
         let* b = Error.with_path "1" (go r2 j2) in
         let* c = Error.with_path "2" (go r3 j3) in
         let* d = Error.with_path "3" (go r4 j4) in
         let* e = Error.with_path "4" (go r5 j5) in
         let+ f = Error.with_path "5" (go r6 j6) in
         (a, b, c, d, e, f)
       | Ok _ -> Error (Error.make [] "tuple6: wrong arity")
       | Error _ as e -> e)
    | Collection { builder; element_codec; _ } ->
      (match R.list input with
       | Error _ as e -> e
       | Ok items ->
         let sink, finalize = builder () in
         let exception B of error in
         (try
            List.iteri (fun i j ->
              match Error.with_path (string_of_int i) (go element_codec j) with
              | Ok v -> sink v
              | Error e -> raise (B e)
            ) items;
            Ok (finalize ())
          with B e -> Error e))
    | Record fields ->
      (match R.record input with
       | Error _ as e -> e
       | Ok assoc -> decode_fields fields assoc)
    | Variant { vname; cases } ->
      decode_variant vname cases input
    | Map { repr; forward; _ } ->
      let+ v = go repr input in forward v
    | Lazy l ->
      go (Lazy.force l) input
  and decode_fields : type f r.
    (f, r) fields -> (string * b) list -> (f, error) result =
    fun fields assoc ->
    match fields with
    | F0 { constructor; _ } -> Ok constructor
    | Field { rest; name; repr; default; _ } ->
      let* f = decode_fields rest assoc in
      (match List.assoc_opt name assoc with
       | None ->
         (match default with
          | Some d -> Ok (f d)
          | None ->
            Error (Error.make [ name ] "missing required field"
                     ~expected:(Printf.sprintf "field '%s'" name)))
       | Some j ->
         match R.null j, default with
         | Ok (), Some d -> Ok (f d)
         | _ ->
           let+ v = Error.with_path name (go repr j) in f v)
  and decode_variant : type v. string -> v case list -> b -> (v, error) result =
    fun vname cases input ->
    match R.string input with
    | Ok name ->
      let rec find = function
        | [] ->
          Error (Error.make [ vname ]
                   (Printf.sprintf "unknown variant case '%s'" name))
        | Case0 { name = n; value; _ } :: _ when String.equal n name ->
          Ok value
        | _ :: rest -> find rest
      in
      find cases
    | Error _ ->
      (match R.list input with
       | Ok [ name_j; arg_j ] ->
         let* name = Error.with_path "variant tag" (R.string name_j) in
         let rec find = function
           | [] ->
             Error (Error.make [ vname ]
                      (Printf.sprintf "unknown variant case '%s'" name))
           | Case { name = n; repr; construct; _ } :: _
             when String.equal n name ->
             let+ v = Error.with_path name (go repr arg_j) in
             construct v
           | _ :: rest -> find rest
         in
         find cases
       | _ ->
         Error (Error.make [ vname ]
                  "variant must be a string (constant) or a [name, arg] list"))
  in
  go codec input

let decode (type b)
    (codec : 'a t)
    ~(reader : (module Reader.S with type t = b))
    (input : b)
  : ('a, error) result =
  decode_via reader codec input
