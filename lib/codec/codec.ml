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

(* -- Drivers ------------------------------------------------------------- *)
(*
   Three-layer architecture, declared bottom-up:

   - [Writer.S] / [Reader.S]    : format-primitive interfaces (the ~14
                                  emit-token / ~10 extract-typed functions
                                  a format must provide).
   - [Encoder.S] / [Decoder.S]  : driver halves (one [encode] / [decode]
                                  function each), produced from a Writer /
                                  Reader by the [Make] functor.
   - [Driver.S]                 : combined encode + decode, built from a
                                  Writer and a Reader together.

   The bottom layer (Writer/Reader) describes what a *format* offers;
   the middle layer (Encoder/Decoder) is what a driver *consumes*. The
   functors bridge the two.

   The layers themselves are format-agnostic and not specifically
   dedicated to streaming — streaming is just one efficient use case
   (when the [Writer] targets a sink). A [Writer] could equally well
   build a materialized value.

   For first-class use, value-level [encoder] / [decoder] / [driver]
   records are exposed alongside, and the [Bridge] sub-module converts
   first-class modules into these records. *)

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

    val null    : input -> (unit,   error) result
    val bool    : input -> (bool,   error) result
    val int     : input -> (int,    error) result
    val int32   : input -> (int32,  error) result
    val int64   : input -> (int64,  error) result
    val float   : input -> (float,  error) result
    val char    : input -> (char,   error) result
    val string  : input -> (string, error) result
    val array   : input -> (input list, error) result
    val object_ : input -> ((string * input) list, error) result
  end
end

module Encoder = struct
  module type S = sig
    type out
    val encode : 'a codec -> 'a -> out -> (unit, error) result
  end

  module Make (W : Writer.S) : S with type out = W.out = struct
  type out = W.out

  (* Internal bail-out: errors are rare during encoding (the only
     source is an unmatched variant case), so we use an exception to
     keep the happy path allocation-free. *)
  exception Bail of error

  let bail e = raise (Bail e)

  let with_path prefix f =
    try f () with Bail e -> raise (Bail (Error.prepend_path prefix e))

  let rec encode_ : type a. a t -> a -> W.out -> unit =
    fun repr value out ->
    match repr with
    | Unit   -> W.null out
    | Bool   -> W.bool out value
    | Int    -> W.int out value
    | Int32  -> W.int32 out value
    | Int64  -> W.int64 out value
    | Float  -> W.float out value
    | Char   -> W.char out value
    | String -> W.string out value
    | Option r ->
      (match value with
       | None -> W.null out
       | Some v -> encode_ r v out)
    | Tuple2 (r1, r2) ->
      let a, b = value in
      W.begin_array out;
      encode_ r1 a out; W.array_sep out;
      encode_ r2 b out;
      W.end_array out
    | Tuple3 (r1, r2, r3) ->
      let a, b, c = value in
      W.begin_array out;
      encode_ r1 a out; W.array_sep out;
      encode_ r2 b out; W.array_sep out;
      encode_ r3 c out;
      W.end_array out
    | Tuple4 (r1, r2, r3, r4) ->
      let a, b, c, d = value in
      W.begin_array out;
      encode_ r1 a out; W.array_sep out;
      encode_ r2 b out; W.array_sep out;
      encode_ r3 c out; W.array_sep out;
      encode_ r4 d out;
      W.end_array out
    | Tuple5 (r1, r2, r3, r4, r5) ->
      let a, b, c, d, e = value in
      W.begin_array out;
      encode_ r1 a out; W.array_sep out;
      encode_ r2 b out; W.array_sep out;
      encode_ r3 c out; W.array_sep out;
      encode_ r4 d out; W.array_sep out;
      encode_ r5 e out;
      W.end_array out
    | Tuple6 (r1, r2, r3, r4, r5, r6) ->
      let a, b, c, d, e, f = value in
      W.begin_array out;
      encode_ r1 a out; W.array_sep out;
      encode_ r2 b out; W.array_sep out;
      encode_ r3 c out; W.array_sep out;
      encode_ r4 d out; W.array_sep out;
      encode_ r5 e out; W.array_sep out;
      encode_ r6 f out;
      W.end_array out
    | Collection { iter; element_codec; _ } ->
      (* No per-element [with_path] wrapping: encoding errors during
         the body are limited to variant-case lookup failures, which
         carry their own path. Avoiding the closure allocation per
         element pays off significantly on large collections. *)
      W.begin_array out;
      let first = ref true in
      iter (fun elem ->
        if !first then first := false else W.array_sep out;
        encode_ element_codec elem out
      ) value;
      W.end_array out
    | Record fields ->
      W.begin_object out;
      let (_ : bool) = encode_fields fields value out true in
      W.end_object out
    | Variant { vname; cases } ->
      encode_variant vname cases value out
    | Map { repr; backward; _ } ->
      encode_ repr (backward value) out
    | Lazy l ->
      encode_ (Lazy.force l) value out

  (* Fields stored outermost-last: recursing into [rest] before writing
     this field yields declaration order. The bool threads through to
     emit separators correctly. *)
  and encode_fields : type f r.
    (f, r) fields -> r -> W.out -> bool -> bool =
    fun fields value out first ->
    match fields with
    | F0 _ -> first
    | Field { rest; name; repr; get; _ } ->
      let first = encode_fields rest value out first in
      W.key out ~first name;
      (* Same rationale as [Collection]: no per-field [with_path]
         wrapping. *)
      encode_ repr (get value) out;
      false

  and encode_variant : type v. string -> v case list -> v -> W.out -> unit =
    fun vname cases value out ->
    let rec loop = function
      | [] ->
        bail (Error.make [ vname ] "no matching case for variant value")
      | Case { name; repr; destruct; _ } :: rest ->
        (match destruct value with
         | None -> loop rest
         | Some payload ->
           W.variant_payload out name (fun out ->
             with_path name (fun () -> encode_ repr payload out)))
      | Case0 { name; match_; _ } :: rest ->
        if match_ value then W.variant_constant out name
        else loop rest
    in
    loop cases

  let encode : 'a t -> 'a -> W.out -> (unit, error) result =
    fun repr value out ->
    try Ok (encode_ repr value out) with Bail e -> Error e
  end
end

module Decoder = struct
  module type S = sig
    type input
    val decode : 'a codec -> input -> ('a, error) result
  end

  module Make (R : Reader.S) : S with type input = R.input = struct
  type input = R.input

  let ( let* ) = Result.bind
  let ( let+ ) r f = Result.map f r

  let rec decode : type a. a t -> R.input -> (a, error) result =
    fun repr input ->
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
      (* Try null first; on failure, recurse with the inner codec. *)
      (match R.null input with
       | Ok () -> Ok None
       | Error _ -> let+ v = decode r input in Some v)
    | Tuple2 (r1, r2) ->
      (match R.array input with
       | Ok [ j1; j2 ] ->
         let* a = Error.with_path "0" (decode r1 j1) in
         let+ b = Error.with_path "1" (decode r2 j2) in
         (a, b)
       | Ok _ -> Error (Error.make [] "tuple2: wrong arity")
       | Error _ as e -> e)
    | Tuple3 (r1, r2, r3) ->
      (match R.array input with
       | Ok [ j1; j2; j3 ] ->
         let* a = Error.with_path "0" (decode r1 j1) in
         let* b = Error.with_path "1" (decode r2 j2) in
         let+ c = Error.with_path "2" (decode r3 j3) in
         (a, b, c)
       | Ok _ -> Error (Error.make [] "tuple3: wrong arity")
       | Error _ as e -> e)
    | Tuple4 (r1, r2, r3, r4) ->
      (match R.array input with
       | Ok [ j1; j2; j3; j4 ] ->
         let* a = Error.with_path "0" (decode r1 j1) in
         let* b = Error.with_path "1" (decode r2 j2) in
         let* c = Error.with_path "2" (decode r3 j3) in
         let+ d = Error.with_path "3" (decode r4 j4) in
         (a, b, c, d)
       | Ok _ -> Error (Error.make [] "tuple4: wrong arity")
       | Error _ as e -> e)
    | Tuple5 (r1, r2, r3, r4, r5) ->
      (match R.array input with
       | Ok [ j1; j2; j3; j4; j5 ] ->
         let* a = Error.with_path "0" (decode r1 j1) in
         let* b = Error.with_path "1" (decode r2 j2) in
         let* c = Error.with_path "2" (decode r3 j3) in
         let* d = Error.with_path "3" (decode r4 j4) in
         let+ e = Error.with_path "4" (decode r5 j5) in
         (a, b, c, d, e)
       | Ok _ -> Error (Error.make [] "tuple5: wrong arity")
       | Error _ as e -> e)
    | Tuple6 (r1, r2, r3, r4, r5, r6) ->
      (match R.array input with
       | Ok [ j1; j2; j3; j4; j5; j6 ] ->
         let* a = Error.with_path "0" (decode r1 j1) in
         let* b = Error.with_path "1" (decode r2 j2) in
         let* c = Error.with_path "2" (decode r3 j3) in
         let* d = Error.with_path "3" (decode r4 j4) in
         let* e = Error.with_path "4" (decode r5 j5) in
         let+ f = Error.with_path "5" (decode r6 j6) in
         (a, b, c, d, e, f)
       | Ok _ -> Error (Error.make [] "tuple6: wrong arity")
       | Error _ as e -> e)
    | Collection { builder; element_codec; _ } ->
      (match R.array input with
       | Error _ as e -> e
       | Ok items ->
         let sink, finalize = builder () in
         let exception Bail of error in
         (try
            List.iteri (fun i j ->
              match Error.with_path (string_of_int i)
                      (decode element_codec j) with
              | Ok v -> sink v
              | Error e -> raise (Bail e)
            ) items;
            Ok (finalize ())
          with Bail e -> Error e))
    | Record fields ->
      (match R.object_ input with
       | Error _ as e -> e
       | Ok assoc -> decode_fields fields assoc)
    | Variant { vname; cases } ->
      decode_variant vname cases input
    | Map { repr; forward; _ } ->
      let+ v = decode repr input in forward v
    | Lazy l ->
      decode (Lazy.force l) input

  and decode_fields : type f r.
    (f, r) fields -> (string * R.input) list -> (f, error) result =
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
         (* If the field is present and null, fall back to default
            (consistent with the Yojson driver's behavior). *)
         match R.null j, default with
         | Ok (), Some d -> Ok (f d)
         | _ ->
           let+ v = Error.with_path name (decode repr j) in f v)

  and decode_variant : type v.
    string -> v case list -> R.input -> (v, error) result =
    fun vname cases input ->
    (* Try string first (constant constructor); on failure try
       array [name, arg] (constructor with payload). *)
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
      (match R.array input with
       | Ok [ name_j; arg_j ] ->
         let* name =
           Error.with_path "variant tag" (R.string name_j) in
         let rec find = function
           | [] ->
             Error (Error.make [ vname ]
                      (Printf.sprintf "unknown variant case '%s'" name))
           | Case { name = n; repr; construct; _ } :: _
             when String.equal n name ->
             let+ v = Error.with_path name (decode repr arg_j) in
             construct v
           | _ :: rest -> find rest
         in
         find cases
       | _ ->
         Error (Error.make [ vname ]
                  "variant must be a string (constant) or a [name, arg] array"))
  end
end

module Driver = struct
  module type S = sig
    type out
    type input
    include Encoder.S with type out := out
    include Decoder.S with type input := input
  end

  module Make (W : Writer.S) (R : Reader.S) : S
    with type out = W.out
     and type input = R.input
  = struct
    type out = W.out
    type input = R.input
    module E = Encoder.Make (W)
    module D = Decoder.Make (R)
    let encode = E.encode
    let decode = D.decode
  end
end

(* -- First-class records, built from modules via [Bridge] ----------------- *)

type 'out encoder = {
  encode : 'a. 'a codec -> 'a -> 'out -> (unit, error) result;
}

type 'input decoder = {
  decode : 'a. 'a codec -> 'input -> ('a, error) result;
}

type ('out, 'input) driver = {
  encoder : 'out encoder;
  decoder : 'input decoder;
}

module Bridge = struct
  let encoder (type o) (module W : Writer.S with type out = o) : o encoder =
    let module E = Encoder.Make (W) in
    { encode = E.encode }

  let decoder (type i) (module R : Reader.S with type input = i) : i decoder =
    let module D = Decoder.Make (R) in
    { decode = D.decode }

  let driver
    (type o) (type i)
    (module W : Writer.S with type out = o)
    (module R : Reader.S with type input = i)
    : (o, i) driver
    =
    { encoder = encoder (module W);
      decoder = decoder (module R) }
end

(* -- Toplevel encode/decode: apply a driver's appropriate half ----------- *)

let encode (driver : ('o, _) driver) codec v (out : 'o) =
  driver.encoder.encode codec v out

let decode (driver : (_, 'i) driver) codec (input : 'i) =
  driver.decoder.decode codec input
