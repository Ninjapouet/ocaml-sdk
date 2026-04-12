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

  let with_path prefix result =
    Result.map_error (fun e -> { e with path = prefix :: e.path }) result

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
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Tuple2 : 'a t * 'b t -> ('a * 'b) t
  | Tuple3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tuple4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Tuple5 : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) t
  | Tuple6 : 'a t * 'b t * 'c t * 'd t * 'e t * 'f t -> ('a * 'b * 'c * 'd * 'e * 'f) t
  | Record : ('r, 'r) fields -> 'r t
  | Variant : 'v variant_desc -> 'v t
  | Map : ('a, 'b) map_desc -> 'b t
  | Lazy : 'a t lazy_t -> 'a t

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

(* -- Convenience constructors --------------------------------------------- *)

let unit = Unit
let bool = Bool
let int = Int
let int32 = Int32
let int64 = Int64
let float = Float
let char = Char
let string = String

let option r = Option r
let list r = List r
let array r = Array r

let tuple2 a b = Tuple2 (a, b)
let tuple3 a b c = Tuple3 (a, b, c)
let tuple4 a b c d = Tuple4 (a, b, c, d)
let tuple5 a b c d e = Tuple5 (a, b, c, d, e)
let tuple6 a b c d e f = Tuple6 (a, b, c, d, e, f)

let map forward backward repr = Map { repr; forward; backward }
let lazy_ l = Lazy l

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

(* -- Driver interface ----------------------------------------------------- *)

type 'a codec = 'a t

module type DRIVER = sig
  type t

  val encode : 'a codec -> 'a -> (t, error) result
  val decode : 'a codec -> t -> ('a, error) result
end

module Make (D : DRIVER) = struct
  include D

  let encode_exn repr v =
    match D.encode repr v with
    | Ok x -> x
    | Error e -> raise (Error.Codec_error e)

  let decode_exn repr raw =
    match D.decode repr raw with
    | Ok x -> x
    | Error e -> raise (Error.Codec_error e)
end
