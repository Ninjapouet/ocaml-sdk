let roundtrip repr v =
  let open Result.Syntax in
  let* json = Codec_yojson.encode repr v in
  Codec_yojson.decode repr json

(* -- Type alias ----------------------------------------------------------- *)

type name = string [@@deriving codec]

let () =
  assert (roundtrip name_codec "hello" = Ok "hello");
  print_endline "PASS: type alias"

(* -- Simple record -------------------------------------------------------- *)

type user = { name : string; age : int } [@@deriving codec]

let () =
  let u = { name = "Alice"; age = 30 } in
  assert (roundtrip user_codec u = Ok u);
  print_endline "PASS: simple record"

(* -- Record with defaults ------------------------------------------------- *)

type config = {
  host : string;
  port : int; [@default 8080]
  debug : bool; [@default false]
} [@@deriving codec]

let () =
  let c = { host = "localhost"; port = 3000; debug = true } in
  assert (roundtrip config_codec c = Ok c);
  (* Test defaults on missing fields *)
  let json = `Assoc [ "host", `String "localhost" ] in
  (match Codec_yojson.decode config_codec json with
   | Ok c -> assert (c.host = "localhost" && c.port = 8080 && c.debug = false)
   | Error e -> failwith (Codec.Error.to_string e));
  print_endline "PASS: record with defaults"

(* -- Record with optional field ------------------------------------------- *)

type with_opt = {
  label : string;
  value : int option;
} [@@deriving codec]

let () =
  assert (roundtrip with_opt_codec { label = "a"; value = Some 42 }
          = Ok { label = "a"; value = Some 42 });
  assert (roundtrip with_opt_codec { label = "b"; value = None }
          = Ok { label = "b"; value = None });
  (* Missing field decodes as None *)
  let json = `Assoc [ "label", `String "c" ] in
  assert (Codec_yojson.decode with_opt_codec json
          = Ok { label = "c"; value = None });
  print_endline "PASS: record with optional field"

(* -- Simple variant ------------------------------------------------------- *)

type color = Red | Green | Blue [@@deriving codec]

let () =
  assert (roundtrip color_codec Red = Ok Red);
  assert (roundtrip color_codec Green = Ok Green);
  assert (roundtrip color_codec Blue = Ok Blue);
  print_endline "PASS: simple variant"

(* -- Variant with payloads ------------------------------------------------ *)

type shape =
  | Circle of float
  | Rect of float * float
  | Point
[@@deriving codec]

let () =
  assert (roundtrip shape_codec (Circle 3.0) = Ok (Circle 3.0));
  assert (roundtrip shape_codec (Rect (4.0, 5.0)) = Ok (Rect (4.0, 5.0)));
  assert (roundtrip shape_codec Point = Ok Point);
  print_endline "PASS: variant with payloads"

(* -- Parametric type ------------------------------------------------------ *)

type 'a box = { value : 'a; tag : string } [@@deriving codec]

let () =
  let b = { value = 42; tag = "int" } in
  assert (roundtrip (box_codec Codec.int) b = Ok b);
  let bs = { value = "hello"; tag = "str" } in
  assert (roundtrip (box_codec Codec.string) bs = Ok bs);
  print_endline "PASS: parametric type"

(* -- Recursive type ------------------------------------------------------- *)

type tree = Leaf | Node of tree * int * tree [@@deriving codec]

let () =
  let t = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)) in
  assert (roundtrip tree_codec t = Ok t);
  print_endline "PASS: recursive type"

(* -- Mutually recursive types --------------------------------------------- *)

type expr =
  | Lit of int
  | Add of expr * expr
  | Bind of binding * expr
and binding = { bname : string; bvalue : expr } [@@deriving codec]

let () =
  let e = Bind ({ bname = "x"; bvalue = Lit 1 }, Add (Lit 2, Lit 3)) in
  assert (roundtrip expr_codec e = Ok e);
  let b = { bname = "y"; bvalue = Add (Lit 1, Lit 2) } in
  assert (roundtrip binding_codec b = Ok b);
  print_endline "PASS: mutually recursive types"

(* -- Attribute [@name] ---------------------------------------------------- *)

type renamed = {
  field_a : string; [@name "a"]
  field_b : int; [@name "b"]
} [@@deriving codec]

let () =
  let r = { field_a = "hello"; field_b = 42 } in
  let json = Codec_yojson.encode_exn renamed_codec r in
  (* Verify field names in JSON *)
  (match json with
   | `Assoc fields ->
     assert (List.mem_assoc "a" fields);
     assert (List.mem_assoc "b" fields);
     assert (not (List.mem_assoc "field_a" fields))
   | _ -> failwith "expected object");
  assert (roundtrip renamed_codec r = Ok r);
  print_endline "PASS: attribute [@name]"

(* -- Done ----------------------------------------------------------------- *)

let () = print_endline "All PPX tests passed."
