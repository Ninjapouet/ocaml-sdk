(** Tests for the [\[@@deriving codec\]] PPX deriver.

    Each test {b prints} the JSON output and asserts on it via
    [[%expect]], then checks that decoding the same JSON yields the
    original value. Roundtrip alone would not catch a buggy codec that
    happens to be its own inverse (e.g. a [Marshal]-based one); pairing
    encoded-output assertions with the roundtrip nails down both
    correctness and stability of the wire format. *)

(** Encode [v], print the JSON, then check that decoding it returns [v]. *)
let show_roundtrip codec v =
  let json = Codec_yojson.to_yojson_exn codec v in
  print_endline (Yojson.Safe.to_string json);
  let decoded = Codec_yojson.of_yojson_exn codec json in
  assert (decoded = v)

(** Roundtrip without printing — for codecs whose encoded form is not
    deterministic (e.g. Hashtbl iteration order). [~check] inspects the
    decoded value with a custom predicate instead of structural equality. *)
let check_roundtrip_with codec v ~check =
  let json = Codec_yojson.to_yojson_exn codec v in
  let decoded = Codec_yojson.of_yojson_exn codec json in
  assert (check decoded)

(* -- Type alias ----------------------------------------------------------- *)

type name = string [@@deriving codec]

let%expect_test "type alias" =
  show_roundtrip name_codec "hello";
  [%expect {| "hello" |}]

(* -- Simple record -------------------------------------------------------- *)

type user = { name : string; age : int } [@@deriving codec]

let%expect_test "simple record" =
  show_roundtrip user_codec { name = "Alice"; age = 30 };
  [%expect {| {"name":"Alice","age":30} |}]

(* -- Record with defaults ------------------------------------------------- *)

type config = {
  host : string;
  port : int; [@default 8080]
  debug : bool; [@default false]
} [@@deriving codec]

let%expect_test "record with defaults: all fields present" =
  show_roundtrip config_codec { host = "localhost"; port = 3000; debug = true };
  [%expect {| {"host":"localhost","port":3000,"debug":true} |}]

let%expect_test "record with defaults: missing fields fall back" =
  let json = `Assoc [ "host", `String "localhost" ] in
  let c = Codec_yojson.of_yojson_exn config_codec json in
  Printf.printf "host=%s port=%d debug=%b" c.host c.port c.debug;
  [%expect {| host=localhost port=8080 debug=false |}]

(* -- Record with optional field ------------------------------------------- *)

type with_opt = {
  label : string;
  value : int option;
} [@@deriving codec]

let%expect_test "field_opt: Some" =
  show_roundtrip with_opt_codec { label = "a"; value = Some 42 };
  [%expect {| {"label":"a","value":42} |}]

let%expect_test "field_opt: None encodes as null" =
  show_roundtrip with_opt_codec { label = "b"; value = None };
  [%expect {| {"label":"b","value":null} |}]

let%expect_test "field_opt: missing field decodes as None" =
  let json = `Assoc [ "label", `String "c" ] in
  let v = Codec_yojson.of_yojson_exn with_opt_codec json in
  assert (v = { label = "c"; value = None });
  [%expect {| |}]

(* -- Simple variant ------------------------------------------------------- *)

type color = Red | Green | Blue [@@deriving codec]

let%expect_test "variant: Red" =
  show_roundtrip color_codec Red;
  [%expect {| "Red" |}]

let%expect_test "variant: Green" =
  show_roundtrip color_codec Green;
  [%expect {| "Green" |}]

let%expect_test "variant: Blue" =
  show_roundtrip color_codec Blue;
  [%expect {| "Blue" |}]

(* -- Variant with payloads ------------------------------------------------ *)

type shape =
  | Circle of float
  | Rect of float * float
  | Point
[@@deriving codec]

let%expect_test "variant payload: Circle" =
  show_roundtrip shape_codec (Circle 3.0);
  [%expect {| ["Circle",3.0] |}]

let%expect_test "variant payload: Rect" =
  show_roundtrip shape_codec (Rect (4.0, 5.0));
  [%expect {| ["Rect",[4.0,5.0]] |}]

let%expect_test "variant payload: Point (constant)" =
  show_roundtrip shape_codec Point;
  [%expect {| "Point" |}]

(* -- Parametric type ------------------------------------------------------ *)

type 'a box = { value : 'a; tag : string } [@@deriving codec]

let%expect_test "parametric: int box" =
  show_roundtrip (box_codec Codec.int) { value = 42; tag = "int" };
  [%expect {| {"value":42,"tag":"int"} |}]

let%expect_test "parametric: string box" =
  show_roundtrip (box_codec Codec.string) { value = "hello"; tag = "str" };
  [%expect {| {"value":"hello","tag":"str"} |}]

(* -- Recursive type ------------------------------------------------------- *)

type tree = Leaf | Node of tree * int * tree [@@deriving codec]

let%expect_test "recursive type" =
  let t = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)) in
  show_roundtrip tree_codec t;
  [%expect {| ["Node",[["Node",["Leaf",1,"Leaf"]],2,["Node",["Leaf",3,"Leaf"]]]] |}]

(* -- Mutually recursive types --------------------------------------------- *)

type expr =
  | Lit of int
  | Add of expr * expr
  | Bind of binding * expr
and binding = { bname : string; bvalue : expr } [@@deriving codec]

let%expect_test "mutually recursive: expr" =
  let e = Bind ({ bname = "x"; bvalue = Lit 1 }, Add (Lit 2, Lit 3)) in
  show_roundtrip expr_codec e;
  [%expect {| ["Bind",[{"bname":"x","bvalue":["Lit",1]},["Add",[["Lit",2],["Lit",3]]]]] |}]

let%expect_test "mutually recursive: binding" =
  let b = { bname = "y"; bvalue = Add (Lit 1, Lit 2) } in
  show_roundtrip binding_codec b;
  [%expect {| {"bname":"y","bvalue":["Add",[["Lit",1],["Lit",2]]]} |}]

(* -- Attribute [@name] ---------------------------------------------------- *)

type renamed = {
  field_a : string; [@name "a"]
  field_b : int; [@name "b"]
} [@@deriving codec]

let%expect_test "attribute [@name] renames fields in the JSON" =
  show_roundtrip renamed_codec { field_a = "hello"; field_b = 42 };
  [%expect {| {"a":"hello","b":42} |}]

(* -- Stdlib container types ---------------------------------------------- *)

(* Queue iteration order is FIFO (deterministic). *)
type job_queue = string Queue.t [@@deriving codec]

let%expect_test "Queue.t via ppx" =
  let q = Queue.create () in
  Queue.add "first" q;
  Queue.add "second" q;
  show_roundtrip job_queue_codec q;
  [%expect {| ["first","second"] |}]

(* Seq.iter forces in order — deterministic. [Seq.t] is a function, so
   polymorphic [=] doesn't compare elements; we materialize via
   [List.of_seq] to check. *)
type lazy_stream = int Seq.t [@@deriving codec]

let%expect_test "Seq.t via ppx" =
  let s = List.to_seq [ 1; 2; 3 ] in
  let json = Codec_yojson.to_yojson_exn lazy_stream_codec s in
  print_endline (Yojson.Safe.to_string json);
  let s' = Codec_yojson.of_yojson_exn lazy_stream_codec json in
  assert (List.of_seq s' = [ 1; 2; 3 ]);
  [%expect {| [1,2,3] |}]

(* Hashtbl.iter order is unspecified — the encoded JSON is
   non-deterministic, so we only check the roundtrip values. *)
type counters = (string, int) Hashtbl.t [@@deriving codec]

let%test_unit "Hashtbl.t via ppx (roundtrip only, non-deterministic order)" =
  let h = Hashtbl.create 4 in
  Hashtbl.add h "a" 1;
  Hashtbl.add h "b" 2;
  check_roundtrip_with counters_codec h ~check:(fun h' ->
    Hashtbl.find h' "a" = 1 && Hashtbl.find h' "b" = 2)

(* Map.Make.iter walks keys in sorted order — deterministic. The
   underlying balanced tree is built by [add]-ing keys in the same
   order on encode and decode, so structural equality holds.
   Map.Make's output signature is already a superset of [Codec.Map.S],
   so the functor application is direct — no glue struct needed. *)
module Smap = struct
  include Map.Make (String)
  module C = Codec.Map.Make (Map.Make (String))
  let t_codec value_codec = C.codec Codec.string value_codec
end

type tally = int Smap.t [@@deriving codec]

let%expect_test "Map.Make via Codec.Map.Make + ppx" =
  let m = Smap.empty |> Smap.add "x" 1 |> Smap.add "y" 2 in
  show_roundtrip tally_codec m;
  [%expect {| [["x",1],["y",2]] |}]
