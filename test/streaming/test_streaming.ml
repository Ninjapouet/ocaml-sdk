(** Tests for the [Marshal] library via the JSON instances from
    [codec-yojson] ([Buffer_writer], [Channel_writer], [Yojson_reader]).

    Demonstrates that [Marshal.serialize] writes JSON directly into a
    [Buffer.t] with no intermediate AST, and compares its allocation
    footprint against the value-conversion path
    ([Codec_yojson.to_yojson + Yojson.Safe.to_string]). *)

(** Encode via Marshal into a fresh Buffer, return the resulting string. *)
let encode_to_string codec value =
  let buf = Buffer.create 256 in
  match
    Marshal.serialize codec value
      ~writer:(module Codec_yojson.Buffer_writer)
      buf
  with
  | Ok () -> Buffer.contents buf
  | Error e -> raise (Codec.Error.Codec_error e)

(** Decode via Codec.Convert after parsing with Yojson — used in
    correctness tests to verify that streaming output round-trips. *)
let parse_then_decode codec s =
  let json = Yojson.Safe.from_string s in
  Codec_yojson.of_yojson_exn codec json

(* -- Correctness tests --------------------------------------------------- *)

type user = { name : string; age : int; email : string option }
[@@deriving codec]

let%expect_test "streaming record" =
  let u = { name = "Alice"; age = 30; email = Some "alice@example.com" } in
  let s = encode_to_string user_codec u in
  print_endline s;
  assert (parse_then_decode user_codec s = u);
  [%expect {| {"name":"Alice","age":30,"email":"alice@example.com"} |}]

let%expect_test "streaming tuples, options, lists" =
  let codec = Codec.(list (tuple3 int string (option bool))) in
  let v = [ 1, "a", Some true; 2, "bee", None; 3, "see", Some false ] in
  let s = encode_to_string codec v in
  print_endline s;
  assert (parse_then_decode codec s = v);
  [%expect {| [[1,"a",true],[2,"bee",null],[3,"see",false]] |}]

type shape = Point | Circle of float | Box of float * float
[@@deriving codec]

let%expect_test "streaming variant: Point (constant)" =
  let s = encode_to_string shape_codec Point in
  print_endline s;
  assert (parse_then_decode shape_codec s = Point);
  [%expect {| "Point" |}]

let%expect_test "streaming variant: Circle (payload)" =
  let s = encode_to_string shape_codec (Circle 1.5) in
  print_endline s;
  assert (parse_then_decode shape_codec s = Circle 1.5);
  [%expect {| ["Circle",1.5] |}]

let%expect_test "streaming variant: Box (tuple payload)" =
  let s = encode_to_string shape_codec (Box (3.0, 4.0)) in
  print_endline s;
  assert (parse_then_decode shape_codec s = Box (3.0, 4.0));
  [%expect {| ["Box",[3.0,4.0]] |}]

let%expect_test "streaming escapes JSON special characters" =
  let raw = "line1\nline2\t\"quoted\"\\backslash\b\r\012" in
  let s = encode_to_string Codec.string raw in
  print_endline s;
  assert (parse_then_decode Codec.string s = raw);
  [%expect {| "line1\nline2\t\"quoted\"\\backslash\b\r\f" |}]

(* [Marshal.serialize] accepts a caller-owned sink — useful when
   feeding a large sequence of small records into a single buffer (HTTP
   response body, log shipper, …) without reallocation between items. *)
let%expect_test "Marshal.serialize accepts a caller-owned buffer" =
  let buf = Buffer.create 1024 in
  let codec = Codec.list user_codec in
  let chunks = [
    [ { name = "Alice"; age = 30; email = None } ];
    [ { name = "Bob";   age = 22; email = Some "b@x" };
      { name = "Carol"; age = 40; email = None } ];
  ] in
  List.iter (fun us ->
    Buffer.clear buf;
    match
      Marshal.serialize codec us
        ~writer:(module Codec_yojson.Buffer_writer)
        buf
    with
    | Ok () ->
      let s = Buffer.contents buf in
      print_endline s;
      assert (parse_then_decode codec s = us)
    | Error e -> raise (Codec.Error.Codec_error e)
  ) chunks;
  [%expect {|
    [{"name":"Alice","age":30,"email":null}]
    [{"name":"Bob","age":22,"email":"b@x"},{"name":"Carol","age":40,"email":null}]
    |}]

(* -- Allocation comparison: Marshal streaming vs Codec to_yojson --------- *)

let%test_unit "Marshal streaming allocates less than Codec.to_yojson + dump" =
  let n = 50_000 in
  let users = List.init n (fun i ->
    { name  = Printf.sprintf "user_%d" i;
      age   = i mod 100;
      email = if i mod 3 = 0 then None
              else Some (Printf.sprintf "u%d@example.com" i) })
  in
  let users_codec = Codec.list user_codec in

  (* Warm up to fault in pages. *)
  let _ = encode_to_string users_codec users in
  let _ =
    Yojson.Safe.to_string (Codec_yojson.to_yojson_exn users_codec users)
  in

  let measure label f =
    Gc.compact ();
    let before = Gc.allocated_bytes () in
    let t0 = Sys.time () in
    let result = f () in
    let t1 = Sys.time () in
    let after = Gc.allocated_bytes () in
    let mib = (after -. before) /. 1024. /. 1024. in
    Printf.printf "  %-22s : %.3fs, %7.2f MiB allocated, output %d bytes\n"
      label (t1 -. t0) mib (String.length result);
    (result, after -. before)
  in

  Printf.printf "Allocation comparison on %d records:\n" n;
  let s_stream, alloc_stream =
    measure "Marshal streaming" (fun () -> encode_to_string users_codec users)
  in
  let s_ast, alloc_ast =
    measure "Codec to_yojson + dump" (fun () ->
      Yojson.Safe.to_string (Codec_yojson.to_yojson_exn users_codec users))
  in

  assert (parse_then_decode users_codec s_stream = users);
  assert (parse_then_decode users_codec s_ast = users);
  assert (alloc_stream < alloc_ast);
  Printf.printf "  ratio (stream / ast) : %.2f\n"
    (alloc_stream /. alloc_ast)
