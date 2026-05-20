open Result.Syntax

let with_path = Codec.Error.with_path

let type_error expected json =
  let got = match json with
    | `Null -> "null" | `Bool _ -> "bool" | `Int _ -> "int"
    | `Intlit _ -> "intlit" | `Float _ -> "float"
    | `String _ -> "string" | `List _ -> "list" | `Assoc _ -> "object"
  in
  Error (Codec.Error.make [] "type mismatch" ~expected ~got)

(* -- Encode: traverse OCaml value guided by GADT, produce JSON ------------ *)

let rec encode : type a. a Codec.t -> a -> (Yojson.Safe.t, Codec.error) result =
  fun repr value ->
  match repr with
  | Codec.Unit -> Ok `Null
  | Codec.Bool -> Ok (`Bool value)
  | Codec.Int -> Ok (`Int value)
  | Codec.Int32 -> Ok (`Int (Int32.to_int value))
  | Codec.Int64 -> Ok (`Intlit (Int64.to_string value))
  | Codec.Float -> Ok (`Float value)
  | Codec.Char -> Ok (`String (String.make 1 value))
  | Codec.String -> Ok (`String value)
  | Codec.Option repr ->
    (match value with
     | None -> Ok `Null
     | Some v -> encode repr v)
  | Codec.Collection { iter; element_codec; _ } ->
    encode_collection iter element_codec value
  | Codec.Tuple2 (r1, r2) ->
    let a, b = value in
    let* ja = encode r1 a in
    let+ jb = encode r2 b in
    `List [ ja; jb ]
  | Codec.Tuple3 (r1, r2, r3) ->
    let a, b, c = value in
    let* ja = encode r1 a in
    let* jb = encode r2 b in
    let+ jc = encode r3 c in
    `List [ ja; jb; jc ]
  | Codec.Tuple4 (r1, r2, r3, r4) ->
    let a, b, c, d = value in
    let* ja = encode r1 a in
    let* jb = encode r2 b in
    let* jc = encode r3 c in
    let+ jd = encode r4 d in
    `List [ ja; jb; jc; jd ]
  | Codec.Tuple5 (r1, r2, r3, r4, r5) ->
    let a, b, c, d, e = value in
    let* ja = encode r1 a in
    let* jb = encode r2 b in
    let* jc = encode r3 c in
    let* jd = encode r4 d in
    let+ je = encode r5 e in
    `List [ ja; jb; jc; jd; je ]
  | Codec.Tuple6 (r1, r2, r3, r4, r5, r6) ->
    let a, b, c, d, e, f = value in
    let* ja = encode r1 a in
    let* jb = encode r2 b in
    let* jc = encode r3 c in
    let* jd = encode r4 d in
    let* je = encode r5 e in
    let+ jf = encode r6 f in
    `List [ ja; jb; jc; jd; je; jf ]
  | Codec.Record fields ->
    encode_record fields value []
  | Codec.Variant { cases; vname } ->
    encode_variant vname cases value
  | Codec.Map { repr; backward; _ } ->
    encode repr (backward value)
  | Codec.Lazy l ->
    encode (Lazy.force l) value

and encode_collection :
  type c e. ((e -> unit) -> c -> unit) -> e Codec.t -> c ->
  (Yojson.Safe.t, Codec.error) result =
  fun iter element_codec container ->
  let exception Bail of Codec.error in
  try
    let items = ref [] in
    let i = ref 0 in
    iter (fun elem ->
      match with_path (string_of_int !i) (encode element_codec elem) with
      | Ok j -> items := j :: !items; incr i
      | Error err -> raise (Bail err)
    ) container;
    Ok (`List (List.rev !items))
  with Bail e -> Error e

and encode_record : type f r. (f, r) Codec.fields -> r -> (string * Yojson.Safe.t) list -> (Yojson.Safe.t, Codec.error) result =
  fun fields value acc ->
  match fields with
  | Codec.F0 _ -> Ok (`Assoc acc)
  | Codec.Field { rest; name; repr; get; _ } ->
    let* j = with_path name (encode repr (get value)) in
    encode_record rest value ((name, j) :: acc)

and encode_variant : type v. string -> v Codec.case list -> v -> (Yojson.Safe.t, Codec.error) result =
  fun vname cases value ->
  let rec try_cases = function
    | [] ->
      Error (Codec.Error.make [ vname ] "no matching case for variant value")
    | Codec.Case { name; repr; destruct; _ } :: rest ->
      (match destruct value with
       | None -> try_cases rest
       | Some a ->
         let+ j = with_path name (encode repr a) in
         `List [ `String name; j ])
    | Codec.Case0 { name; match_; _ } :: rest ->
      if match_ value then Ok (`String name)
      else try_cases rest
  in
  try_cases cases

(* -- Decode: traverse JSON guided by GADT, produce OCaml value ------------ *)

let rec decode : type a. a Codec.t -> Yojson.Safe.t -> (a, Codec.error) result =
  fun repr json ->
  match repr with
  | Codec.Unit ->
    (match json with `Null -> Ok () | j -> type_error "null" j)
  | Codec.Bool ->
    (match json with `Bool b -> Ok b | j -> type_error "bool" j)
  | Codec.Int ->
    (match json with `Int i -> Ok i | j -> type_error "int" j)
  | Codec.Int32 ->
    (match json with `Int i -> Ok (Int32.of_int i) | j -> type_error "int" j)
  | Codec.Int64 ->
    (match json with
     | `Intlit s ->
       (match Int64.of_string_opt s with
        | Some i -> Ok i
        | None -> Error (Codec.Error.make [] "invalid int64" ~got:s))
     | `Int i -> Ok (Int64.of_int i)
     | j -> type_error "int64" j)
  | Codec.Float ->
    (match json with
     | `Float f -> Ok f
     | `Int i -> Ok (Float.of_int i)
     | j -> type_error "float" j)
  | Codec.Char ->
    (match json with
     | `String s when String.length s = 1 -> Ok s.[0]
     | `String _ ->
       Error (Codec.Error.make [] "expected single character"
                ~got:"multi-char string")
     | j -> type_error "string" j)
  | Codec.String ->
    (match json with `String s -> Ok s | j -> type_error "string" j)
  | Codec.Option repr ->
    (match json with
     | `Null -> Ok None
     | j -> let+ v = decode repr j in Some v)
  | Codec.Collection { builder; element_codec; _ } ->
    (match json with
     | `List l -> decode_collection builder element_codec l
     | j -> type_error "list" j)
  | Codec.Tuple2 (r1, r2) ->
    (match json with
     | `List [ j1; j2 ] ->
       let* a = with_path "0" (decode r1 j1) in
       let+ b = with_path "1" (decode r2 j2) in
       (a, b)
     | j -> type_error "tuple2" j)
  | Codec.Tuple3 (r1, r2, r3) ->
    (match json with
     | `List [ j1; j2; j3 ] ->
       let* a = with_path "0" (decode r1 j1) in
       let* b = with_path "1" (decode r2 j2) in
       let+ c = with_path "2" (decode r3 j3) in
       (a, b, c)
     | j -> type_error "tuple3" j)
  | Codec.Tuple4 (r1, r2, r3, r4) ->
    (match json with
     | `List [ j1; j2; j3; j4 ] ->
       let* a = with_path "0" (decode r1 j1) in
       let* b = with_path "1" (decode r2 j2) in
       let* c = with_path "2" (decode r3 j3) in
       let+ d = with_path "3" (decode r4 j4) in
       (a, b, c, d)
     | j -> type_error "tuple4" j)
  | Codec.Tuple5 (r1, r2, r3, r4, r5) ->
    (match json with
     | `List [ j1; j2; j3; j4; j5 ] ->
       let* a = with_path "0" (decode r1 j1) in
       let* b = with_path "1" (decode r2 j2) in
       let* c = with_path "2" (decode r3 j3) in
       let* d = with_path "3" (decode r4 j4) in
       let+ e = with_path "4" (decode r5 j5) in
       (a, b, c, d, e)
     | j -> type_error "tuple5" j)
  | Codec.Tuple6 (r1, r2, r3, r4, r5, r6) ->
    (match json with
     | `List [ j1; j2; j3; j4; j5; j6 ] ->
       let* a = with_path "0" (decode r1 j1) in
       let* b = with_path "1" (decode r2 j2) in
       let* c = with_path "2" (decode r3 j3) in
       let* d = with_path "3" (decode r4 j4) in
       let* e = with_path "4" (decode r5 j5) in
       let+ f = with_path "5" (decode r6 j6) in
       (a, b, c, d, e, f)
     | j -> type_error "tuple6" j)
  | Codec.Record fields ->
    (match json with
     | `Assoc assoc -> decode_record fields assoc
     | j -> type_error "object" j)
  | Codec.Variant { cases; vname } ->
    decode_variant vname cases json
  | Codec.Map { repr; forward; _ } ->
    let+ v = decode repr json in forward v
  | Codec.Lazy l ->
    decode (Lazy.force l) json

and decode_collection :
  type c e. (unit -> (e -> unit) * (unit -> c)) -> e Codec.t ->
  Yojson.Safe.t list -> (c, Codec.error) result =
  fun builder element_codec items ->
  let sink, finalize = builder () in
  let exception Bail of Codec.error in
  try
    List.iteri (fun i j ->
      match with_path (string_of_int i) (decode element_codec j) with
      | Ok v -> sink v
      | Error err -> raise (Bail err)
    ) items;
    Ok (finalize ())
  with Bail e -> Error e

and decode_record : type f r. (f, r) Codec.fields -> (string * Yojson.Safe.t) list -> (f, Codec.error) result =
  fun fields assoc ->
  match fields with
  | Codec.F0 { constructor; _ } -> Ok constructor
  | Codec.Field { rest; name; repr; default; _ } ->
    let* f = decode_record rest assoc in
    (match List.assoc_opt name assoc, default with
     | (None | Some `Null), Some d -> Ok (f d)
     | (None | Some `Null), None ->
       Error (Codec.Error.make [ name ] "missing required field"
                ~expected:(Printf.sprintf "field '%s'" name))
     | Some j, _ ->
       let+ v = with_path name (decode repr j) in f v)

and decode_variant : type v. string -> v Codec.case list -> Yojson.Safe.t -> (v, Codec.error) result =
  fun vname cases json ->
  match json with
  | `String name ->
    let rec find = function
      | [] ->
        Error (Codec.Error.make [ vname ]
                 (Printf.sprintf "unknown variant case '%s'" name))
      | Codec.Case0 { name = n; value; _ } :: _ when String.equal n name ->
        Ok value
      | _ :: rest -> find rest
    in
    find cases
  | `List [ `String name; arg ] ->
    let rec find = function
      | [] ->
        Error (Codec.Error.make [ vname ]
                 (Printf.sprintf "unknown variant case '%s'" name))
      | Codec.Case { name = n; repr; construct; _ } :: _
        when String.equal n name ->
        let+ v = with_path name (decode repr arg) in construct v
      | _ :: rest -> find rest
    in
    find cases
  | j -> type_error "variant (string or [name, arg])" j

(* -- Driver --------------------------------------------------------------- *)

include Codec.Make (struct
  type t = Yojson.Safe.t
  let encode = encode
  let decode = decode
end)

let encode_string repr v =
  let+ j = encode repr v in Yojson.Safe.to_string j

let decode_string repr s =
  match Yojson.Safe.from_string s with
  | json -> decode repr json
  | exception Yojson.Json_error msg ->
    Error (Codec.Error.make [] msg ~expected:"valid JSON")

(* ========================================================================== *)
(*                                  Tests                                     *)
(* ========================================================================== *)

let%test_module "Primitives" = (module struct
  let roundtrip repr v =
    match encode repr v with
    | Error _ -> false
    | Ok json ->
      (match decode repr json with
       | Ok v' -> v = v'
       | Error _ -> false)

  let%test "unit" = roundtrip Codec.unit ()
  let%test "bool true" = roundtrip Codec.bool true
  let%test "bool false" = roundtrip Codec.bool false
  let%test "int" = roundtrip Codec.int 42
  let%test "int negative" = roundtrip Codec.int (-7)
  let%test "int32" = roundtrip Codec.int32 42l
  let%test "int64" = roundtrip Codec.int64 9999999999L
  let%test "float" = roundtrip Codec.float 3.14
  let%test "char" = roundtrip Codec.char 'x'
  let%test "string" = roundtrip Codec.string "hello"
  let%test "string empty" = roundtrip Codec.string ""

  let%expect_test "int encodes directly" =
    let json = encode_exn Codec.int 42 in
    print_endline (Yojson.Safe.to_string json);
    [%expect {| 42 |}]

  let%expect_test "type mismatch error" =
    (match decode Codec.int (`String "hello") with
     | Error e -> print_string (Codec.Error.to_string e)
     | Ok _ -> print_string "unexpected Ok");
    [%expect {| type mismatch (expected int, got string) |}]
end)

let%test_module "Combinators" = (module struct
  let roundtrip repr v =
    match encode repr v with
    | Error _ -> false
    | Ok json ->
      (match decode repr json with
       | Ok v' -> v = v'
       | Error _ -> false)

  let%test "option Some" = roundtrip Codec.(option int) (Some 42)
  let%test "option None" = roundtrip Codec.(option int) None
  let%test "list" = roundtrip Codec.(list int) [ 1; 2; 3 ]
  let%test "list empty" = roundtrip Codec.(list int) []
  let%test "array" = roundtrip Codec.(array string) [| "a"; "b" |]
  let%test "tuple2" = roundtrip Codec.(tuple2 int string) (1, "hello")
  let%test "tuple3" = roundtrip Codec.(tuple3 int string bool) (1, "hi", true)

  let%expect_test "list error with path" =
    (match decode Codec.(list int) (`List [ `Int 1; `String "bad" ]) with
     | Error e -> print_string (Codec.Error.to_string e)
     | Ok _ -> print_string "unexpected Ok");
    [%expect {| 1: type mismatch (expected int, got string) |}]
end)

let%test_module "Records" = (module struct
  type user = { name : string; age : int }

  let user_repr =
    Codec.record "user" (fun name age -> { name; age })
    |> Codec.field "name" Codec.string (fun u -> u.name)
    |> Codec.field "age" Codec.int (fun u -> u.age)
    |> Codec.seal

  let roundtrip repr v =
    match encode repr v with
    | Error _ -> false
    | Ok json ->
      (match decode repr json with
       | Ok v' -> v = v'
       | Error _ -> false)

  let%test "user roundtrip" =
    roundtrip user_repr { name = "Alice"; age = 30 }

  let%expect_test "user to JSON" =
    let json = encode_exn user_repr { name = "Alice"; age = 30 } in
    print_endline (Yojson.Safe.pretty_to_string json);
    [%expect {| { "name": "Alice", "age": 30 } |}]

  let%expect_test "user from JSON string" =
    (match decode_string user_repr {|{"name":"Bob","age":25}|} with
     | Ok u -> Printf.printf "%s, %d" u.name u.age
     | Error e -> print_string (Codec.Error.to_string e));
    [%expect {| Bob, 25 |}]

  let%expect_test "missing field error" =
    (match decode user_repr (`Assoc [ "name", `String "Alice" ]) with
     | Error e -> print_string (Codec.Error.to_string e)
     | Ok _ -> print_string "unexpected Ok");
    [%expect {| age: missing required field (expected field 'age') |}]

  let%expect_test "wrong type error" =
    (match decode user_repr (`Int 42) with
     | Error e -> print_string (Codec.Error.to_string e)
     | Ok _ -> print_string "unexpected Ok");
    [%expect {| type mismatch (expected object, got int) |}]

  type config = { host : string; port : int; debug : bool }

  let config_repr =
    Codec.record "config" (fun host port debug -> { host; port; debug })
    |> Codec.field "host" Codec.string (fun c -> c.host)
    |> Codec.field ~default:8080 "port" Codec.int (fun c -> c.port)
    |> Codec.field ~default:false "debug" Codec.bool (fun c -> c.debug)
    |> Codec.seal

  let%test "config all present" =
    roundtrip config_repr { host = "localhost"; port = 3000; debug = true }

  let%test "config defaults" =
    match decode config_repr (`Assoc [ "host", `String "localhost" ]) with
    | Ok c -> c.host = "localhost" && c.port = 8080 && c.debug = false
    | Error _ -> false

  type with_opt = { label : string; value : int option }

  let with_opt_repr =
    Codec.record "with_opt" (fun label value -> { label; value })
    |> Codec.field "label" Codec.string (fun w -> w.label)
    |> Codec.field_opt "value" Codec.int (fun w -> w.value)
    |> Codec.seal

  let%test "field_opt Some" = roundtrip with_opt_repr { label = "a"; value = Some 1 }
  let%test "field_opt None" = roundtrip with_opt_repr { label = "b"; value = None }

  let%test "field_opt missing" =
    decode with_opt_repr (`Assoc [ "label", `String "c" ])
    = Ok { label = "c"; value = None }
end)

let%test_module "Variants" = (module struct
  type color = Red | Green | Blue

  let color_repr =
    Codec.variant "color" [
      Codec.case0 "Red" Red;
      Codec.case0 "Green" Green;
      Codec.case0 "Blue" Blue;
    ]

  let roundtrip repr v =
    match encode repr v with
    | Error _ -> false
    | Ok json ->
      (match decode repr json with
       | Ok v' -> v = v'
       | Error _ -> false)

  let%test "constant variant Red" = roundtrip color_repr Red
  let%test "constant variant Green" = roundtrip color_repr Green
  let%test "constant variant Blue" = roundtrip color_repr Blue

  let%expect_test "constant variant JSON" =
    print_endline (Yojson.Safe.to_string (encode_exn color_repr Green));
    [%expect {| "Green" |}]

  type shape =
    | Circle of float
    | Rect of float * float
    | Point

  let shape_repr =
    Codec.variant "shape" [
      Codec.case "Circle" Codec.float
        (function Circle r -> Some r | _ -> None)
        (fun r -> Circle r);
      Codec.case "Rect" Codec.(tuple2 float float)
        (function Rect (w, h) -> Some (w, h) | _ -> None)
        (fun (w, h) -> Rect (w, h));
      Codec.case0 "Point" Point;
    ]

  let%test "Circle roundtrip" = roundtrip shape_repr (Circle 3.0)
  let%test "Rect roundtrip" = roundtrip shape_repr (Rect (4.0, 5.0))
  let%test "Point roundtrip" = roundtrip shape_repr Point

  let%expect_test "Circle JSON" =
    print_endline (Yojson.Safe.to_string (encode_exn shape_repr (Circle 3.0)));
    [%expect {| ["Circle",3.0] |}]

  let%expect_test "unknown case error" =
    (match decode color_repr (`String "Purple") with
     | Error e -> print_string (Codec.Error.to_string e)
     | Ok _ -> print_string "unexpected");
    [%expect {| color: unknown variant case 'Purple' |}]

  (* Recursive type *)
  type tree = Leaf | Node of tree * int * tree

  let rec tree_repr =
    lazy (Codec.variant "tree" [
      Codec.case0 "Leaf" Leaf;
      Codec.case "Node" Codec.(tuple3 (lazy_ tree_repr) int (lazy_ tree_repr))
        (function Node (l, v, r) -> Some (l, v, r) | _ -> None)
        (fun (l, v, r) -> Node (l, v, r));
    ])

  let tree_repr = Codec.lazy_ tree_repr

  let%test "recursive tree roundtrip" =
    let t = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)) in
    roundtrip tree_repr t
end)

let%test_module "Map" = (module struct
  type email = Email of string

  let email_repr =
    Codec.map
      (fun s -> Email s)
      (fun (Email s) -> s)
      Codec.string

  let%test "map roundtrip" =
    let v = Email "test@example.com" in
    match encode email_repr v with
    | Error _ -> false
    | Ok json ->
      (match decode email_repr json with
       | Ok (Email s) -> s = "test@example.com"
       | Error _ -> false)

  let%expect_test "map encodes as underlying" =
    print_endline (Yojson.Safe.to_string (encode_exn email_repr (Email "a@b.c")));
    [%expect {| "a@b.c" |}]
end)

let%test_module "Nested" = (module struct
  let roundtrip repr v =
    match encode repr v with
    | Error _ -> false
    | Ok json ->
      (match decode repr json with
       | Ok v' -> v = v'
       | Error _ -> false)

  type user = { name : string; age : int }

  let user_repr =
    Codec.record "user" (fun name age -> { name; age })
    |> Codec.field "name" Codec.string (fun u -> u.name)
    |> Codec.field "age" Codec.int (fun u -> u.age)
    |> Codec.seal

  let%test "list of records" =
    roundtrip (Codec.list user_repr)
      [ { name = "Alice"; age = 30 }; { name = "Bob"; age = 25 } ]

  let%test "nested option Some Some" =
    roundtrip Codec.(option (option int)) (Some (Some 42))

  let%test "nested option None" =
    roundtrip Codec.(option (option int)) None

  (* Note: Some None and None both encode to `Null in JSON.
     This is an inherent limitation of the format — not a bug. *)
  let%test "nested option Some None decodes as None" =
    encode_exn Codec.(option (option int)) (Some None) = `Null
end)
