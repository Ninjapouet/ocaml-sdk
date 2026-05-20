(** Streaming, zero-AST driver test.

    Demonstrates that the [Codec.t] GADT can drive serialization without
    building any intermediate representation: the driver walks the OCaml
    value, guided by the type description, and writes tokens directly
    into a [Buffer.t]. Allocations are limited to the output buffer and
    small primitive conversions (e.g. [string_of_int]); no
    [`Assoc]/[`List] AST nodes, no field-name lists, no intermediate
    tuples. *)

(* -- Streaming JSON driver ------------------------------------------------ *)

module Json_stream : sig
  val encode_to_buffer : Buffer.t -> 'a Codec.t -> 'a -> unit
  val encode : 'a Codec.t -> 'a -> string
end = struct

  let add_escaped_string buf s =
    Buffer.add_char buf '"';
    for i = 0 to String.length s - 1 do
      match String.unsafe_get s i with
      | '"'    -> Buffer.add_string buf "\\\""
      | '\\'   -> Buffer.add_string buf "\\\\"
      | '\n'   -> Buffer.add_string buf "\\n"
      | '\r'   -> Buffer.add_string buf "\\r"
      | '\t'   -> Buffer.add_string buf "\\t"
      | '\b'   -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | c when Char.code c < 0x20 ->
        Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c      -> Buffer.add_char buf c
    done;
    Buffer.add_char buf '"'

  (* [Float.to_string 1.0] returns ["1."], which is not valid JSON;
     append a trailing zero so the output parses back as a float. *)
  let add_float buf f =
    let s = Float.to_string f in
    Buffer.add_string buf s;
    let n = String.length s in
    if n > 0 && s.[n - 1] = '.' then Buffer.add_char buf '0'

  let rec encode_to_buffer : type a. Buffer.t -> a Codec.t -> a -> unit =
    fun buf repr value ->
    match repr with
    | Codec.Unit   -> Buffer.add_string buf "null"
    | Codec.Bool   -> Buffer.add_string buf (if value then "true" else "false")
    | Codec.Int    -> Buffer.add_string buf (string_of_int value)
    | Codec.Int32  -> Buffer.add_string buf (Int32.to_string value)
    | Codec.Int64  -> Buffer.add_string buf (Int64.to_string value)
    | Codec.Float  -> add_float buf value
    | Codec.Char   ->
      Buffer.add_char buf '"';
      Buffer.add_char buf value;
      Buffer.add_char buf '"'
    | Codec.String -> add_escaped_string buf value
    | Codec.Option r ->
      (match value with
       | None   -> Buffer.add_string buf "null"
       | Some v -> encode_to_buffer buf r v)
    | Codec.Collection { iter; element_codec; _ } ->
      Buffer.add_char buf '[';
      let first = ref true in
      iter (fun x ->
        if !first then first := false else Buffer.add_char buf ',';
        encode_to_buffer buf element_codec x) value;
      Buffer.add_char buf ']'
    | Codec.Tuple2 (r1, r2) ->
      let a, b = value in
      Buffer.add_char buf '[';
      encode_to_buffer buf r1 a; Buffer.add_char buf ',';
      encode_to_buffer buf r2 b;
      Buffer.add_char buf ']'
    | Codec.Tuple3 (r1, r2, r3) ->
      let a, b, c = value in
      Buffer.add_char buf '[';
      encode_to_buffer buf r1 a; Buffer.add_char buf ',';
      encode_to_buffer buf r2 b; Buffer.add_char buf ',';
      encode_to_buffer buf r3 c;
      Buffer.add_char buf ']'
    | Codec.Tuple4 (r1, r2, r3, r4) ->
      let a, b, c, d = value in
      Buffer.add_char buf '[';
      encode_to_buffer buf r1 a; Buffer.add_char buf ',';
      encode_to_buffer buf r2 b; Buffer.add_char buf ',';
      encode_to_buffer buf r3 c; Buffer.add_char buf ',';
      encode_to_buffer buf r4 d;
      Buffer.add_char buf ']'
    | Codec.Tuple5 (r1, r2, r3, r4, r5) ->
      let a, b, c, d, e = value in
      Buffer.add_char buf '[';
      encode_to_buffer buf r1 a; Buffer.add_char buf ',';
      encode_to_buffer buf r2 b; Buffer.add_char buf ',';
      encode_to_buffer buf r3 c; Buffer.add_char buf ',';
      encode_to_buffer buf r4 d; Buffer.add_char buf ',';
      encode_to_buffer buf r5 e;
      Buffer.add_char buf ']'
    | Codec.Tuple6 (r1, r2, r3, r4, r5, r6) ->
      let a, b, c, d, e, f = value in
      Buffer.add_char buf '[';
      encode_to_buffer buf r1 a; Buffer.add_char buf ',';
      encode_to_buffer buf r2 b; Buffer.add_char buf ',';
      encode_to_buffer buf r3 c; Buffer.add_char buf ',';
      encode_to_buffer buf r4 d; Buffer.add_char buf ',';
      encode_to_buffer buf r5 e; Buffer.add_char buf ',';
      encode_to_buffer buf r6 f;
      Buffer.add_char buf ']'
    | Codec.Record fields ->
      Buffer.add_char buf '{';
      let _ : bool = encode_fields buf fields value true in
      Buffer.add_char buf '}'
    | Codec.Variant { cases; _ } ->
      encode_variant buf cases value
    | Codec.Map { repr; backward; _ } ->
      encode_to_buffer buf repr (backward value)
    | Codec.Lazy l ->
      encode_to_buffer buf (Lazy.force l) value

  (* Fields are stored outermost-last: recursing into [rest] before
     writing this field gives the original declaration order. The
     boolean threads through to know whether to emit a separator. *)
  and encode_fields : type f r.
    Buffer.t -> (f, r) Codec.fields -> r -> bool -> bool =
    fun buf fields value first ->
    match fields with
    | Codec.F0 _ -> first
    | Codec.Field { rest; name; repr; get; _ } ->
      let first = encode_fields buf rest value first in
      if not first then Buffer.add_char buf ',';
      add_escaped_string buf name;
      Buffer.add_char buf ':';
      encode_to_buffer buf repr (get value);
      false

  and encode_variant : type v. Buffer.t -> v Codec.case list -> v -> unit =
    fun buf cases value ->
    let rec loop = function
      | [] -> failwith "Json_stream: no matching variant case"
      | Codec.Case { name; repr; destruct; _ } :: rest ->
        (match destruct value with
         | None   -> loop rest
         | Some a ->
           Buffer.add_char buf '[';
           add_escaped_string buf name;
           Buffer.add_char buf ',';
           encode_to_buffer buf repr a;
           Buffer.add_char buf ']')
      | Codec.Case0 { name; match_; _ } :: rest ->
        if match_ value then add_escaped_string buf name
        else loop rest
    in
    loop cases

  let encode repr v =
    let buf = Buffer.create 256 in
    encode_to_buffer buf repr v;
    Buffer.contents buf
end

(* -- Helpers ------------------------------------------------------------- *)

(* The streaming driver and Yojson may format floats differently and
   emit object keys in a different order; comparing parsed output is the
   format-independent correctness check. *)
let parse_then_decode repr s =
  let json = Yojson.Safe.from_string s in
  Codec_yojson.decode_exn repr json

(* -- Correctness tests --------------------------------------------------- *)

type user = { name : string; age : int; email : string option }
[@@deriving codec]

let () =
  let u = { name = "Alice"; age = 30; email = Some "alice@example.com" } in
  let s = Json_stream.encode user_codec u in
  assert (parse_then_decode user_codec s = u);
  print_endline "PASS: streaming record round-trips via Yojson decode"

let () =
  let repr = Codec.(list (tuple3 int string (option bool))) in
  let v = [ 1, "a", Some true; 2, "bee", None; 3, "see", Some false ] in
  let s = Json_stream.encode repr v in
  assert (parse_then_decode repr s = v);
  print_endline "PASS: streaming tuples, options, lists"

type shape = Point | Circle of float | Box of float * float
[@@deriving codec]

let () =
  List.iter (fun s ->
    let txt = Json_stream.encode shape_codec s in
    assert (parse_then_decode shape_codec txt = s)
  ) [ Point; Circle 1.5; Box (3.0, 4.0) ];
  print_endline "PASS: streaming variants (constant + payload)"

let () =
  let s = "line1\nline2\t\"quoted\"\\backslash\b\r\012" in
  let txt = Json_stream.encode Codec.string s in
  assert (parse_then_decode Codec.string txt = s);
  print_endline "PASS: streaming escapes JSON special characters"

(* The lower-level [encode_to_buffer] lets the caller reuse an existing
   buffer across many values — useful when streaming a large feed of
   small records to a socket or file. *)
let () =
  let buf = Buffer.create 1024 in
  let repr = Codec.list user_codec in
  let chunks = [
    [ { name = "Alice"; age = 30; email = None } ];
    [ { name = "Bob";   age = 22; email = Some "b@x" };
      { name = "Carol"; age = 40; email = None } ];
  ] in
  List.iter (fun us ->
    Buffer.clear buf;
    Json_stream.encode_to_buffer buf repr us;
    assert (parse_then_decode repr (Buffer.contents buf) = us)
  ) chunks;
  print_endline "PASS: encode_to_buffer reuses a caller-owned buffer"

(* -- Allocation comparison on a large dataset --------------------------- *)

let () =
  let n = 50_000 in
  let users = List.init n (fun i ->
    { name  = Printf.sprintf "user_%d" i;
      age   = i mod 100;
      email = if i mod 3 = 0 then None
              else Some (Printf.sprintf "u%d@example.com" i) })
  in
  let users_repr = Codec.list user_codec in

  (* Warm up to fault in pages and avoid first-run noise. *)
  let _ = Json_stream.encode users_repr users in
  let _ = Codec_yojson.encode_string users_repr users in

  let measure label f =
    Gc.compact ();
    let before = Gc.allocated_bytes () in
    let t0 = Sys.time () in
    let result = f () in
    let t1 = Sys.time () in
    let after = Gc.allocated_bytes () in
    let mib = (after -. before) /. 1024. /. 1024. in
    Printf.printf "  %-18s : %.3fs, %7.2f MiB allocated, output %d bytes\n"
      label (t1 -. t0) mib (String.length result);
    (result, after -. before)
  in

  Printf.printf "Allocation comparison on %d records:\n" n;
  let s_stream, alloc_stream =
    measure "streaming driver" (fun () ->
      Json_stream.encode users_repr users)
  in
  let s_yojson, alloc_yojson =
    measure "Yojson driver" (fun () ->
      match Codec_yojson.encode_string users_repr users with
      | Ok s    -> s
      | Error e -> failwith (Codec.Error.to_string e))
  in

  (* Both outputs must denote the same value. *)
  assert (parse_then_decode users_repr s_stream = users);
  assert (parse_then_decode users_repr s_yojson = users);

  (* The streaming driver should allocate strictly less: it skips the
     entire intermediate JSON AST (one [`Assoc] cell per record, one
     [`String]/[`Int] node per field, one [`List] for the whole array). *)
  assert (alloc_stream < alloc_yojson);
  Printf.printf "  ratio (stream / yojson) : %.2f\n"
    (alloc_stream /. alloc_yojson);
  print_endline "PASS: streaming driver allocates less than Yojson driver"
