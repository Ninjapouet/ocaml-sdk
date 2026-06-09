(** Codec_yojson — Yojson-backed JSON drivers for the Codec library.

    Two encoding paths coexist:

    {ul
    {- {!module:Raw}: encode produces a [Yojson.Safe.t], decode consumes
       one. Use it when you want to inspect or compose the AST.}
    {- {!module:Buffer_writer} / {!module:Channel_writer}: streaming
       writers that emit JSON syntax directly to a target ([Buffer.t],
       [out_channel], …) — no intermediate AST. Plug them into
       {!Codec.Encoder.Make} (or use the pre-built record helpers
       below).}}

    For symmetric decoding, the parser side reuses Yojson via
    {!module:Yojson_reader} (an instance of {!Codec.Reader.S}).

    {2 Quick start}

    {[
      type user = { name : string; age : int }
      [@@deriving codec]

      (* Easiest path: encode/decode through a string. *)
      let s = Codec_yojson.encode_string_exn user_codec u in
      let u = Codec_yojson.decode_string_exn user_codec s

      (* Or with the pre-built record encoders/decoder/driver. *)
      let buf = Buffer.create 256 in
      let () = (Codec_yojson.buffer_encoder.encode user_codec u buf
                |> Result.get_ok)
      in
      let json_string = Buffer.contents buf
    ]}
*)

(** {1 Raw: encode/decode via [Yojson.Safe.t]} *)

(** AST-based encode/decode. Useful when the caller wants to manipulate
    the JSON value (composition, inspection). For raw serialization to
    a string or channel, prefer the streaming helpers below. *)
module Raw : sig
  val encode     : 'a Codec.codec -> 'a -> (Yojson.Safe.t, Codec.error) result
  val decode     : 'a Codec.codec -> Yojson.Safe.t -> ('a, Codec.error) result
  val encode_exn : 'a Codec.codec -> 'a -> Yojson.Safe.t
  val decode_exn : 'a Codec.codec -> Yojson.Safe.t -> 'a
end

(** {1 Streaming JSON writers (module form)}

    A {!type:Codec.Writer.S} for each common sink type. Combine with
    {!Codec.Encoder.Make}, or use the {{!records}pre-built records}
    below. *)

module Buffer_writer  : Codec.Writer.S with type out = Buffer.t
module Channel_writer : Codec.Writer.S with type out = out_channel

(** {1 Reader over Yojson AST (module form)} *)

module Yojson_reader : Codec.Reader.S with type input = Yojson.Safe.t

(** {1:records Pre-built records (value form)}

    First-class records produced from the modules above via
    {!Codec.Bridge}. Use these directly without instantiating any
    functor. *)

val buffer_encoder  : Buffer.t Codec.encoder
val channel_encoder : out_channel Codec.encoder
val yojson_decoder  : Yojson.Safe.t Codec.decoder

(** Pre-cut driver: streaming encode to [Buffer.t], decode from
    [Yojson.Safe.t]. *)
val driver : (Buffer.t, Yojson.Safe.t) Codec.driver

(** {1 Top-level convenience: string ↔ value}

    Encoding goes through {!val:buffer_encoder} (streaming, no AST
    allocated). Decoding parses with [Yojson.Safe.from_string] then
    runs {!val:yojson_decoder}. *)

val encode_string : 'a Codec.codec -> 'a -> (string, Codec.error) result
val decode_string : 'a Codec.codec -> string -> ('a, Codec.error) result
val encode_string_exn : 'a Codec.codec -> 'a -> string
val decode_string_exn : 'a Codec.codec -> string -> 'a
