(** Sink/source serialization driven by a {!Codec.t}.

    Marshal is the {b side-effect} side of the codec ecosystem:
    encoding writes tokens into a caller-provided sink (a [Buffer.t],
    [out_channel], …); decoding reads from a caller-provided input
    (typically a parsed AST). No intermediate value is materialized by
    the framework itself.

    The companion {!Codec} library covers the {b pure} value-conversion
    side (OCaml value <-> [Yojson.Safe.t] / S-expression / …). Same
    {!Codec.t} GADT, different semantics. *)

(** Token-emission interface. A [Writer.S] writes JSON-like syntax to
    a sink of type {!type:out} via side effects. *)
module Writer : sig
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

    (** Called before every array item except the first. *)
    val array_sep   : out -> unit

    val end_array   : out -> unit

    val begin_object : out -> unit

    (** [key out ~first name] writes the key for a field. [~first] is
        [true] for the first field of an object (no leading separator). *)
    val key : out -> first:bool -> string -> unit

    val end_object : out -> unit

    (** Encoding of a variant constant constructor (no payload). *)
    val variant_constant : out -> string -> unit

    (** [variant_payload out name write_payload] encodes a variant with
        a payload. The writer is in control of the surrounding syntax;
        it invokes [write_payload] to delegate the payload encoding. *)
    val variant_payload : out -> string -> (out -> unit) -> unit
  end
end

(** Input-extraction interface. A [Reader.S] pulls primitive values
    out of a parsed input of type {!type:input}. *)
module Reader : sig
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

(** {1 serialize / deserialize}

    {[
      (* Streaming JSON to a Buffer: *)
      let buf = Buffer.create 256 in
      Marshal.serialize user_codec my_user
        ~writer:(module Codec_yojson.Buffer_writer)
        buf
      |> Result.get_ok

      (* Decode from a parsed AST: *)
      let u =
        Marshal.deserialize user_codec
          ~reader:(module Codec_yojson.Yojson_reader)
          (Yojson.Safe.from_string raw)
        |> Result.get_ok
    ]}

    Curry-style: partial-apply on the codec/value to pre-build a
    serializer, then pick the writer/sink later.

    {[
      let s = Marshal.serialize user_codec my_user in
      s ~writer:(module Codec_yojson.Buffer_writer)  buf;
      s ~writer:(module Codec_yojson.Channel_writer) stdout
    ]} *)

val serialize :
  'a Codec.t -> 'a ->
  writer:(module Writer.S with type out = 's) ->
  's ->
  (unit, Codec.error) result

val deserialize :
  'a Codec.t ->
  reader:(module Reader.S with type input = 'i) ->
  'i ->
  ('a, Codec.error) result
