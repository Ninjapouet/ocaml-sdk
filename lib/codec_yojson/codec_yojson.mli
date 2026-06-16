(** Codec_yojson — Yojson instances for the [Codec] and [Marshal]
    libraries.

    {1 Pure value conversion (Codec)}

    Convert OCaml values to/from {!Yojson.Safe.t} via the
    {!Codec} value-conversion API. *)

module Writer : Codec.Writer.S with type t = Yojson.Safe.t
module Reader : Codec.Reader.S with type t = Yojson.Safe.t

val to_yojson : 'a Codec.codec -> 'a -> (Yojson.Safe.t, Codec.error) result
val of_yojson : 'a Codec.codec -> Yojson.Safe.t -> ('a, Codec.error) result

val to_yojson_exn : 'a Codec.codec -> 'a -> Yojson.Safe.t
val of_yojson_exn : 'a Codec.codec -> Yojson.Safe.t -> 'a

(** {1 Sink-based streaming (Marshal)}

    Stream JSON syntax to a [Buffer.t] or [out_channel] without
    materializing an intermediate {!Yojson.Safe.t}. *)

module Buffer_writer  : Marshal.Writer.S with type out = Buffer.t
module Channel_writer : Marshal.Writer.S with type out = out_channel
module Yojson_reader  : Marshal.Reader.S with type input = Yojson.Safe.t

(** {1 Convenience: string ↔ value}

    Encoding goes through {!Buffer_writer} (no AST allocated).
    Decoding uses [Yojson.Safe.from_string] then {!Yojson_reader}. *)

val to_string : 'a Codec.codec -> 'a -> (string, Codec.error) result
val of_string : 'a Codec.codec -> string -> ('a, Codec.error) result

val to_string_exn : 'a Codec.codec -> 'a -> string
val of_string_exn : 'a Codec.codec -> string -> 'a
