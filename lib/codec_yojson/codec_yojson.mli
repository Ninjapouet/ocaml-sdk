(** Codec_yojson - Yojson driver for the Codec library.

    Encodes/decodes OCaml values to/from JSON via
    {{:https://github.com/ocaml-community/yojson} Yojson}, by traversing
    the {!Codec.t} GADT type description directly. No intermediate copy.

    {[
      type user = { name : string; age : int }

      let user_codec =
        Codec.record "user" (fun name age -> { name; age })
        |> Codec.field "name" Codec.string (fun u -> u.name)
        |> Codec.field "age" Codec.int (fun u -> u.age)
        |> Codec.seal

      let json = Codec_yojson.encode_exn user_codec { name = "Alice"; age = 30 }
      let user = Codec_yojson.decode_exn user_codec json
    ]}
*)

include Codec.DRIVER with type t = Yojson.Safe.t

(** Like {!val:encode} but raises {!Codec.Error.Codec_error} on failure. *)
val encode_exn : 'a Codec.codec -> 'a -> t

(** Like {!val:decode} but raises {!Codec.Error.Codec_error} on failure. *)
val decode_exn : 'a Codec.codec -> t -> 'a

(** Encode an OCaml value to a JSON string. *)
val encode_string : 'a Codec.codec -> 'a -> (string, Codec.error) result

(** Decode a JSON string to an OCaml value. *)
val decode_string : 'a Codec.codec -> string -> ('a, Codec.error) result
