(lang dune 3.0)

(name codec)

(generate_opam_files true)

(source (github ocamlpro/ocaml-sdk))
(license ISC)
(authors "OCamlPro <contact@ocamlpro.com>")
(maintainers "OCamlPro <contact@ocamlpro.com>")

(package
 (name codec)
 (synopsis "Minimal, format-agnostic codec library for OCaml")
 (description
  "A lightweight GADT-based type description library for format-agnostic serialization of OCaml types.")
 (depends
  (ocaml (>= 5.1))
  (fmt (>= 0.9))
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)))

(package
 (name marshal)
 (synopsis "Sink-based streaming serialization for the codec library")
 (description
  "Serializes/deserializes values described by a Codec.t into/from a caller-provided sink/source. Same type description as codec, different API (side effects, no intermediate value).")
 (depends
  (ocaml (>= 5.1))
  (codec (= :version))
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)))

(package
 (name codec-yojson)
 (synopsis "Yojson driver for the codec and marshal libraries")
 (description
  "Provides Yojson-based instances for both Codec (value conversion to/from Yojson.Safe.t) and Marshal (streaming JSON to/from Buffer.t, out_channel, and the Yojson AST).")
 (depends
  (ocaml (>= 5.1))
  (codec (= :version))
  (marshal (= :version))
  (yojson (>= 2.0))
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)))

(package
 (name ppx_codec)
 (synopsis "PPX deriver for the codec library")
 (description
  "Automatically derives Codec.codec values for OCaml types (records, variants, tuples, parametric and recursive types).")
 (depends
  (ocaml (>= 5.1))
  (codec (= :version))
  (ppxlib (>= 0.28))))
