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
 (name codec-yojson)
 (synopsis "Yojson driver for the codec library")
 (description
  "Serializes OCaml types to JSON via Yojson, guided by Codec type representations. No intermediate copy.")
 (depends
  (ocaml (>= 5.1))
  (codec (= :version))
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
