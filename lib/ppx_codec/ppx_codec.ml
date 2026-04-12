let codec =
  Ppxlib.Deriving.add "codec"
    ~str_type_decl:
      (Ppxlib.Deriving.Generator.V2.make_noarg Gen.generate_str)
