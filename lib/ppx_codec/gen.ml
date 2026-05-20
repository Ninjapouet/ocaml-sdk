open Ppxlib

let codec_name type_name =
  type_name ^ "_codec"

(* -- Attributes ----------------------------------------------------------- *)

let attr_name =
  Attribute.declare "codec.name" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun s -> s)

let attr_name_cd =
  Attribute.declare "codec.name" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun s -> s)

let attr_default =
  Attribute.declare "codec.default" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun e -> e)

let get_field_name ld =
  match Attribute.get attr_name ld with
  | Some s -> s
  | None -> ld.pld_name.txt

let get_ctor_name cd =
  match Attribute.get attr_name_cd cd with
  | Some s -> s
  | None -> cd.pcd_name.txt

(* -- Helpers -------------------------------------------------------------- *)

let loc_err ~loc msg =
  Location.raise_errorf ~loc "%s" msg

let elid ~loc parts =
  Ast_builder.Default.pexp_ident ~loc
    { loc; txt = Ldot (List.fold_left (fun acc p -> Ldot (acc, p))
                         (Lident (List.hd parts)) (List.tl (List.rev (List.tl (List.rev parts)))),
                       List.hd (List.rev parts)) }

let evar ~loc s = Ast_builder.Default.evar ~loc s
let pvar ~loc s = Ast_builder.Default.pvar ~loc s

let codec_lid ~loc name =
  Ast_builder.Default.pexp_ident ~loc
    { loc; txt = Ldot (Lident "Codec", name) }

let apply ~loc f args =
  Ast_builder.Default.eapply ~loc f args

let pipe ~loc e1 e2 =
  apply ~loc (Ast_builder.Default.evar ~loc "|>") [ e1; e2 ]

(** [make_destruct ~loc pat rhs] generates:
    [fun _x -> match _x with | <pat> -> Some <rhs> | _ -> None] *)
let make_destruct ~loc pat rhs =
  let open Ast_builder.Default in
  pexp_fun ~loc Nolabel None (pvar ~loc "_x")
    (pexp_match ~loc (evar ~loc "_x")
       [ case ~lhs:pat ~guard:None
           ~rhs:(pexp_construct ~loc { loc; txt = Lident "Some" } (Some rhs));
         case ~lhs:(ppat_any ~loc) ~guard:None
           ~rhs:(pexp_construct ~loc { loc; txt = Lident "None" } None) ])

(* -- Recursion detection -------------------------------------------------- *)

(** Collect type names defined in a set of mutual type declarations *)
let defined_names tds =
  List.map (fun td -> td.ptype_name.txt) tds

(** Check if a core_type references any of the given type names *)
let rec type_references names ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; _ }, args) ->
    List.mem name names || List.exists (type_references names) args
  | Ptyp_constr (_, args) ->
    List.exists (type_references names) args
  | Ptyp_tuple cts ->
    List.exists (type_references names) cts
  | Ptyp_var _ -> false
  | Ptyp_arrow (_, t1, t2) ->
    type_references names t1 || type_references names t2
  | _ -> false

let td_references names td =
  match td.ptype_kind with
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | Some ct -> type_references names ct
     | None -> false)
  | Ptype_record lds ->
    List.exists (fun ld -> type_references names ld.pld_type) lds
  | Ptype_variant cds ->
    List.exists (fun cd ->
        match cd.pcd_args with
        | Pcstr_tuple cts -> List.exists (type_references names) cts
        | Pcstr_record lds ->
          List.exists (fun ld -> type_references names ld.pld_type) lds
      ) cds
  | Ptype_open -> false

let is_recursive tds =
  let names = defined_names tds in
  List.exists (td_references names) tds

(* -- Core type → Codec expression ----------------------------------------- *)

(** [rec_names]: names of types in the current recursive group.
    References to these are wrapped in [Codec.lazy_] because during
    [let rec], they have type [_ lazy_t]. *)
let rec codec_of_core_type ~loc ~rec_names ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; _ }, []) when
      List.mem name [ "unit"; "bool"; "int"; "int32"; "int64";
                      "float"; "char"; "string" ] ->
    codec_lid ~loc name
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ arg ]) ->
    apply ~loc (codec_lid ~loc "option") [ codec_of_core_type ~loc ~rec_names arg ]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ arg ]) ->
    apply ~loc (codec_lid ~loc "list") [ codec_of_core_type ~loc ~rec_names arg ]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ arg ]) ->
    apply ~loc (codec_lid ~loc "array") [ codec_of_core_type ~loc ~rec_names arg ]
  | Ptyp_constr ({ txt = Ldot (Lident "Seq", "t"); _ }, [ arg ]) ->
    apply ~loc (codec_lid ~loc "seq") [ codec_of_core_type ~loc ~rec_names arg ]
  | Ptyp_constr ({ txt = Ldot (Lident "Queue", "t"); _ }, [ arg ]) ->
    apply ~loc (codec_lid ~loc "queue") [ codec_of_core_type ~loc ~rec_names arg ]
  | Ptyp_constr ({ txt = Ldot (Lident "Hashtbl", "t"); _ }, [ k; v ]) ->
    apply ~loc (codec_lid ~loc "hashtbl")
      [ codec_of_core_type ~loc ~rec_names k;
        codec_of_core_type ~loc ~rec_names v ]
  | Ptyp_constr ({ txt; _ }, args) ->
    let base_name = match txt with
      | Lident name -> codec_name name
      | Ldot (prefix, name) ->
        let ident_str = Format.asprintf "%a" Pprintast.longident prefix in
        ident_str ^ "." ^ codec_name name
      | Lapply _ -> loc_err ~loc "functor application in type not supported"
    in
    let base = evar ~loc base_name in
    let result = match args with
      | [] -> base
      | _ -> apply ~loc base (List.map (codec_of_core_type ~loc ~rec_names) args)
    in
    (* Wrap recursive references: during let rec, they are lazy_t *)
    let is_rec = match txt with
      | Lident name -> List.mem name rec_names
      | _ -> false
    in
    if is_rec then
      apply ~loc (codec_lid ~loc "lazy_") [ result ]
    else result
  | Ptyp_var name ->
    evar ~loc (codec_name name)
  | Ptyp_tuple components ->
    let n = List.length components in
    if n < 2 || n > 6 then
      loc_err ~loc "tuples must have 2 to 6 components";
    let tuple_fn = codec_lid ~loc (Printf.sprintf "tuple%d" n) in
    apply ~loc tuple_fn (List.map (codec_of_core_type ~loc ~rec_names) components)
  | _ ->
    loc_err ~loc "unsupported type in [@@deriving codec]"

(* -- Record generation ---------------------------------------------------- *)

let gen_record ~loc ~rec_names ~type_name lds =
  let open Ast_builder.Default in
  (* Constructor: fun field1 field2 ... -> { field1; field2; ... } *)
  let field_names = List.map (fun ld -> ld.pld_name.txt) lds in
  let constructor =
    List.fold_right
      (fun name body ->
         pexp_fun ~loc Nolabel None (pvar ~loc name) body)
      field_names
      (pexp_record ~loc
         (List.map (fun name ->
              ({ loc; txt = Lident name }, evar ~loc name))
            field_names)
         None)
  in
  (* Start: Codec.record "type_name" constructor *)
  let start =
    apply ~loc (codec_lid ~loc "record")
      [ estring ~loc type_name; constructor ]
  in
  (* Pipeline: |> Codec.field "name" repr (fun r -> r.name) *)
  let with_fields =
    List.fold_left
      (fun acc ld ->
         let fname = get_field_name ld in
         let has_default = Attribute.get attr_default ld in
         let is_option = match ld.pld_type.ptyp_desc with
           | Ptyp_constr ({ txt = Lident "option"; _ }, _) -> true
           | _ -> false
         in
         let getter =
           pexp_fun ~loc Nolabel None (pvar ~loc "r")
             (pexp_field ~loc (evar ~loc "r")
                { loc; txt = Lident ld.pld_name.txt })
         in
         match has_default, is_option with
         | Some default_expr, _ ->
           (* field ~default:expr "name" repr getter *)
           let field_call =
             pexp_apply ~loc (codec_lid ~loc "field")
               [ (Labelled "default", default_expr);
                 (Nolabel, estring ~loc fname);
                 (Nolabel, codec_of_core_type ~loc ~rec_names ld.pld_type);
                 (Nolabel, getter) ]
           in
           pipe ~loc acc field_call
         | None, true ->
           (* field_opt for 'a option without explicit default *)
           let inner_type = match ld.pld_type.ptyp_desc with
             | Ptyp_constr (_, [ arg ]) -> arg
             | _ -> assert false
           in
           let field_opt_call =
             apply ~loc (codec_lid ~loc "field_opt")
               [ estring ~loc fname;
                 codec_of_core_type ~loc ~rec_names inner_type;
                 getter ]
           in
           pipe ~loc acc field_opt_call
         | None, false ->
           let field_call =
             apply ~loc (codec_lid ~loc "field")
               [ estring ~loc fname;
                 codec_of_core_type ~loc ~rec_names ld.pld_type;
                 getter ]
           in
           pipe ~loc acc field_call)
      start lds
  in
  (* Seal *)
  pipe ~loc with_fields (codec_lid ~loc "seal")

(* -- Variant generation --------------------------------------------------- *)

let gen_case ~loc ~rec_names cd =
  let open Ast_builder.Default in
  let cname = get_ctor_name cd in
  let ctor_lid = { loc; txt = Lident cd.pcd_name.txt } in
  match cd.pcd_args with
  | Pcstr_tuple [] ->
    (* case0 "Name" Name *)
    apply ~loc (codec_lid ~loc "case0")
      [ estring ~loc cname;
        pexp_construct ~loc ctor_lid None ]
  | Pcstr_tuple [ single ] ->
    (* case "Name" repr
         (function Name v -> Some v | _ -> None)
         (fun v -> Name v) *)
    let v = pvar ~loc "v" in
    let ve = evar ~loc "v" in
    let destruct =
      make_destruct ~loc
        (ppat_construct ~loc ctor_lid (Some v))
        ve
    in
    let construct =
      pexp_fun ~loc Nolabel None v
        (pexp_construct ~loc ctor_lid (Some ve))
    in
    apply ~loc (codec_lid ~loc "case")
      [ estring ~loc cname;
        codec_of_core_type ~loc ~rec_names single;
        destruct;
        construct ]
  | Pcstr_tuple components ->
    (* Multiple args: use tupleN *)
    let n = List.length components in
    if n > 6 then loc_err ~loc "variant constructors with more than 6 arguments not supported";
    let vars = List.mapi (fun i _ -> Printf.sprintf "v%d" i) components in
    let var_pats = List.map (pvar ~loc) vars in
    let var_exprs = List.map (evar ~loc) vars in
    let tup_pat = ppat_tuple ~loc var_pats in
    let tup_expr = pexp_tuple ~loc var_exprs in
    let ctor_pat = ppat_construct ~loc ctor_lid (Some tup_pat) in
    let ctor_expr = pexp_construct ~loc ctor_lid (Some tup_expr) in
    let destruct = make_destruct ~loc ctor_pat tup_expr in
    let construct =
      pexp_fun ~loc Nolabel None tup_pat ctor_expr
    in
    let repr =
      let tuple_fn = codec_lid ~loc (Printf.sprintf "tuple%d" n) in
      apply ~loc tuple_fn (List.map (codec_of_core_type ~loc ~rec_names) components)
    in
    apply ~loc (codec_lid ~loc "case")
      [ estring ~loc cname; repr; destruct; construct ]
  | Pcstr_record lds ->
    (* Inline record: generate a record codec as the payload *)
    let field_names = List.map (fun ld -> ld.pld_name.txt) lds in
    let record_repr = gen_record ~loc ~rec_names ~type_name:cd.pcd_name.txt lds in
    let vars = List.map (fun name -> evar ~loc name) field_names in
    let var_pats = List.map (fun name -> pvar ~loc name) field_names in
    let record_pat =
      ppat_record ~loc
        (List.map (fun name ->
             ({ loc; txt = Lident name }, pvar ~loc name))
           field_names)
        Closed
    in
    let record_expr =
      pexp_record ~loc
        (List.map (fun name ->
             ({ loc; txt = Lident name }, evar ~loc name))
           field_names)
        None
    in
    let ctor_pat = ppat_construct ~loc ctor_lid (Some record_pat) in
    let ctor_expr = pexp_construct ~loc ctor_lid (Some record_expr) in
    let _ = vars in
    let _ = var_pats in
    let destruct = make_destruct ~loc ctor_pat ctor_expr in
    let construct =
      pexp_fun ~loc Nolabel None (pvar ~loc "v") (evar ~loc "v")
    in
    apply ~loc (codec_lid ~loc "case")
      [ estring ~loc cname; record_repr; destruct; construct ]

let gen_variant ~loc ~rec_names ~type_name cds =
  let open Ast_builder.Default in
  let cases = elist ~loc (List.map (gen_case ~loc ~rec_names) cds) in
  apply ~loc (codec_lid ~loc "variant")
    [ estring ~loc type_name; cases ]

(* -- Top-level generation ------------------------------------------------- *)

let generate_one ~loc ~rec_names ~type_params td =
  let open Ast_builder.Default in
  let type_name = td.ptype_name.txt in
  let body = match td.ptype_kind, td.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      codec_of_core_type ~loc ~rec_names manifest
    | Ptype_record lds, _ ->
      gen_record ~loc ~rec_names ~type_name lds
    | Ptype_variant cds, _ ->
      gen_variant ~loc ~rec_names ~type_name cds
    | Ptype_abstract, None ->
      loc_err ~loc "abstract types without manifest not supported by [@@deriving codec]"
    | Ptype_open, _ ->
      loc_err ~loc "open types not supported by [@@deriving codec]"
  in
  (* Add type parameters as function arguments *)
  let body =
    List.fold_right
      (fun (tp, _) body ->
         match tp.ptyp_desc with
         | Ptyp_var name ->
           pexp_fun ~loc Nolabel None
             (pvar ~loc (codec_name name))
             body
         | _ -> body)
      type_params body
  in
  (codec_name type_name, body)

let generate_str ~ctxt (_rec_flag, tds) =
  let open Ast_builder.Default in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let rec_flag = is_recursive tds in
  let bindings =
    List.map
      (fun td ->
         let type_params = td.ptype_params in
         let rec_names = if rec_flag then defined_names tds else [] in
         let (name, body) = generate_one ~loc ~rec_names ~type_params td in
         let body = if rec_flag then pexp_lazy ~loc body else body in
         value_binding ~loc
           ~pat:(pvar ~loc name)
           ~expr:body)
      tds
  in
  if rec_flag then
    (* Recursive: generate
       let rec foo_codec = lazy (...) and bar_codec = lazy (...)
       let foo_codec = Codec.lazy_ foo_codec
       let bar_codec = Codec.lazy_ bar_codec *)
    let rec_stri = pstr_value ~loc Recursive bindings in
    let unwrap_stris =
      List.map
        (fun td ->
           let name = codec_name td.ptype_name.txt in
           pstr_value ~loc Nonrecursive
             [ value_binding ~loc
                 ~pat:(pvar ~loc name)
                 ~expr:(apply ~loc (codec_lid ~loc "lazy_") [ evar ~loc name ]) ])
        tds
    in
    rec_stri :: unwrap_stris
  else
    [ pstr_value ~loc Nonrecursive bindings ]
