(** 
   The iterator modules from the OCaml compiler. Shamelessly copied, since this is
   so tedious to write myself - the compiler guys know whats up. All credits to:
   https://github.com/ocaml/ocaml/blob/trunk/parsing/ast_iterator.ml
*)

open Parsetree
open Location
open Ast_iterator

let iter_fst f (x, _) = f x
let iter_snd f (_, y) = f y
let iter_tuple f1 f2 (x, y) = f1 x; f2 y
let iter_tuple3 f1 f2 f3 (x, y, z) = f1 x; f2 y; f3 z
let iter_opt f = function None -> () | Some x -> f x

let iter_loc sub {loc; txt = _} = sub.location sub loc

module T = struct
  (* Type expressions for the core language *)

  let row_field sub {
      prf_desc;
      prf_loc;
      prf_attributes;
    } =
    sub.location sub prf_loc;
    sub.attributes sub prf_attributes;
    match prf_desc with
    | Rtag (_, _, tl) -> List.iter (sub.typ sub) tl
    | Rinherit t -> sub.typ sub t

  let object_field sub {
      pof_desc;
      pof_loc;
      pof_attributes;
    } =
    sub.location sub pof_loc;
    sub.attributes sub pof_attributes;
    match pof_desc with
    | Otag (_, t) -> sub.typ sub t
    | Oinherit t -> sub.typ sub t

  let iter sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs; _} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ptyp_any
    | Ptyp_var _ -> ()
    | Ptyp_arrow (_lab, t1, t2) ->
      sub.typ sub t1; sub.typ sub t2
    | Ptyp_tuple tyl -> List.iter (sub.typ sub) tyl
    | Ptyp_constr (lid, tl) ->
      iter_loc sub lid; List.iter (sub.typ sub) tl
    | Ptyp_object (ol, _o) ->
      List.iter (object_field sub) ol
    | Ptyp_class (lid, tl) ->
      iter_loc sub lid; List.iter (sub.typ sub) tl
    | Ptyp_alias (t, _) -> sub.typ sub t
    | Ptyp_variant (rl, _b, _ll) ->
      List.iter (row_field sub) rl
    | Ptyp_poly (_, t) -> sub.typ sub t
    | Ptyp_package (lid, l) ->
      iter_loc sub lid;
      List.iter (iter_tuple (iter_loc sub) (sub.typ sub)) l
    | Ptyp_extension x -> sub.extension sub x

  let iter_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private = _;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    iter_loc sub ptype_name;
    List.iter (iter_fst (sub.typ sub)) ptype_params;
    List.iter
      (iter_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
      ptype_cstrs;
    sub.type_kind sub ptype_kind;
    iter_opt (sub.typ sub) ptype_manifest;
    sub.location sub ptype_loc;
    sub.attributes sub ptype_attributes

  let iter_type_kind sub = function
    | Ptype_abstract -> ()
    | Ptype_variant l ->
      List.iter (sub.constructor_declaration sub) l
    | Ptype_record l -> List.iter (sub.label_declaration sub) l
    | Ptype_open -> ()

  let iter_constructor_arguments sub = function
    | Pcstr_tuple l -> List.iter (sub.typ sub) l
    | Pcstr_record l ->
      List.iter (sub.label_declaration sub) l

  let iter_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private = _;
       ptyext_loc;
       ptyext_attributes} =
    iter_loc sub ptyext_path;
    List.iter (sub.extension_constructor sub) ptyext_constructors;
    List.iter (iter_fst (sub.typ sub)) ptyext_params;
    sub.location sub ptyext_loc;
    sub.attributes sub ptyext_attributes

  let iter_type_exception sub
      {ptyexn_constructor; ptyexn_loc; ptyexn_attributes} =
    sub.extension_constructor sub ptyexn_constructor;
    sub.location sub ptyexn_loc;
    sub.attributes sub ptyexn_attributes

  let iter_extension_constructor_kind sub = function
      Pext_decl(ctl, cto) ->
      iter_constructor_arguments sub ctl; iter_opt (sub.typ sub) cto
    | Pext_rebind li ->
      iter_loc sub li

  let iter_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    iter_loc sub pext_name;
    iter_extension_constructor_kind sub pext_kind;
    sub.location sub pext_loc;
    sub.attributes sub pext_attributes

end

module CT = struct
  (* Type expressions for the class language *)

  let iter sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcty_constr (lid, tys) ->
      iter_loc sub lid; List.iter (sub.typ sub) tys
    | Pcty_signature x -> sub.class_signature sub x
    | Pcty_arrow (_lab, t, ct) ->
      sub.typ sub t; sub.class_type sub ct
    | Pcty_extension x -> sub.extension sub x
    | Pcty_open (o, e) ->
      sub.open_description sub o; sub.class_type sub e

  let iter_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pctf_inherit ct -> sub.class_type sub ct
    | Pctf_val (_s, _m, _v, t) -> sub.typ sub t
    | Pctf_method (_s, _p, _v, t) -> sub.typ sub t
    | Pctf_constraint (t1, t2) ->
      sub.typ sub t1; sub.typ sub t2
    | Pctf_attribute x -> sub.attribute sub x
    | Pctf_extension x -> sub.extension sub x

  let iter_signature sub {pcsig_self; pcsig_fields} =
    sub.typ sub pcsig_self;
    List.iter (sub.class_type_field sub) pcsig_fields
end

let iter_functor_param sub = function
  | Unit -> ()
  | Named (name, mty) ->
    iter_loc sub name;
    sub.module_type sub mty

module MT = struct
  (* Type expressions for the module language *)

  let iter sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmty_ident s -> iter_loc sub s
    | Pmty_alias s -> iter_loc sub s
    | Pmty_signature sg -> sub.signature sub sg
    | Pmty_functor (param, mt2) ->
      iter_functor_param sub param;
      sub.module_type sub mt2
    | Pmty_with (mt, l) ->
      sub.module_type sub mt;
      List.iter (sub.with_constraint sub) l
    | Pmty_typeof me -> sub.module_expr sub me
    | Pmty_extension x -> sub.extension sub x

  let iter_with_constraint sub = function
    | Pwith_type (lid, d) ->
      iter_loc sub lid; sub.type_declaration sub d
    | Pwith_module (lid, lid2) ->
      iter_loc sub lid; iter_loc sub lid2
    | Pwith_typesubst (lid, d) ->
      iter_loc sub lid; sub.type_declaration sub d
    | Pwith_modsubst (s, lid) ->
      iter_loc sub s; iter_loc sub lid

  let iter_signature_item sub {psig_desc = desc; psig_loc = loc} =
    sub.location sub loc;
    match desc with
    | Psig_value vd -> sub.value_description sub vd
    | Psig_type (_, l)
    | Psig_typesubst l ->
      List.iter (sub.type_declaration sub) l
    | Psig_typext te -> sub.type_extension sub te
    | Psig_exception ed -> sub.type_exception sub ed
    | Psig_module x -> sub.module_declaration sub x
    | Psig_modsubst x -> sub.module_substitution sub x
    | Psig_recmodule l ->
      List.iter (sub.module_declaration sub) l
    | Psig_modtype x -> sub.module_type_declaration sub x
    | Psig_open x -> sub.open_description sub x
    | Psig_include x -> sub.include_description sub x
    | Psig_class l -> List.iter (sub.class_description sub) l
    | Psig_class_type l ->
      List.iter (sub.class_type_declaration sub) l
    | Psig_extension (x, attrs) ->
      sub.attributes sub attrs;
      sub.extension sub x
    | Psig_attribute x -> sub.attribute sub x
end


module M = struct
  (* Value expressions for the module language *)

  let iter sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmod_ident x -> iter_loc sub x
    | Pmod_structure str -> sub.structure sub str
    | Pmod_functor (param, body) ->
      iter_functor_param sub param;
      sub.module_expr sub body
    | Pmod_apply (m1, m2) ->
      sub.module_expr sub m1; sub.module_expr sub m2
    | Pmod_constraint (m, mty) ->
      sub.module_expr sub m; sub.module_type sub mty
    | Pmod_unpack e -> sub.expr sub e
    | Pmod_extension x -> sub.extension sub x

  let iter_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    sub.location sub loc;
    match desc with
    | Pstr_eval (x, attrs) ->
      sub.attributes sub attrs; sub.expr sub x
    | Pstr_value (_r, vbs) -> List.iter (sub.value_binding sub) vbs
    | Pstr_primitive vd -> sub.value_description sub vd
    | Pstr_type (_rf, l) -> List.iter (sub.type_declaration sub) l
    | Pstr_typext te -> sub.type_extension sub te
    | Pstr_exception ed -> sub.type_exception sub ed
    | Pstr_module x -> sub.module_binding sub x
    | Pstr_recmodule l -> List.iter (sub.module_binding sub) l
    | Pstr_modtype x -> sub.module_type_declaration sub x
    | Pstr_open x -> sub.open_declaration sub x
    | Pstr_class l -> List.iter (sub.class_declaration sub) l
    | Pstr_class_type l ->
      List.iter (sub.class_type_declaration sub) l
    | Pstr_include x -> sub.include_declaration sub x
    | Pstr_extension (x, attrs) ->
      sub.attributes sub attrs; sub.extension sub x
    | Pstr_attribute x -> sub.attribute sub x
end

module E = struct
  (* Value expressions for the core language *)

  let iter sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs; pexp_loc_stack = _} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pexp_ident x -> iter_loc sub x
    | Pexp_constant _ -> ()
    | Pexp_let (_r, vbs, e) ->
      List.iter (sub.value_binding sub) vbs;
      sub.expr sub e
    | Pexp_fun (_lab, def, p, e) ->
      iter_opt (sub.expr sub) def;
      sub.pat sub p;
      sub.expr sub e
    | Pexp_function pel -> sub.cases sub pel
    | Pexp_apply (e, l) ->
      sub.expr sub e; List.iter (iter_snd (sub.expr sub)) l
    | Pexp_match (e, pel) ->
      sub.expr sub e; sub.cases sub pel
    | Pexp_try (e, pel) -> sub.expr sub e; sub.cases sub pel
    | Pexp_tuple el -> List.iter (sub.expr sub) el
    | Pexp_construct (lid, arg) ->
      iter_loc sub lid; iter_opt (sub.expr sub) arg
    | Pexp_variant (_lab, eo) ->
      iter_opt (sub.expr sub) eo
    | Pexp_record (l, eo) ->
      List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) l;
      iter_opt (sub.expr sub) eo
    | Pexp_field (e, lid) ->
      sub.expr sub e; iter_loc sub lid
    | Pexp_setfield (e1, lid, e2) ->
      sub.expr sub e1; iter_loc sub lid;
      sub.expr sub e2
    | Pexp_array el -> List.iter (sub.expr sub) el
    | Pexp_ifthenelse (e1, e2, e3) ->
      sub.expr sub e1; sub.expr sub e2;
      iter_opt (sub.expr sub) e3
    | Pexp_sequence (e1, e2) ->
      sub.expr sub e1; sub.expr sub e2
    | Pexp_while (e1, e2) ->
      sub.expr sub e1; sub.expr sub e2
    | Pexp_for (p, e1, e2, _d, e3) ->
      sub.pat sub p; sub.expr sub e1; sub.expr sub e2;
      sub.expr sub e3
    | Pexp_coerce (e, t1, t2) ->
      sub.expr sub e; iter_opt (sub.typ sub) t1;
      sub.typ sub t2
    | Pexp_constraint (e, t) ->
      sub.expr sub e; sub.typ sub t
    | Pexp_send (e, _s) -> sub.expr sub e
    | Pexp_new lid -> iter_loc sub lid
    | Pexp_setinstvar (s, e) ->
      iter_loc sub s; sub.expr sub e
    | Pexp_override sel ->
      List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) sel
    | Pexp_letmodule (s, me, e) ->
      iter_loc sub s; sub.module_expr sub me;
      sub.expr sub e
    | Pexp_letexception (cd, e) ->
      sub.extension_constructor sub cd;
      sub.expr sub e
    | Pexp_assert e -> sub.expr sub e
    | Pexp_lazy e -> sub.expr sub e
    | Pexp_poly (e, t) ->
      sub.expr sub e; iter_opt (sub.typ sub) t
    | Pexp_object cls -> sub.class_structure sub cls
    | Pexp_newtype (_s, e) -> sub.expr sub e
    | Pexp_pack me -> sub.module_expr sub me
    | Pexp_open (o, e) ->
      sub.open_declaration sub o; sub.expr sub e
    | Pexp_letop {let_; ands; body} ->
      sub.binding_op sub let_;
      List.iter (sub.binding_op sub) ands;
      sub.expr sub body
    | Pexp_extension x -> sub.extension sub x
    | Pexp_unreachable -> ()

  let iter_binding_op sub {pbop_op; pbop_pat; pbop_exp; pbop_loc} =
    iter_loc sub pbop_op;
    sub.pat sub pbop_pat;
    sub.expr sub pbop_exp;
    sub.location sub pbop_loc

end

module P = struct
  (* Patterns *)

  let iter sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs; ppat_loc_stack = _} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ppat_any -> ()
    | Ppat_var s -> iter_loc sub s
    | Ppat_alias (p, s) -> sub.pat sub p; iter_loc sub s
    | Ppat_constant _ -> ()
    | Ppat_interval _ -> ()
    | Ppat_tuple pl -> List.iter (sub.pat sub) pl
    | Ppat_construct (l, p) ->
      iter_loc sub l; iter_opt (sub.pat sub) p
    | Ppat_variant (_l, p) -> iter_opt (sub.pat sub) p
    | Ppat_record (lpl, _cf) ->
      List.iter (iter_tuple (iter_loc sub) (sub.pat sub)) lpl
    | Ppat_array pl -> List.iter (sub.pat sub) pl
    | Ppat_or (p1, p2) -> sub.pat sub p1; sub.pat sub p2
    | Ppat_constraint (p, t) ->
      sub.pat sub p; sub.typ sub t
    | Ppat_type s -> iter_loc sub s
    | Ppat_lazy p -> sub.pat sub p
    | Ppat_unpack s -> iter_loc sub s
    | Ppat_exception p -> sub.pat sub p
    | Ppat_extension x -> sub.extension sub x
    | Ppat_open (lid, p) ->
      iter_loc sub lid; sub.pat sub p

end

module CE = struct
  (* Value expressions for the class language *)

  let iter sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcl_constr (lid, tys) ->
      iter_loc sub lid; List.iter (sub.typ sub) tys
    | Pcl_structure s ->
      sub.class_structure sub s
    | Pcl_fun (_lab, e, p, ce) ->
      iter_opt (sub.expr sub) e;
      sub.pat sub p;
      sub.class_expr sub ce
    | Pcl_apply (ce, l) ->
      sub.class_expr sub ce;
      List.iter (iter_snd (sub.expr sub)) l
    | Pcl_let (_r, vbs, ce) ->
      List.iter (sub.value_binding sub) vbs;
      sub.class_expr sub ce
    | Pcl_constraint (ce, ct) ->
      sub.class_expr sub ce; sub.class_type sub ct
    | Pcl_extension x -> sub.extension sub x
    | Pcl_open (o, e) ->
      sub.open_description sub o; sub.class_expr sub e

  let iter_kind sub = function
    | Cfk_concrete (_o, e) -> sub.expr sub e
    | Cfk_virtual t -> sub.typ sub t

  let iter_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcf_inherit (_o, ce, _s) -> sub.class_expr sub ce
    | Pcf_val (s, _m, k) -> iter_loc sub s; iter_kind sub k
    | Pcf_method (s, _p, k) ->
      iter_loc sub s; iter_kind sub k
    | Pcf_constraint (t1, t2) ->
      sub.typ sub t1; sub.typ sub t2
    | Pcf_initializer e -> sub.expr sub e
    | Pcf_attribute x -> sub.attribute sub x
    | Pcf_extension x -> sub.extension sub x

  let iter_structure sub {pcstr_self; pcstr_fields} =
    sub.pat sub pcstr_self;
    List.iter (sub.class_field sub) pcstr_fields

  let class_infos sub f {pci_virt = _; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    List.iter (iter_fst (sub.typ sub)) pl;
    iter_loc sub pci_name;
    f pci_expr;
    sub.location sub pci_loc;
    sub.attributes sub pci_attributes
end

module ST = struct
  let iter sub =
    List.iter (sub.structure_item sub)
end
