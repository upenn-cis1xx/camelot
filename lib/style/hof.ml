open Canonical
open Check
open Utils
open Astutils
    
module UseMap : STRUCTURECHECK = struct
  type ctxt = Parsetree.structure_item_desc Pctxt.pctxt

  let fix = "using a higher order function like transform"

  let violation = "overly verbose function implementation"

  let check st (P {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pstr_value (Asttypes.Recursive, [vb]) ->
        let func_name = ident_of_let vb in
        let func_body_exp = body_of_fun vb.pvb_expr in
        begin match func_body_exp.pexp_desc with
        | Pexp_match (_, [c_empty; c_cons]) ->
          let empty_case_ok = is_pat_constr c_empty.pc_lhs "[]" &&
                              c_empty.pc_rhs =| "[]" in
          let tail_binding = binding_of_lcase c_cons in
          let uses_func_ok = uses_func_recursively_list c_cons func_name tail_binding in
          if empty_case_ok && uses_func_ok then
            st := Hint.mk_hint location source fix violation :: !st
        | _ -> ()
      end
    | _ -> ()
    end
    
end

module UseIter : STRUCTURECHECK = struct
  
  type ctxt = Parsetree.structure_item_desc Pctxt.pctxt

  let fix = "using a higher order function like iter"

  let violation = "overly verbose function implementation"

  let check st (P {location; source; pattern} : ctxt) =
    begin match pattern with
      | Pstr_value (Asttypes.Recursive, [vb]) ->
        let func_name = ident_of_let vb in
        let func_body_exp = body_of_fun vb.pvb_expr in
        begin match func_body_exp.pexp_desc with
        | Pexp_match (_, [c_empty; c_cons]) ->
          let empty_case_ok = is_pat_constr c_empty.pc_lhs "[]" &&
                              c_empty.pc_rhs =| "()" in
          let tail_binding = binding_of_lcase c_cons in
          let uses_func_ok = uses_func_recursively_seq c_cons func_name tail_binding in
          if empty_case_ok && uses_func_ok then
            st := Hint.mk_hint location source fix violation :: !st
        | _ -> ()
      end
    | _ -> ()
    end
end
