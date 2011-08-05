open List
open Printf
open Types
open Fcparsing
open Common
open Debug

type typing_frame = fc_context * fc_program

exception FC_type_error of fc_position * typing_frame * type_error 

let typing_frame_to_string (fm : typing_frame) : string =
  let (ctx, p) = fm in 
  let (nlctx, lctx) = split_context ctx in 
  "{ " ^ (fc_context_to_string nlctx) ^ " || " ^ (fc_context_to_string lctx) ^ " } |-\n" 
		  ^ (fc_term_to_pretty_string (fc_program_to_fc_term   p))
;; 

let srec_type (t: cslr_type) : cslr_type = 
  T_func (t, 
	  Linear, 
	  T_func (T_func (T_bits,
			  Modal,
			  T_func (t, 
				  Linear,
				  t)
	                   ),
		  Modal,
		  T_func (T_bits, 
			  Modal,
			  t)
		  )
  )
;;

let case_type (t: cslr_type) : cslr_type = 
  T_func (T_bits,
	     Linear,
	     T_func (T_prod (t, 
				   T_prod (T_func (T_bits, Linear, t), 
					      T_func (T_bits, Linear, t)
					      )
				   ),
			Linear, 
			t)
	     )
;;

let rec is_sub_type (t1: cslr_type) (t2: cslr_type) : bool = 
  if (is_error_type t1) || (is_error_type t2) then false 
  else 
    if t1 = t2 then true 
    else match (t1, t2) with 
	(T_bits, T_bits) -> true 
      | (T_var s1, T_var s2) -> s1 = s2 
      | (T_func (T_bits, Arrow, t1_res), T_func (T_bits, _, t2_res)) -> 
	is_sub_type t1_res t2_res
      | (T_func (t1_arg, t1_asp, t1_res), T_func (t2_arg, t2_asp, t2_res)) ->
	(is_sub_type t2_arg t1_arg) && (is_sub_type t1_res t2_res) && (asp_leq t2_asp t1_asp)
      | (T_prod (t1_fst, t1_snd), T_prod (t2_fst, t2_snd)) 
      | (T_tensor (t1_fst, t1_snd), T_tensor (t2_fst, t2_snd)) -> 
	(is_sub_type t1_fst t2_fst) && (is_sub_type t1_snd t2_snd)
      | (T_proba t1_val, T_proba t2_val) -> 
	is_sub_type t1_val t2_val
      | _ -> false
;;

let rec type_equal_ignore_aspect (t1 : cslr_type) (t2 : cslr_type) (level : int) : bool =
  if level <= 0 
  then (t1 = t2)
  else 
    match (t1, t2) with 
      | (T_func (t11, _, t12), T_func (t21, _, t22)) -> 
	(t11 = t21) && (type_equal_ignore_aspect t12 t22 (level - 1)) 
      | _ -> (t1 = t2)
;;

let rec non_linearize_bits (c : fc_context) : fc_context = 
  (* Bits can be duplicated without breaking linearity! *)
  match c with 
    [] -> []
  | arg :: c' -> 
      {str=arg.str; asp=if (arg.asp = Linear) && (arg.typ = T_bits) then Arrow else arg.asp; typ=arg.typ} 
      :: (non_linearize_bits c')
;;

let rec rename_prog (p : fc_program) (vs_old: string) (vs_new: string) : fc_program =  
  match p with  
      P_var (i, s) -> if s = vs_old then P_var (i, vs_new) else P_var (i, s)
    | P_nil _ | P_one _ | P_zero _ | P_case _ | P_srec _ | P_rand _ -> p 
    | P_if (i, (e0, e1, e2)) -> 
	P_if (i, (rename_prog e0 vs_old vs_new, rename_prog e1 vs_old vs_new, rename_prog e2 vs_old vs_new))
    | P_func (i, (i', arg, e)) ->
      if (arg.str = vs_old) then p else P_func (i, (i', arg, rename_prog e vs_old vs_new))
    | P_app (i, (e1, e2)) -> 
	P_app (i, (rename_prog e1 vs_old vs_new, rename_prog e2 vs_old vs_new))
    | P_let (i, (s, e1, e2)) -> 
	P_let (i, (s, rename_prog e1 vs_old vs_new, if (s = vs_old) then e2 else rename_prog e2 vs_old vs_new))
    | P_pair (i, (e1, e2)) -> 
	P_pair (i, (rename_prog e1 vs_old vs_new, rename_prog e2 vs_old vs_new))
    | P_fst (i, e) -> 
	P_fst (i, rename_prog e vs_old vs_new)
    | P_snd (i, e) ->
	P_snd (i, rename_prog e vs_old vs_new)
    | P_tensor (i, (e1, e2)) -> 
	P_tensor (i, (rename_prog e1 vs_old vs_new, rename_prog e2 vs_old vs_new))
    | P_tslet (i, (s1, s2, e1, e2)) -> 
	P_tslet (i, (s1, s2, rename_prog e1 vs_old vs_new, 
		     if (s1 = vs_old) || (s2 = vs_old) then e2 else rename_prog e2 vs_old vs_new))
    | P_ret (i, e) -> 
	P_ret (i, rename_prog e vs_old vs_new)
    | P_bind (i, (s, e1, e2)) ->
	P_bind (i, (s, rename_prog e1 vs_old vs_new, if (s = vs_old) then e2 else rename_prog e2 vs_old vs_new))
    | P_hypo (i, num, (s, (e1, e2), e3))  -> 
      P_hypo (i, num, (s, (rename_prog e1 vs_old vs_new, rename_prog e2 vs_old vs_new), rename_prog e3 vs_old vs_new))
    | P_paren e -> 
      P_paren (rename_prog e vs_old vs_new)
;;

let rec args_larger_than (a : aspect) (ctx : fc_context) : fc_argument list =
  if a = Linear then []
  else match ctx with 
    | [] -> []
    | arg ::ctx' ->
      if asp_leq arg.asp a then args_larger_than a ctx' else arg :: (args_larger_than a ctx')
;;

let rec infer_rec (ctx : fc_context) (prog : fc_program) (verbose : bool) : cslr_type =
  let st = (ctx, prog) in 
  let (nlctx, lctx) = split_context (non_linearize_bits (reduce_context ctx (fc_program_to_fc_term prog)) ) in 
  let tt = match prog with 
    | P_var (pos, vs) -> 
      let f_arg = found_var ctx vs in
      if fst f_arg 
      then snd (snd f_arg) 
      else raise (FC_type_error (pos, st, TE_var vs))
    | P_nil _ -> T_bits
    | P_one _ | P_zero _ -> 
	T_func (T_bits, Linear, T_bits) 
    | P_case (pos, t) -> 
      if is_closed_type t then case_type t
      else raise (FC_type_error (pos, st, TE_non_defined t))
    | P_srec (pos, t) -> 
      if is_closed_type t then srec_type t
      else raise (FC_type_error (pos, st, TE_non_defined t))
    | P_if (pos, (e0, e1, e2)) -> 
      let fc_is_free_e0 = fun (a_var: fc_argument) -> fc_is_free a_var.str (fc_program_to_fc_term e0) in
      let (lctx0, lctx12) = (partition fc_is_free_e0 lctx) in 
      let t0 = infer_rec (nlctx @ lctx0) e0 verbose in 
      if t0 <> T_bits 
      then 
	raise (FC_type_error (pos, st,  TE_if_cond t0))
      else
	let t1 = infer_rec (nlctx @ lctx12) e1 verbose in 
	let t2 = infer_rec (nlctx @ lctx12) e2 verbose in 
	if t1 = t2 
	then t1 
	else raise (FC_type_error (pos, st,  TE_if_branch (t1, t2)))
    | P_func (_, (pos_arg, arg, e))  -> 
      if fc_is_free arg.str (fc_program_to_fc_term   e)
      then
 	let (_, new_name) = add_arg_to_context arg ctx in 
	let renamed_e = if new_name = "" then e else rename_prog e arg.str new_name in 
	let renamed_arg = if new_name = "" then arg else {arg with str = new_name} in 
	let rec check_with_asp (a : aspect) = 
	  let (new_ctx, _) = add_arg_to_context {arg with asp = a} ctx in 
	  try (a, infer_rec new_ctx renamed_e verbose) with 
	    | FC_type_error (pos, st, TE_aspect wrong_args) -> 
	      let other_wrong_args = List.filter (fun x -> x.str <> renamed_arg.str) wrong_args in 
	      if (other_wrong_args <> []) 
	      then 		
		raise (FC_type_error (pos, st, TE_aspect other_wrong_args))
	      else 
		if a = Linear 
		then 
		  if (arg.typ = T_bits) then check_with_asp Modal else check_with_asp Arrow
		else 
		  if a = Arrow 
		  then check_with_asp Modal 
		  else raise (FC_type_error (pos_arg, st, TE_func arg)) 
	    | FC_type_error (pos, st, TE_var var_name) -> 
	      if (a =  Linear) 
	      then 
		if (arg.typ = T_bits) then check_with_asp Modal else check_with_asp Arrow
	      else 
		raise (FC_type_error (pos, st, TE_var var_name))
	    | excp -> raise excp
	in 
	let (e_asp, e_type) = check_with_asp Linear in
	let final_asp = if ((arg.typ = T_bits) && (e_asp = Arrow)) then Linear else e_asp in 
	T_func (arg.typ, final_asp, e_type)
      else 
	let e_type = (infer_rec ctx e verbose) in 
	T_func (arg.typ, Linear, e_type)
    | P_app (pos, (e1, e2)) -> 
      let fc_is_free_e1 = fun (a_var: fc_argument) -> fc_is_free a_var.str (fc_program_to_fc_term   e1) in
      let (lctx1, lctx2) = (partition fc_is_free_e1 lctx) in
      let type_e1 = infer_rec (nlctx @ lctx1) e1 verbose in
      let type_e2 = infer_rec (nlctx @ lctx2) e2 verbose in
      (match type_e1 with 
	  T_func (arg_type, _asp, res_type) -> 
	    if (is_sub_type type_e2 arg_type) 
	    then 
	      let large_args = args_larger_than _asp (reduce_context (nlctx @ lctx2) (fc_program_to_fc_term e2)) in 
	      if  (large_args  = []) || ((type_e2 = T_bits) && (_asp = Arrow))
	      then res_type
	      else 
		raise (FC_type_error (pos, st, TE_aspect large_args))
	    else 
	      raise (FC_type_error(pos, st, TE_app (type_e1, type_e2)))
	| _ -> raise (FC_type_error (pos, st, TE_app (type_e1, type_e2)))
      )
    | P_let (_, (v_str, e1, e2)) -> 
      let fc_is_free_e1 = fun (a_var: fc_argument) -> fc_is_free a_var.str (fc_program_to_fc_term   e1) in
      let (lctx_1, lctx_2) = (partition fc_is_free_e1 lctx) in 
      let v_type = infer_rec (nlctx @ lctx_1) e1 verbose in   
      if fc_is_free v_str (fc_program_to_fc_term   e2)
      then 
	let ctx1 = reduce_context (nlctx @ lctx_1) (fc_program_to_fc_term   e1) in 
	let v_asp = (context_max_arg ctx1).asp in 
	let v_var = {str=v_str; asp=v_asp; typ=v_type} in 
	let (new_ctx, new_name) = add_arg_to_context v_var (nlctx @ lctx_2) in 
	try infer_rec new_ctx (if new_name = "" then e2 else rename_prog e2 v_str new_name) verbose with 
	  | FC_type_error (pos, st, TE_aspect wrong_vars) -> 
	    let bound_name = if new_name = "" then v_str else new_name in 
	    let (found, (bad_asp, _)) = found_var wrong_vars bound_name in 
	    if found 
	    then 
	      let other_wrong_vars = remove_var_from_context wrong_vars bound_name in 
	      let wrong_vars_of_e1 = sub_context_of_aspect ctx1 bad_asp in 
	      raise (FC_type_error (pos, st, TE_aspect (union_context wrong_vars_of_e1 other_wrong_vars)))
	    else raise (FC_type_error (pos, st, TE_aspect wrong_vars))
	  | excp -> raise excp
      else 
	infer_rec (nlctx @ lctx_2) e2 verbose 
    | P_pair (_, (e1, e2)) -> 
      let t1 = infer_rec ctx e1 verbose in 
      let t2 = infer_rec ctx e2 verbose in 
      T_prod (t1, t2)
    | P_fst (pos, e) -> 
      ( let t = infer_rec ctx e verbose in 
	match t with 
	    T_prod (t1, _) -> t1
	  | _ -> raise (FC_type_error(pos, st,  TE_proj t))
      )
    | P_snd (pos, e) -> 
      ( let t = infer_rec ctx e verbose in 
	match t with 
	    T_prod (_, t2) -> t2
	  | _ -> raise (FC_type_error(pos, st,  TE_proj t))
      )
    | P_tensor (_, (e1, e2)) -> 
      let fc_is_free_e1 (v: fc_argument) = fc_is_free v.str (fc_program_to_fc_term   e1) in
      let (lctx_1, lctx_2) = (partition fc_is_free_e1 lctx) in 
      let t1 = infer_rec (nlctx @ lctx_1) e1 verbose in 
      let t2 = infer_rec (nlctx @ lctx_2) e2 verbose in
      T_tensor (t1, t2) 
    | P_tslet (pos, (s1, s2, e1, e2)) -> 
      let fc_is_free_e1 = fun (v: fc_argument) -> fc_is_free v.str (fc_program_to_fc_term   e1) in
      let (lctx_1, lctx_2) = (partition fc_is_free_e1 lctx) in 
      let t1 = infer_rec (nlctx @ lctx_1) e1 verbose in 
      if (fc_is_free s1 (fc_program_to_fc_term   e2)) || (fc_is_free s2 (fc_program_to_fc_term   e2)) 
      then 
	( match t1 with 
	    T_tensor (t11, t12) -> 
	      let (c, s11) = add_arg_to_context {str=s1; asp=Linear; typ=t11} (nlctx @ lctx_2) in 
	      let (new_ctx, s22) = add_arg_to_context {str=s2; asp=Linear; typ=t12} c in 
	      let e21 = if s11 = "" then e2 else rename_prog e2 s1 s11 in 
	      let e22 = if s22 = "" then e21 else rename_prog e21 s2 s22 in 
	      ( try infer_rec new_ctx e22 verbose with 
		| FC_type_error (pos, st, TE_aspect wrong_vars) -> 
		  let bound_name_1 = if s11 = "" then s1 else s11 in 
		  let bound_name_2 = if s22 = "" then s2 else s22 in 
		  let (found_1, (_, _)) = found_var wrong_vars bound_name_1 in 
		  let (found_2, (_, _)) = found_var wrong_vars bound_name_2 in 
		  if found_1 
		  then raise (FC_type_error (pos, st, TE_tsaspect s1))
		  else 
		    if found_2  
		    then raise (FC_type_error (pos, st, TE_tsaspect s1))
		    else raise (FC_type_error (pos, st, TE_aspect wrong_vars))
		| excp -> raise excp
	      )
	  | _ -> raise (FC_type_error(pos, st,  TE_tensor t1))
	)
      else
	  infer_rec ctx e2 verbose 
    | P_rand _ -> T_proba T_bits
    | P_bind (pos, (v_str, e1, e2)) -> 
      let fc_is_free_e1 = fun (a_var: fc_argument) -> fc_is_free a_var.str (fc_program_to_fc_term   e1) in
      let (lctx_1, lctx_2) = (partition fc_is_free_e1 lctx) in 
      let t1 = infer_rec (nlctx @ lctx_1) e1 verbose in 
      ( match t1 with 
	| T_proba t11 -> 
	  let t2 =
	    if fc_is_free v_str (fc_program_to_fc_term   e2) 
	    then 
	      let ctx1 = reduce_context (nlctx @ lctx_1) (fc_program_to_fc_term   e1) in 
	      let v_asp = (context_max_arg ctx1).asp in 
	      let arg = {str=v_str; asp=v_asp; typ=t11} in 
	      let (new_ctx, new_name) = add_arg_to_context arg (nlctx @ lctx_2) in 
	      try infer_rec new_ctx (if new_name = "" then e2 else rename_prog e2 v_str new_name) verbose with 
		| FC_type_error (pos, st, TE_aspect wrong_vars) -> 
		  let bound_name = if new_name = "" then v_str else new_name in 
		  let (found, (bad_asp, _)) = found_var wrong_vars bound_name in 
		  if found 
		  then 
		    let other_wrong_vars = remove_var_from_context wrong_vars bound_name in 
		    let wrong_vars_of_e1 = sub_context_of_aspect ctx1 bad_asp in 
		    raise (FC_type_error (pos, st, TE_aspect (union_context wrong_vars_of_e1 other_wrong_vars)))
		  else raise (FC_type_error (pos, st, TE_aspect wrong_vars))
		| excp -> raise excp
	    else 
	      infer_rec ctx e2 verbose 
	  in
	  ( match t2 with 
	    | T_proba _ -> t2 
	    | _ -> raise (FC_type_error (pos, st,  TE_bind (t1, t2)))
	  )
	| _ -> raise (FC_type_error(pos, st,  TE_bind (t1, T_error "Wrong in first type")))
      )  
    | P_ret (_, e)  -> 
      T_proba (infer_rec ctx e verbose)
    | P_hypo (pos, num, (s, (e1, e2), e3)) -> 
      let t1 = infer_rec ctx e1 verbose in 
      let t2 = infer_rec ctx e2 verbose in 
      if (type_equal_ignore_aspect t1 t2 num)
      then infer_rec ctx e3 verbose 
      else raise (FC_type_error (pos, st, TE_hypo (s, t1, t2)))
    | P_paren p -> 
      infer_rec ctx p verbose 
  in 
  let _ = 
    if verbose
    then print_endline ((typing_frame_to_string (ctx, prog)) ^  " : " ^ (cslr_type_to_string tt)) 
    else () 
  in 
  tt
;;

let infer_prog (c : fc_context) (p : fc_program) (verbose : bool)  : cslr_type =
  infer_rec c p verbose
;;

let infer (c : fc_context) (e : fc_term) (verbose : bool)  : cslr_type =
  let prog = fc_term_to_fc_program e in 
  let ctx = non_linearize_bits (reduce_context c e) in 
  infer_rec ctx  prog verbose
;;

let infer_string (c : fc_context) (s : string) (verbose : bool) : cslr_type =
  let prog = parse_string_to_fc_program s in 
  let ctx = non_linearize_bits (reduce_context c (fc_program_to_fc_term prog)) in 
  infer_rec ctx prog verbose 
;;
