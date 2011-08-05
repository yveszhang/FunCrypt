(* 
This file contains functions typing of the parsing result and build the FC environment. 
*)
open List
open Types
open Debruijn
open Fcparsing
open Common
open Typing

let build_db_term (p: fc_program) : db_term = 
  fc_term_to_db_term (fc_program_to_fc_term (correct_parsing p) )
;;

let unfold_type_in_context (types : (string * cslr_type) list) (c : fc_context) : fc_context = 
  List.map (fun x -> {x with typ = unfold_type types x.typ}) c
;;

let rec unfold_type_in_prog (types : (string * cslr_type) list) (p : fc_program) : fc_program =
  if types = []
  then p
  else
    match p with
      |	P_case (i, t) ->
	P_case (i, unfold_type types t)
      | P_srec (i, t) ->
	P_srec (i, unfold_type types t)
      | P_func (i, (i', a, p')) ->
	P_func (i, (i', {a with typ = unfold_type types a.typ}, unfold_type_in_prog types p'))
      | P_if (i, (p1, p2, p3)) ->
	P_if (i, (unfold_type_in_prog types p1, unfold_type_in_prog types p2, unfold_type_in_prog types p3))
      | P_app (i, (p1, p2)) ->
	P_app (i, (unfold_type_in_prog types p1, unfold_type_in_prog types p2))
      | P_let (i, (s, p1, p2)) ->
	P_let (i, (s, unfold_type_in_prog types p1, unfold_type_in_prog types p2))
      | P_pair (i, (p1, p2)) ->
	P_pair (i, (unfold_type_in_prog types p1, unfold_type_in_prog types p2))
      | P_fst (i, p') ->
	P_fst (i, unfold_type_in_prog types p')
      | P_snd (i, p') ->
	P_snd (i, unfold_type_in_prog types p')
      | P_tensor (i, (p1, p2)) ->
	P_tensor (i, (unfold_type_in_prog types p1, unfold_type_in_prog types p2))
      | P_tslet (i, (s1, s2, p1, p2)) ->
	P_tslet (i, (s1, s2, unfold_type_in_prog types p1, unfold_type_in_prog types p2))
      | P_ret (i, p') ->
	P_ret (i, unfold_type_in_prog types p')
      | P_bind (i, (s, p1, p2)) ->
	P_bind (i, (s, unfold_type_in_prog types p1, unfold_type_in_prog types p2))
      | P_hypo (i, num, (s, (p1, p2), p3)) -> 
	P_hypo (i, num, (s, (unfold_type_in_prog types p1, unfold_type_in_prog types p2), unfold_type_in_prog types p3))
      | P_paren p' ->
	P_paren (unfold_type_in_prog types p')
      | _ -> p
;;

let rec unfold_type_in_fc_term (types : (string * cslr_type) list) (p : fc_term) : fc_term =
  if types = [] 
  then p
  else
    match p with 
      |	FC_case t -> 
	FC_case (unfold_type types t) 
      | FC_srec t -> 
	FC_srec (unfold_type types t) 
      | FC_func (a, p') ->
	FC_func ({a with typ = unfold_type types a.typ}, unfold_type_in_fc_term types p')
      | FC_if (p1, p2, p3) -> 
	FC_if (unfold_type_in_fc_term types p1, unfold_type_in_fc_term types p2, unfold_type_in_fc_term types p3)
      | FC_app (p1, p2) -> 
	FC_app (unfold_type_in_fc_term types p1, unfold_type_in_fc_term types p2) 
      | FC_let (s, p1, p2) ->
	FC_let (s, unfold_type_in_fc_term types p1, unfold_type_in_fc_term types p2)
      | FC_pair (p1, p2) -> 
	FC_pair (unfold_type_in_fc_term types p1, unfold_type_in_fc_term types p2) 
      | FC_fst p' -> 
	FC_fst (unfold_type_in_fc_term types p')
      | FC_snd p' -> 
	FC_snd (unfold_type_in_fc_term types p')
      | FC_tensor (p1, p2) ->
	FC_tensor (unfold_type_in_fc_term types p1, unfold_type_in_fc_term types p2) 
      | FC_tslet (s1, s2, p1, p2) -> 
	FC_tslet (s1, s2, unfold_type_in_fc_term types p1, unfold_type_in_fc_term types p2)
      | FC_ret p' -> 
	FC_ret (unfold_type_in_fc_term types p') 
      | FC_bind (s, p1, p2) -> 
	FC_bind (s, unfold_type_in_fc_term types p1, unfold_type_in_fc_term types p2)
      | FC_hypo (s, c, (p1, p2), p3) -> 
	FC_hypo (s, (unfold_type_in_context types c), 
		 (unfold_type_in_fc_term types p1, unfold_type_in_fc_term types p2), unfold_type_in_fc_term types p3)
      | _ -> p
;;

let convert_typedef (l : parsing_entry list) : (string * cslr_type) list = 
  List.map (fun x -> (x.e_name, x.e_typ)) l
;;

let convert_vardef (types : (string * cslr_type) list) (l : parsing_entry list) : fc_context = 
  let entry_to_context = fun y -> {str=y.e_name; asp=y.e_asp; typ=unfold_type types y.e_typ} in 
  List.map entry_to_context l
;; 

let rec typing_progs (types : (string * cslr_type) list) (c : fc_context) (l : parsing_entry list) (verbose : bool) 
    : parsing_entry list = 
  match l with 
    | [] -> []
    | e :: l' -> 
      let _ =
	  if verbose
	  then (
	    print_endline ("Type checking \"" ^ e.e_name ^ "\":") ;
  	    print_endline ("\t{ " ^ (fc_context_to_string c) ^ " }") ;
	    print_endline ("\t" ^ (fc_term_to_string (fc_program_to_fc_term e.e_prog)))
	  )
	  else ()
      in
      let p = unfold_type_in_prog types e.e_prog in 
      let _typ = infer_prog c p verbose in 
      let _asp = (context_max_arg (reduce_context c (fc_program_to_fc_term p))).asp in 
      let _arg = {str=e.e_name; asp=_asp; typ=_typ} in 
      {e with e_typ = _typ; e_asp = _asp} :: (typing_progs types (_arg :: c) l' verbose)
;;

let rec convert_hypodef (types : (string * cslr_type) list) (c : fc_context) (hypos : parsing_entry list) (verbose : bool) 
    : fc_hypo_def list =
  let typed_hypos = typing_progs types c hypos false in 
  match typed_hypos with 
      [] -> []
    | e1 :: e2 :: l' -> 
      let name1 = String.sub e1.e_name 0 (String.index e1.e_name '{') in 
      let name2 = String.sub e2.e_name 0 (String.index e2.e_name '{') in 
      let pos = {e1.e_pos with end_pos = e2.e_pos.end_pos} in 
      if name1 = name2 
      then 
	if e1.e_typ = e2.e_typ
	then 
	  {h_name = name1; 
	    h_left = build_db_term e1.e_prog; 
	    h_right = build_db_term e2.e_prog;
	    h_typ = e1.e_typ
	   } :: (convert_hypodef types c l' verbose) 
	else 
	  raise (FC_type_error (pos, (c, e1.e_prog), TE_hypo (name1, e1.e_typ, e2.e_typ)))
      else 
	raise (FC_parse_error ((position_to_string pos)
			       ^ ": Two consecutive hypothesis programs do not have mathcing names!"))
    | e :: _ -> 
      raise (FC_parse_error ((position_to_string e.e_pos) ^ ": Non-matching hypothesis program!"))
;;

(* progdef_ only combines program definitions, it does not check type. *)
let rec merge_progdef (progs : parsing_entry list) : fc_program =
  match progs with 
    | [] -> P_var (null_pos, "")
    | e :: l' -> 
      let e' = merge_progdef l' in 
      ( match e' with 
	| P_var (_, "") ->  e.e_prog 
	| _ -> 	
	  let pos = {e.e_pos with end_pos = (get_prog_pos e').end_pos} in 
	  P_let (pos, (e.e_name, e.e_prog, e'))
      )
;;
      
let typing_parsing_result (env : fc_parsing_result) (verbose : bool) : fc_parsing_result =
  let _types =  convert_typedef env.typedef in 
  let _context =  convert_vardef _types env.vardef in 
  let _progdef = typing_progs _types _context env.progdef verbose in 
  let _hypodef = typing_progs _types _context env.hypodef verbose in 
  {env with progdef = _progdef; hypodef = _hypodef}
;;

let build_fc_environment (env : fc_parsing_result) (verbose : bool) : fc_environment =
  let _types =  convert_typedef env.typedef in 
  let _vars =  convert_vardef _types env.vardef in 
  let _hypos = convert_hypodef _types _vars env.hypodef verbose in 
  let _p = merge_progdef env.progdef in 
  let _prog = match _p with 
     | P_var (_, "") ->  DB_idx (-1) 
     | _ ->  let _ = infer_prog _vars (unfold_type_in_prog _types _p) false in build_db_term _p 
  in 
  {types=_types; vars=_vars; prog=_prog; hypos=_hypos} 
;;

