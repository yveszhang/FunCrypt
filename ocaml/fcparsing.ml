open List
open Lexer
open Types

let rec rebuild_app_tree (p : fc_program) : fc_program = 
  match p with 
      P_app (pos123, (e1, P_app (pos23, (e2, e3)) ) ) -> 
	let pos2 = get_prog_pos e2 in 
	let pos12 = {pos123 with end_pos = pos2.end_pos} in 
	rebuild_app_tree (P_app (pos123, (P_app (pos12, (e1, e2)), e3)) ) 
    | _ -> p
;;

(* This function correct (right-associative) order of application of the parsing, 
   to the correct order (left-associative). 
   It also removes the token of parenthesis.*)
let rec correct_parsing (p : fc_program) : fc_program =
  match p with 
    | P_var x -> P_var x 
    | P_nil x -> P_nil x
    | P_one x -> P_one x
    | P_zero x -> P_zero x
    | P_case x -> P_case x
    | P_srec x -> P_srec x
    | P_if (pos, (e, e1, e2)) ->
	P_if (pos, ((correct_parsing e), (correct_parsing e1), (correct_parsing e2)))
    | P_func (pos, (pos', arg, e)) -> 
	P_func (pos, (pos', arg, correct_parsing e))
    | P_app (_, (_, P_app _)) -> 
	correct_parsing (rebuild_app_tree p)
    | P_app (pos, (e1, e2)) -> 
	P_app (pos, (correct_parsing e1, correct_parsing e2))
    | P_let (pos, (s, e1, e2)) -> 
	P_let (pos, (s, correct_parsing e1, correct_parsing e2))
    | P_pair (pos, (e1, e2)) ->
	P_pair (pos, (correct_parsing e1, correct_parsing e2))
    | P_fst (pos1, P_app (pos2, (e1, e2))) ->
      let pos_e1 = get_prog_pos e1 in
      let pos_fst = { pos1 with end_pos = pos_e1.end_pos } in 
      P_app (pos1, (P_fst (pos_fst, correct_parsing e1), correct_parsing e2))
    | P_fst (pos, e) -> 
	P_fst (pos, correct_parsing e)
    | P_snd (pos1, P_app (pos2, (e1, e2))) ->
      let pos_e1 = get_prog_pos e1 in
      let pos_snd = { pos1 with end_pos = pos_e1.end_pos } in 
      P_app (pos1, (P_snd (pos_snd, correct_parsing e1), correct_parsing e2))
    | P_snd (pos, e) -> 
	P_snd (pos, correct_parsing e)
    | P_tensor (pos, (e1, e2)) -> 
	P_tensor (pos, (correct_parsing e1, correct_parsing e2))
    | P_tslet (pos, (s1, s2, e1, e2)) -> 
	P_tslet (pos, (s1, s2, correct_parsing e1, correct_parsing e2))
    | P_rand x -> P_rand x
    | P_ret (pos, e) ->
	P_ret (pos, correct_parsing e)
    | P_bind (pos, (s, e1, e2)) -> 
	P_bind (pos, (s, correct_parsing e1, correct_parsing e2))
    | P_hypo (pos, num, (s, (e1, e2), e3)) -> 
      P_hypo (pos, num, (s, (correct_parsing e1, correct_parsing e2), correct_parsing e3))
    | P_paren e ->
	P_paren (correct_parsing e)
;;

let rec remove_dup_name_rec (names : string list) (l : parsing_entry list) : (parsing_entry list) * (string list) =
  match l with 
      [] -> ([], [])
    | e :: l' -> 
      if List.mem e.e_name names
      then 
	let (l1, l2) = remove_dup_name_rec names l' in 
	let _msg = (position_to_string e.e_pos) ^ " : name " ^ e.e_name
	  ^ " already defined --- the definition is skipped!" 
	in 
	(l1, _msg :: l2)
      else 
	let (l1, l2) = remove_dup_name_rec (e.e_name :: names) l' in (e :: l1, l2) 
;;

let remove_dup_name (x : fc_parsing_result) : fc_parsing_result = 
  let (_typedef, w_t)  = remove_dup_name_rec [] x.typedef in 
  let (_vardef, w_v) = remove_dup_name_rec [] x.vardef in 
  let vars = List.map (fun x -> x.e_name) _vardef in 
  let (_progdef, w_p) = remove_dup_name_rec vars x.progdef in 
  let (_hypodef, w_h) = remove_dup_name_rec [] x.hypodef in 
  {typedef = _typedef; vardef = _vardef; progdef = _progdef; hypodef = _hypodef; 
   warnings = x.warnings @ w_t @ w_v @ w_p @ w_h
  }
;;

let rec found_ill_type_in_type (types : string list) (t : cslr_type) : string = 
  match t with 
    | T_var s -> if List.mem s types then "" else s
    | T_prod (t1, t2) | T_tensor (t1, t2) | T_func (t1, _, t2) ->
      let s = found_ill_type_in_type types t1 in 
      if s = "" then found_ill_type_in_type types t2 else s
    | T_proba t -> found_ill_type_in_type types t
    | _ -> ""
;;

let rec found_ill_type_in_prog (types : string list) (p : fc_program) : fc_position * string = 
  match p with 
    | P_case (pos, t) | P_srec (pos, t) ->
      (pos, found_ill_type_in_type types t)
    | P_func (_, (pos, a, p')) -> 
      let s = found_ill_type_in_type types a.typ in 
      if s = "" then found_ill_type_in_prog types p' else (pos, s) 
    | P_if (_, (p1, p2, p3)) -> 
      let (pos1, s1) = found_ill_type_in_prog types p1 in 
      if s1 = "" 
      then 
	let (pos2, s2) = found_ill_type_in_prog types p2 in 
	if s2 = "" 
	then 
	  found_ill_type_in_prog types p3
	else (pos2, s2)
      else (pos1, s1) 
    | P_app (_, (p1, p2)) | P_let (_, (_, p1, p2)) | P_pair (_, (p1, p2)) 
    | P_tensor (_, (p1, p2)) | P_tslet (_, (_, _, p1, p2)) | P_bind (_, (_, p1, p2)) -> 
      let (pos1, s1) = found_ill_type_in_prog types p1 in 
      if s1 = "" then found_ill_type_in_prog types p2 else (pos1, s1) 
    | P_fst (_, p') | P_snd (_, p') | P_ret (_, p') | P_paren p' -> 
      found_ill_type_in_prog types p'
    | _ -> (null_pos, "")
;;

let remove_ill_type_def (l : parsing_entry list) : (parsing_entry list) * (string list) =
  let rec remove (vars : string list) (l : parsing_entry list)  =
    match l with 
	[] -> ([], []) 
      | e :: l' -> 
	let s' = found_ill_type_in_type vars e.e_typ in 
	if s' = "" 
	then 
	  let (l1, l2) = remove (e.e_name :: vars) l' in (e :: l1, l2)
	else 
	  let (l1, l2) = remove vars l' in 
	  let _msg = (position_to_string e.e_pos) ^ " : type definition contains unknown type idenitifier " ^ s' 
	    ^ " --- the definition is skipped!" 
	  in 
	  (l1, _msg :: l2) 
  in remove [] l
;;

let rec remove_vars_with_ill_type (types : string list) (l : parsing_entry list) 
    : (parsing_entry list) * (string list) =
  match l with 
      [] -> ([], []) 
    | e :: l' -> 
      let s = found_ill_type_in_type types e.e_typ in 
      let (l1, l2) = remove_vars_with_ill_type types l' in 
      if s = "" 
      then 
	(e :: l1, l2) 
      else 
	let msg =  (position_to_string e.e_pos) ^ " : the type of variable " ^ e.e_name
	  ^ " contains unknown type " ^ s ^ " --- declaration skipped!" 
	in (l1, msg :: l2)
;;

let rec remove_progs_with_ill_type (types : string list) (l : parsing_entry list) 
    : (parsing_entry list) * (string list) =
  match l with 
      [] -> ([], [])
    | e :: l' -> 
      let (pos, s) = found_ill_type_in_prog types e.e_prog in 
      let (l1, l2)  = remove_progs_with_ill_type types l' in 
      if s = "" 
      then (e :: l1, l2) 
      else 
	let msg =  (position_to_string pos) ^ " : the definition of program " ^ e.e_name
	  ^ " contains unknown type " ^ s ^ " --- definition skipped!" 
	in (l1, msg :: l2)
;;

let remove_entry_with_undefined_type (x : fc_parsing_result) : fc_parsing_result = 
  let (_typedef, w_t)  = remove_ill_type_def x.typedef in 
  let types = List.map (fun z -> z.e_name) _typedef in 
  let (_vardef, w_v) = remove_vars_with_ill_type types x.vardef in 
  let (_progdef, w_p) = remove_progs_with_ill_type types x.progdef in 
  {x with typedef = _typedef; vardef = _vardef; progdef = _progdef; 
   warnings = x.warnings @ w_t @ w_v @ w_p 
  }
;;

(* Postparsing: remove entries with duplicate name and undefined types, correct the application order of program. *)
let postparsing_env (x : fc_parsing_result) : fc_parsing_result = 
  let l =   remove_entry_with_undefined_type (remove_dup_name x) in 
  let _progdef = List.map (fun x -> {x with e_prog = correct_parsing x.e_prog}) l.progdef in 
  let _hypodef = List.map (fun x -> {x with e_prog = correct_parsing x.e_prog}) l.hypodef in 
  {l with progdef = _progdef; hypodef = _hypodef}
;;

let parse_file (fn : string) (verbose : bool) : fc_parsing_result =
  if Sys.file_exists fn 
  then 
    let in_c = open_in fn in
    let lexbuf = Lexing.from_channel in_c in 
    let parse_res = postparsing_env (Parser.fc_file Lexer.token lexbuf) in 
    let _ = (List.iter (fun x -> print_endline ("WARNING!!! " ^ x)) parse_res.warnings; print_endline "") in 
    parse_res
  else 
    {typedef=[]; vardef=[]; progdef=[]; hypodef=[]; warnings=[]}
;;

let parse_string (s : string) : fc_parsing_result  = 
  let lexbuf = Lexing.from_string s in 
  postparsing_env 
    {typedef = []; vardef = []; 
     progdef = [ {default_entry with e_name = "_"; e_prog = (Parser.fc_prog Lexer.token lexbuf)} ];
     hypodef = [] ; warnings = []
    }
;;

let parse_string_to_fc_program (s : string) : fc_program = 
  let lexbuf = Lexing.from_string s in 
  Parser.fc_prog Lexer.token lexbuf
;;
