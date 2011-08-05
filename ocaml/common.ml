(* This file defines common functions dealing with free variables, contexts, and conversion 
   of internal strucutre to strings etc.  
*)

open List
open Types
open Fcparsing 

let rec sublist b e l = (* sublist 3 5 l: return the sublist between position 3 (the 4th)  and 5 (the 6th) *)
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if e = 0 then [] else sublist (b-1) (e-1) t in
     if b > 0 then tail else h :: tail
;;

let rec remove_duplicate_string (l : string list) : string list =
  let rec remove (s : string) (l : string list) : string list = 
    match l with [] -> [] | s' :: l' -> if (s = s') then remove s l' else s' :: (remove s l')
  in 
  match l with 
      [] -> [] 
    | s :: l' -> s :: remove_duplicate_string (remove s l')
;;

let rec string_list_to_string (l : string list) : string = 
  match l with 
    | [] -> ""
    | s :: l' -> s ^ "; " ^ (string_list_to_string  l')
;;

let rec subset_of_string (l1 : string list) (l2 : string list) : bool = 
  List.mem false (List.map (fun x -> mem x l2) l1)
;;

let aspect_to_string (x : aspect) : string = 
  match x with 
      Linear -> "Linear"
    | Modal -> "Modal"
    | Arrow -> "Non-linear, non-modal"
;;

let rec error_type (t : cslr_type) : bool * string =
  match t with 
      T_error s -> (true, s)
    | T_prod (t1, t2) | T_tensor (t1, t2) | T_func (t1, _, t2) ->
      let (b, s) = (error_type t1) in 
      if b then (b, s) else error_type t2
    | T_proba t -> error_type t
    | _ -> (false, "")
;;

let rec is_error_type (t : cslr_type) : bool = fst (error_type t)
;;

let rec is_closed_type (t : cslr_type) : bool =
  match t with 
      T_var _ -> false
    | T_prod (t1, t2) | T_tensor (t1, t2) | T_func (t1, _, t2) ->
      (is_closed_type t1) && (is_closed_type t2) 
    | T_proba t -> is_closed_type t
    | _ -> true 
;;

let rec type_vars (t : cslr_type) : string list = 
  let vars = match t with 
    | T_var s -> [s]
    | T_prod (t1, t2) | T_tensor (t1, t2) | T_func (t1, _, t2) ->
      (type_vars t1) @ (type_vars t2)
    | T_proba t -> type_vars t 
    | _ -> []
  in remove_duplicate_string vars
;;

let rec instantiate_type (s : string) (s_def : cslr_type) (t : cslr_type) : cslr_type =
    match t with 
	T_var s' -> if s' = s then s_def else t
      | T_prod (t1, t2) -> T_prod (instantiate_type s s_def t1, instantiate_type s s_def t2) 
      | T_tensor (t1, t2) -> T_tensor (instantiate_type s s_def t1, instantiate_type s s_def t2) 
      | T_func (t1, a, t2) -> T_func (instantiate_type s s_def t1, a, instantiate_type s s_def t2) 
      | T_proba t -> T_proba (instantiate_type s s_def t) 
      | _ -> t
;;

let rec unfold_type (type_def : (string * cslr_type) list) (t : cslr_type) : cslr_type = 
  if (type_def = []) || (is_closed_type t) 
  then t 
  else 
    let rec found_type_def (s : string) (l : (string * cslr_type) list) : cslr_type = 
      match l with 
	  [] -> T_error ("The definition of type " ^ s ^ " is not found!") 
	| (s', t') :: l' -> if s = s' then t' else found_type_def s l'
    in 
    match t with 
	T_var s -> unfold_type type_def (found_type_def s type_def)
      | T_prod (t1, t2) -> T_prod (unfold_type type_def t1, unfold_type type_def t2) 
      | T_tensor (t1, t2) -> T_tensor (unfold_type type_def t1, unfold_type type_def t2) 
      | T_func (t1, a, t2) -> T_func (unfold_type type_def t1, a, unfold_type type_def t2) 
      | T_proba t -> T_proba (unfold_type type_def t) 
      | _ -> t    
;;

let rec cslr_type_to_string (t: cslr_type) : string = 
  let (b, s) = error_type t in 
  if b then "Type error [" ^ s ^"]" else 
  match t with 
      T_bits -> "bits"
    | T_var s -> s
    | T_prod (t1, t2) -> 
	"(" ^ (cslr_type_to_string t1) ^ " & " ^ (cslr_type_to_string t2) ^ ")"
    | T_tensor (t1, t2) ->
	"(" ^ (cslr_type_to_string t1) ^ " * " ^ (cslr_type_to_string t2) ^ ")"	
    | T_proba t -> 
	let s = cslr_type_to_string t in
	  if t = T_bits then "$"^s else "$("^s^")"
    | T_func (t1, asp, t2) -> 
	let s1 = cslr_type_to_string t1 in
	let s1' = if t1 = T_bits then s1 else "(" ^ s1 ^ ")" in
	let s2 = cslr_type_to_string t2 in
	  (match asp with 
	      Arrow -> s1' ^ " -> " ^ s2
	    | Modal -> "[" ^ s1 ^ "] -> " ^ s2
	    | Linear -> "<" ^s1 ^ "> -> " ^ s2
	   ) 
    | T_error s0 -> "Type error [" ^ s0 ^"]"
;;

let fc_argument_to_string (arg : fc_argument) : string = 
  match arg.asp with 
      Linear -> arg.str ^ " : " ^ "<" ^ (cslr_type_to_string arg.typ) ^ ">"
    | Modal -> arg.str ^ " : " ^ "[" ^ (cslr_type_to_string arg.typ) ^ "]"
    | Arrow -> arg.str ^ " : " ^ (cslr_type_to_string arg.typ)
;;

let rec fc_args_to_string (ctx: fc_argument list) : string = 
  match ctx with 
      [] -> "" 
  | arg :: ctx' -> " (" ^ (fc_argument_to_string arg) ^ ")" ^ (fc_args_to_string ctx')
;;

let rec fc_context_to_string (ctx: fc_argument list) : string = 
  match ctx with 
    | [] -> "" 
    | [arg] -> fc_argument_to_string arg
    | arg :: ctx' -> (fc_argument_to_string arg) ^ ", " ^ (fc_context_to_string ctx')
;;

let rec fc_term_to_string (e : fc_term) : string = 
  match e with 
    FC_var s -> s
  | FC_nil -> "nil" 
  | FC_one -> "B1"
  | FC_zero -> "B0"
  | FC_case t -> 
      "case (" ^ (cslr_type_to_string t) ^ ")"
  | FC_srec t -> 
      "srec (" ^ (cslr_type_to_string t) ^ ")"
  | FC_if (e1, e2, e3) -> 
      "if " ^ (fc_term_to_string e1) 
      ^ " then " ^ (fc_term_to_string e2) 
      ^ " else " ^ (fc_term_to_string e3)
  | FC_func (v, e') -> 
      "func (" ^ (fc_argument_to_string v) ^ ") -> " 
      ^ (fc_term_to_string e')
  | FC_app (e1, e2) -> 
      (fc_term_to_string e1) ^ " ( " ^ (fc_term_to_string e2) ^" ) "
  | FC_let (v, e1, e2) -> 
      "let " ^ v ^ " = " ^ (fc_term_to_string e1) 
      ^ " in " ^ (fc_term_to_string e2) 
  | FC_pair (e1, e2) -> 
      "< " ^ (fc_term_to_string e1) ^ " , " ^ (fc_term_to_string e2) ^ " >"
  | FC_fst e -> 
      "first " ^ (fc_term_to_string e)
  | FC_snd e -> 
      "second " ^ (fc_term_to_string e)
  | FC_tensor (e1, e2) -> 
      "( " ^ (fc_term_to_string e1) ^ " * " ^ (fc_term_to_string e2) ^ " )"
  | FC_tslet (v1, v2, e1, e2) -> 
      "let (" ^ v1 ^ " * " ^ v2 ^") = " ^ (fc_term_to_string e1) 
      ^ " in " ^ (fc_term_to_string e2) 
  | FC_rand -> "rand" 
  | FC_ret e -> 
      "return " ^ (fc_term_to_string e)
  | FC_bind (v, e1, e2) -> 
      "bind " ^ v ^ " = " ^ (fc_term_to_string e1) 
      ^ " in " ^ (fc_term_to_string e2) 
  | FC_hypo (s, args, (e1, e2), e3)  ->
    "lethypo " ^ s ^ " (" ^ (fc_context_to_string args) ^ " ) = ( " ^ (fc_term_to_string e1) ^ " == " ^ (fc_term_to_string e2) ^ " ) in " 
      ^ (fc_term_to_string e3)
;;

let rec space_of_length (n : int) : string =
  if n = 0 then ""
  else " " ^ (space_of_length (n-1))
;;

let rec fc_term_to_pretty_string_rec (indent : int) (newline : bool) (e : fc_term) : string =
  match e with 
    | FC_var s -> 
      (if newline then (space_of_length indent)  else "") ^ s
    | FC_nil -> 
      (if newline then (space_of_length indent)  else "") ^ "nil" 
    | FC_one -> 
      (if newline then (space_of_length indent)  else "") ^ "B1"
    | FC_zero -> 
      (if newline then (space_of_length indent)  else "") ^ "B0"
    | FC_case t -> 
      (if newline then (space_of_length indent)  else "") ^ "case (" ^ (cslr_type_to_string t) ^ ")"
    | FC_srec t -> 
      (if newline then (space_of_length indent)  else "") ^ "srec (" ^ (cslr_type_to_string t) ^ ")"
    | FC_if (e1, e2, e3) -> 
      (if newline then (space_of_length indent)  else "")
      ^ "if " ^ (fc_term_to_pretty_string_rec indent false e1)  
      ^ " then " ^ (fc_term_to_pretty_string_rec indent false e2) 
      ^ " else " ^ (fc_term_to_pretty_string_rec indent false e3)
    | FC_func (v, e') -> 
      let str = fc_term_to_string e in 
      if String.length str > (100 - indent)
      then 
	(if newline then space_of_length indent else "")
	^ "func (" ^ (fc_argument_to_string v) ^ ") -> \n" 
	^ (fc_term_to_pretty_string_rec (indent + 2) true e')
      else str
    | FC_app (e1, e2) -> 
      ( match e1 with 
	| FC_func _ ->
	  (if newline then space_of_length indent else "") 
	  ^ "( " ^ (fc_term_to_string e1) ^ ")  " ^ (fc_term_to_pretty_string_rec indent false e2) 
	| _ ->
	  (fc_term_to_pretty_string_rec indent newline e1) ^ " ( " ^ (fc_term_to_pretty_string_rec indent false e2) ^" )"
      )
    | FC_let (v, e1, e2) -> 
      let str = (fc_term_to_string e) in 
      if (String.length str) > (100 - indent)
      then 
	( match e1 with 
	  | FC_let _ | FC_tslet _ | FC_bind _ | FC_func _ | FC_hypo _ -> 
	    if (String.length (fc_term_to_string e1)) > (90 - indent) 
	    then 
	      (if newline then (space_of_length indent)  else "") 
	      ^ "let " ^ v ^ " = \n" 
	      ^ (fc_term_to_pretty_string_rec (indent + 2) true e1) ^ "\n" 
	      ^ (if newline then (space_of_length indent)  else "") ^ "in\n" 
	      ^ (fc_term_to_pretty_string_rec indent true e2) 	  
	    else 
	      (if newline then (space_of_length indent)  else "") 
	      ^ "let " ^ v ^ " = " ^ (fc_term_to_pretty_string_rec indent false e1) ^ " in\n" 
	      ^ (fc_term_to_pretty_string_rec indent true e2) 
	  | _ ->	  
	    (if newline then (space_of_length indent)  else "") 
	    ^ "let " ^ v ^ " = " ^ (fc_term_to_pretty_string_rec indent false e1) ^ " in\n" 
	    ^ (fc_term_to_pretty_string_rec indent true e2) 
	)
      else 
	(if newline then (space_of_length indent)  else "")  ^ (fc_term_to_string e)
    | FC_pair (e1, e2) ->
      let str = fc_term_to_string e in 
      if String.length str > (100 - indent) 
      then 
	"\n" ^ (space_of_length indent)  
	^ "< " ^ (fc_term_to_pretty_string_rec indent false e1) ^ " ,\n" 
	^ (fc_term_to_pretty_string_rec (indent + 2)  true e2) ^ " >"
      else str
    | FC_fst e -> 
      (if newline then (space_of_length indent)  else "") 
      ^ "first (" ^ (fc_term_to_pretty_string_rec indent false e) ^ ")"
    | FC_snd e -> 
      (if newline then (space_of_length indent)  else "") 
      ^ "second (" ^ (fc_term_to_pretty_string_rec indent false e) ^ ")"
    | FC_tensor (e1, e2) -> 
      (if newline then (space_of_length indent)  else "") 
      ^ "( " ^ (fc_term_to_pretty_string_rec indent false e1) ^ " * " ^ (fc_term_to_pretty_string_rec indent false e2) ^ " )"
    | FC_tslet (v1, v2, e1, e2) -> 
      let str = (fc_term_to_string e) in 
      if (String.length str) > (100 - indent)
      then 
	( match e1 with 
	  | FC_let _ | FC_tslet _ | FC_bind _ | FC_func _ | FC_hypo _ -> 
	    if (String.length (fc_term_to_string e1)) > (90 - indent) 
	    then 
	      (if newline then (space_of_length indent)  else "") 
	      ^ "let (" ^ v1 ^ " * " ^ v2 ^") = \n "
	      ^ (fc_term_to_pretty_string_rec (indent + 2) true e1) ^ "\n" 
	      ^ (if newline then (space_of_length indent)  else "") ^ "in\n" 
	      ^ (fc_term_to_pretty_string_rec indent true e2) 	  
	    else 
	      (if newline then (space_of_length indent)  else "") 
	      ^ "let (" ^ v1 ^ " * " ^ v2 ^") = " ^ (fc_term_to_pretty_string_rec indent false e1) ^ " in \n" 
	      ^ (fc_term_to_pretty_string_rec indent true e2) 
	  | _ ->	  
	    (if newline then (space_of_length indent)  else "") 
	    ^ "let (" ^ v1 ^ " * " ^ v2 ^") = " ^ (fc_term_to_pretty_string_rec indent false e1) ^ " in \n" 
	    ^ (fc_term_to_pretty_string_rec indent true e2) 
	)
      else
	(if newline then (space_of_length indent)  else "")  ^ (fc_term_to_string e)
    | FC_rand -> 
      (if newline then (space_of_length indent)  else "")  ^ "rand" 
    | FC_ret e -> 
      (if newline then (space_of_length indent)  else "") 
      ^ "return (" ^ (fc_term_to_pretty_string_rec indent false e) ^ ")"
    | FC_bind (v, e1, e2) -> 
      let str = (fc_term_to_string e) in 
      if (String.length str) > (100 - indent)
      then 
	( match e1 with 
	  | FC_let _ | FC_tslet _ | FC_bind _ | FC_func _ | FC_hypo _ -> 
	    if (String.length (fc_term_to_string e1)) > (90 - indent) 
	    then 
	      (if newline then (space_of_length indent)  else "") 
	      ^ "bind " ^ v ^ " = \n" 
	      ^ (fc_term_to_pretty_string_rec (indent + 2) true e1) ^ "\n" 
	      ^ (if newline then (space_of_length indent)  else "") ^ "in\n" 
	      ^ (fc_term_to_pretty_string_rec indent true e2) 	  
	    else 
	      (if newline then (space_of_length indent)  else "") 
	      ^ "bind " ^ v ^ " = " ^ (fc_term_to_pretty_string_rec indent false e1) ^ " in\n" 
	      ^ (fc_term_to_pretty_string_rec indent true e2) 
	  | _ ->	  
	    (if newline then (space_of_length indent)  else "") 
	    ^ "bind " ^ v ^ " = " ^ (fc_term_to_pretty_string_rec indent false e1) ^ " in\n" 
	    ^ (fc_term_to_pretty_string_rec indent true e2) 
	)
      else 
	(if newline then (space_of_length indent)  else "")  ^ (fc_term_to_string e)
    | FC_hypo (s, args, (e1, e2), e3)  ->
      let head = (if newline then (space_of_length indent)  else "") in 
      let s1 = fc_term_to_string e1 in 
      let s2 = fc_term_to_string e2 in 
      head ^ "lethypo " ^ s ^ " ( " ^ (fc_context_to_string args) ^ " ) = \n" 
      ^ ( 
	if (String.length s1) > (98 - indent)
	then (fc_term_to_pretty_string_rec (indent+2) true e1) ^ "\n"
	else head ^ "  " ^ s1 ^ "\n" 
      )
      ^ head ^ " == \n" 
      ^ ( 
	if (String.length s2) > (98 - indent) 
	then (fc_term_to_pretty_string_rec (indent+2) true e2) ^ "\n" 
	else head ^ "  " ^ s2 ^ "\n" 
      )
      ^ head ^ "in \n" 
      ^ (fc_term_to_pretty_string_rec indent true e3)
;;

let fc_term_to_pretty_string (e : fc_term) : string =
  fc_term_to_pretty_string_rec 0 true e
;;

let is_in_context (v : string) (c : fc_context) : bool =
  List.mem v (List.map (fun x -> x.str) c)
;;

(* This function returns a list of free variables of a CSLR term. *)
let rec fc_free_var (tm : fc_term) : string list = 
  let vars =  match tm with
    | FC_var s -> [s]
    | FC_nil | FC_one | FC_zero | FC_srec _ | FC_case _ | FC_rand -> []
    | FC_if (e0, e1, e2) ->
      (fc_free_var (e0)) @ (fc_free_var (e1)) @ (fc_free_var (e2))
    | FC_func (arg, e) ->
      filter (fun x -> x <> arg.str) (fc_free_var e)
    | FC_app (e1, e2) | FC_pair (e1, e2) | FC_tensor (e1, e2) ->   
      (fc_free_var (e1)) @ (fc_free_var (e2))
    | FC_tslet  (s1, s2, e1, e2)  ->
      (fc_free_var e1) @ (filter (fun x -> (x <> s1) && (x <> s2)) (fc_free_var e2))
    | FC_let (s, e1, e2) | FC_bind (s, e1, e2) -> 
      (fc_free_var e1) @ (filter (fun x -> x <> s) (fc_free_var e2))
    | FC_fst e | FC_snd e | FC_ret e  ->
      fc_free_var (e)
    | FC_hypo (_, _, (e1, e2), e3) -> 
      (fc_free_var e1) @ (fc_free_var e2) @ (fc_free_var e3)
  in remove_duplicate_string vars
;;

(* This functions tests whether a given variable is free in a CSLR term. *)
let fc_is_free (s: string) (tm: fc_term) : bool = mem s (fc_free_var tm)
;;
  
(* This function renames a free variable vs_old with a new name vs_new. *)
let rec fc_rename (tm: fc_term) (vs_old: string) (vs_new: string) : fc_term =  
  match tm with  
      FC_var s -> if s = vs_old then FC_var vs_new else FC_var s
    | FC_nil | FC_one | FC_zero | FC_case _ | FC_srec _ | FC_rand -> tm 
    | FC_if (e0, e1, e2) -> 
	FC_if (fc_rename e0 vs_old vs_new, fc_rename e1 vs_old vs_new, fc_rename e2 vs_old vs_new)
    | FC_func (arg, e) ->
      if (arg.str = vs_old) then tm else FC_func (arg, fc_rename e vs_old vs_new)
    | FC_app (e1, e2) -> 
	FC_app (fc_rename e1 vs_old vs_new, fc_rename e2 vs_old vs_new)
    | FC_let (s, e1, e2) -> 
	FC_let (s, fc_rename e1 vs_old vs_new, if (s = vs_old) then e2 else fc_rename e2 vs_old vs_new)
    | FC_pair (e1, e2) -> 
	FC_pair (fc_rename e1 vs_old vs_new, fc_rename e2 vs_old vs_new)
    | FC_fst e -> 
	FC_fst (fc_rename e vs_old vs_new)
    | FC_snd e ->
	FC_snd (fc_rename e vs_old vs_new)
    | FC_tensor (e1, e2) -> 
	FC_tensor (fc_rename e1 vs_old vs_new, fc_rename e2 vs_old vs_new)
    | FC_tslet (s1, s2, e1, e2) -> 
	FC_tslet (s1, s2, fc_rename e1 vs_old vs_new, if (s1 = vs_old) || (s2 = vs_old) then e2 else fc_rename e2 vs_old vs_new)
    | FC_ret e -> 
	FC_ret (fc_rename e vs_old vs_new)
    | FC_bind (s, e1, e2) ->
	FC_bind (s, fc_rename e1 vs_old vs_new, if (s = vs_old) then e2 else fc_rename e2 vs_old vs_new)
    | FC_hypo (s, args, (e1, e2), e3)  -> 
      if is_in_context s args 
      then 
	FC_hypo (s, args, (e1, e2), fc_rename e3 vs_old vs_new)
      else 
	FC_hypo (s, args, (fc_rename e1 vs_old vs_new, fc_rename e2 vs_old vs_new), fc_rename e3 vs_old vs_new)
;;

(**********************************************************************)
(* The following functions deal with CSLR typing contexts. *)

(* The function searches a context for a variable and if found, returns 
   (true, (aspect, type)), othewise returns (false, (Linear, T_error)) *)
let rec found_var (ctx: fc_context) (v: string) : bool * (aspect * cslr_type) = 
  match ctx with 
      [] -> (false, (Linear, T_error ("Variabe \"" ^ v ^ "\" not found in context.")))
    | a_var :: ctx' -> 
	if v = a_var.str 
	then (true, (a_var.asp, a_var.typ)) 
	else found_var ctx' v
;;

let rec union_context (c1 : fc_context) (c2 : fc_context) : fc_context = 
  match c1 with 
    | [] -> c2 
    | a :: c1' -> 
      if List.mem a.str (List.map (fun x -> x.str) c2) 
      then union_context c1' c2 else a :: (union_context c1' c2)
;;

let sub_context_of_aspect (ctx : fc_context) (a : aspect) : fc_context =
  List.filter (fun x -> x.asp = a) ctx 
;;

let remove_var_from_context (ctx : fc_context) (var : string) : fc_context =
  List.filter (fun x -> x.str <> var) ctx
;;

(* This function split a context a non-linear context and a linear context. *)
let split_context (ctx: fc_context) : fc_context * fc_context = 
  match (partition (fun x -> x.asp = Linear) ctx) with 
      (lin_ctx, a_ctx) -> (a_ctx, lin_ctx)
;;

(* The argument with the max aspect in a context *)
let rec context_max_arg (ctx: fc_context) : fc_argument = 
  match ctx with 
      [] -> {str = ""; asp = Modal; typ = T_error "No argument"}
    | a_var :: ctx' -> let a = (context_max_arg ctx') in 
                         if asp_leq a.asp a_var.asp then a_var else a
;;

(* This function checks whether every variable in a context has a lower aspect than the speified one. *)
let rec context_leq_aspect (ctx: fc_context) (a: aspect) : bool = 
  match ctx with 
      [] -> true 
    | a_var :: ctx' -> 
	(asp_leq a_var.asp a) && (context_leq_aspect ctx' a)
;;

(* Remove unused variables w.r.t. the given term.*)
let reduce_context (ctx: fc_context) (e: fc_term) : fc_context = 
  filter (fun (x : fc_argument) -> fc_is_free x.str e) ctx
;;

(* Adding an argument into the context. As the variable may has a name which is already  *)
(* in the context, so renaming of the new variable is possible.  *)
(* The result then consists of the new context and the renamed variable.      *)
(* If no renaming occurs, then the second part of the result is the empty string. *) 
let rec add_arg_to_context (a : fc_argument) (c : fc_context) : fc_context * string = 
  match c with 
    [] -> ([a], "")
  | aa :: c' -> 
      if (a.str = aa.str) 
      then 
	let new_name = "^"^a.str in 
	let x = add_arg_to_context {str=new_name; asp=a.asp; typ=a.typ} c' in 	
	if (snd x) = "" then (aa :: fst x, new_name) else (aa :: fst x, snd x) 
      else 
	let x = add_arg_to_context a c' in (aa :: fst x, snd x)
;;

let type_error_to_string (err : type_error) : string =
  match err with 
    | TE_var s -> 
      "The variable " ^ s ^ " is not found in the typing context! "
      ^ "Probably it is declared as a linear argument but is used non-linearly."
    | TE_non_defined t -> 
      "There are non-defined types: [ " ^ (string_list_to_string (type_vars t)) ^ " ]"
    | TE_if_cond t -> 
      "The type of IF-condition is wrong: " ^ (cslr_type_to_string t) 
    | TE_if_branch (t1, t2) -> 
      "The types of IF_branches are not equal: " ^ (cslr_type_to_string t1) 
      ^ " ; " ^ (cslr_type_to_string t2) 
    | TE_func a -> 
      "No aspect can be assigned to the function argument ( " 
      ^ (fc_argument_to_string {a with asp=Arrow}) ^ " )"
    | TE_aspect a -> 
      "Variables ( " ^ (fc_context_to_string a) ^ " ) have too high aspect!"
    | TE_app (t1, t2) -> 
      ( match t1 with 
	| T_func _ -> 
	  "Function is applied to an argument with wrong type: " 
	  ^ (cslr_type_to_string t1) ^ " ; " ^ (cslr_type_to_string t2)
	| _ -> 
	  "Application with a term of non-function type: " ^ (cslr_type_to_string t1) 
      )
    | TE_proj t -> 
      "Product projection with a non-product type: " ^ (cslr_type_to_string t) 
    | TE_tensor t -> 
      "Tensor projection with a non-tensor type: " ^ (cslr_type_to_string t) 
    | TE_tsaspect s -> 
      "The tensor projection variable " ^ s ^ " is used non-linearly!"
    | TE_bind (t1, t2) -> 
      "Bind with non-proba types: " ^ (cslr_type_to_string t1) ^ " ; " ^ (cslr_type_to_string t2)
    | TE_hypo (s, t1, t2) -> 
      "Programs in hypothesis " ^ s ^ " do not have the same type: " 
      ^ (cslr_type_to_string t1) ^ " ; " ^ (cslr_type_to_string t2)
;;

let rec fc_extract (expr : fc_term)  (pos : int list) : fc_term =
  match pos, expr with
  | [], _ -> expr
  | 0::tl, FC_if (e1, _, _) -> fc_extract e1 tl
  | 1::tl, FC_if (_, e2, _) -> fc_extract e2 tl
  | 2::tl, FC_if (_, _, e3) -> fc_extract e3 tl
  | 0::tl, FC_func (_, e) -> fc_extract e tl
  | 0::tl, FC_app (e1, _) -> fc_extract e1 tl
  | 1::tl, FC_app (_, e2) -> fc_extract e2 tl
  | 0::tl, FC_let (_, e1, _) -> fc_extract e1 tl
  | 1::tl, FC_let (_, _, e2) -> fc_extract e2 tl
  | 0::tl, FC_pair (e1, _) -> fc_extract e1 tl
  | 1::tl, FC_pair (_, e2) -> fc_extract e2 tl
  | 0::tl, FC_fst e -> fc_extract e tl
  | 0::tl, FC_snd e -> fc_extract e tl
  | 0::tl, FC_tensor (e1, _) -> fc_extract e1 tl
  | 1::tl, FC_tensor (_, e2) -> fc_extract e2 tl
  | 0::tl, FC_tslet (_, _, e1, _) -> fc_extract e1 tl
  | 1::tl, FC_tslet (_, _, _, e2) ->fc_extract e2 tl
  | 0::tl, FC_ret e -> fc_extract e tl
  | 0::tl, FC_bind (_, e1, _) -> fc_extract e1 tl
  | 1::tl, FC_bind (_, _, e2) -> fc_extract e2 tl
  | 0::tl, FC_hypo (_, _, (e1, _), _) -> fc_extract e1 tl
  | 1::tl, FC_hypo (_, _, (_, e2), _) ->fc_extract e2 tl
  | 2::tl, FC_hypo (_, _, (_, _), e3) -> fc_extract e3 tl
  | l, e -> raise (Invalid_argument "fc_extract: invalid position")
;;

let rec fc_navigate (f : fc_term -> fc_term) (pos : int list) (expr : fc_term) : fc_term =
  match pos, expr with
  | [], _ -> f expr
  | 0::tl, FC_if (e1, e2, e3) -> FC_if (fc_navigate f tl e1, e2, e3)
  | 1::tl, FC_if (e1, e2, e3) -> FC_if (e1, fc_navigate f tl e2, e3)
  | 2::tl, FC_if (e1, e2, e3) -> FC_if (e1, e2, fc_navigate f tl e3)
  | 0::tl, FC_func (x, e) -> FC_func (x, fc_navigate f tl e)
  | 0::tl, FC_app (e1, e2) -> FC_app (fc_navigate f tl e1, e2)
  | 1::tl, FC_app (e1, e2) -> FC_app (e1, fc_navigate f tl e2)
  | 0::tl, FC_let (x, e1, e2) -> FC_let (x, fc_navigate f tl e1, e2)
  | 1::tl, FC_let (x, e1, e2) -> FC_let (x, e1, fc_navigate f tl e2)
  | 0::tl, FC_pair (e1, e2) -> FC_pair (fc_navigate f tl e1, e2)
  | 1::tl, FC_pair (e1, e2) -> FC_pair (e1, fc_navigate f tl e2)
  | 0::tl, FC_fst e -> FC_fst (fc_navigate f tl e)
  | 0::tl, FC_snd e -> FC_snd (fc_navigate f tl e)
  | 0::tl, FC_tensor (e1, e2) -> FC_tensor (fc_navigate f tl e1, e2)
  | 1::tl, FC_tensor (e1, e2) -> FC_tensor (e1, fc_navigate f tl e2)
  | 0::tl, FC_tslet (x, y, e1, e2) -> FC_tslet (x, y, fc_navigate f tl e1, e2)
  | 1::tl, FC_tslet (x, y, e1, e2) -> FC_tslet (x, y, e1, fc_navigate f tl e2)
  | 0::tl, FC_ret e ->FC_ret ( fc_navigate f tl e)
  | 0::tl, FC_bind (x, e1, e2) -> FC_bind (x, fc_navigate f tl e1, e2)
  | 1::tl, FC_bind (x, e1, e2) -> FC_bind (x, e1, fc_navigate f tl e2)
  | 0::tl, FC_hypo (x, a, (e1, e2), e3) -> FC_hypo (x, a, (fc_navigate f tl e1, e2), e3)
  | 1::tl, FC_hypo (x, a, (e1, e2), e3) -> FC_hypo (x, a, (e1, fc_navigate f tl e2), e3)
  | 2::tl, FC_hypo (x, a, (e1, e2), e3) -> FC_hypo (x, a, (e1, e2), fc_navigate f tl e3)
  | l, e -> raise (Invalid_argument "extract: invalid position")
;;

let rec fc_split_function (e : fc_term) (n : int) : fc_context * fc_term  =
  if n <= 0 
  then ([], e) 
  else 
    match e with 
      | FC_func (a, e') -> 
	let (c, e0) = fc_split_function e' (n-1) in 
	let (c', s) = add_arg_to_context a c in 
	if s = "" then (c', e0) else (c', fc_rename e0 a.str s)
      | _ -> failwith "FC.Common.fc_split_function"
;;

(* This function simply transform fc_program into fc_term, without changing the structure. *)
let rec fc_program_to_fc_term (p : fc_program) : fc_term =
  match p with 
      P_var (_, s) -> FC_var s 
    | P_nil _ -> FC_nil
    | P_one _ -> FC_one 
    | P_zero _ -> FC_zero
    | P_case (_, t) -> FC_case t
    | P_srec (_, t) -> FC_srec t
    | P_if (_, (e, e1, e2)) ->
	FC_if ((fc_program_to_fc_term e), (fc_program_to_fc_term e1), (fc_program_to_fc_term e2))
    | P_func (_, (_, arg, e)) -> 
	FC_func (arg, fc_program_to_fc_term e)
    | P_app (_, (e1, e2)) -> 
	FC_app (fc_program_to_fc_term e1, fc_program_to_fc_term e2)
    | P_let (_, (s, e1, e2)) -> 
	FC_let (s, fc_program_to_fc_term e1, fc_program_to_fc_term e2)
    | P_pair (_, (e1, e2)) ->
	FC_pair (fc_program_to_fc_term e1, fc_program_to_fc_term e2)
    | P_fst (_, e) -> 
	FC_fst (fc_program_to_fc_term e)
    | P_snd (_, e) -> 
	FC_snd (fc_program_to_fc_term e)
    | P_tensor (_, (e1, e2)) -> 
	FC_tensor (fc_program_to_fc_term e1, fc_program_to_fc_term e2)
    | P_tslet (_, (s1, s2, e1, e2)) -> 
	FC_tslet (s1, s2, fc_program_to_fc_term e1, fc_program_to_fc_term e2)
    | P_rand _ -> FC_rand
    | P_ret (_, e) ->
	FC_ret (fc_program_to_fc_term e)
    | P_bind (_, (s, e1, e2)) -> 
	FC_bind (s, fc_program_to_fc_term e1, fc_program_to_fc_term e2)
    | P_hypo (_, n, (s, (e1, e2), e3))  -> 
      let (args, e1') = fc_split_function (fc_program_to_fc_term e1) n in 
      let (_, e2') = fc_split_function (fc_program_to_fc_term e2) n in 
      FC_hypo (s, args, (e1', e2'), fc_program_to_fc_term e3)
    | P_paren e ->
	fc_program_to_fc_term e
;;

let rec build_func (args : fc_context) (func_def: fc_program) : fc_program =
  match args with 
    | [] -> func_def
    | a :: args' -> 
      P_func (null_pos, (null_pos, a, build_func args' func_def))
;;

let rec fc_term_to_fc_program (e : fc_term) : fc_program = 
  match e with 
    | FC_var s -> 
      P_var (null_pos, s) 
    | FC_nil -> 
      P_nil null_pos
    | FC_one -> 
      P_one null_pos
    | FC_zero -> 
      P_zero null_pos
    | FC_case t -> 
      P_case (null_pos, t) 
    | FC_srec t -> 
      P_srec (null_pos, t)
    | FC_if (e0, e1, e2) -> 
      P_if (null_pos, (fc_term_to_fc_program e0, fc_term_to_fc_program e1, fc_term_to_fc_program e2)) 
    | FC_func (a, e) -> 
      P_func (null_pos, (null_pos, a, fc_term_to_fc_program e))
    | FC_app (e1, e2) -> 
      P_app (null_pos, (fc_term_to_fc_program e1, fc_term_to_fc_program e2)) 
    | FC_let (s, e1, e2) -> 
      P_let (null_pos, (s, fc_term_to_fc_program e1, fc_term_to_fc_program e2))
    | FC_pair (e1, e2) -> 
      P_pair (null_pos, (fc_term_to_fc_program e1, fc_term_to_fc_program e2)) 
    | FC_fst e -> 
      P_fst (null_pos, fc_term_to_fc_program e) 
    | FC_snd e -> 
      P_snd (null_pos, fc_term_to_fc_program e) 
    | FC_tensor (e1, e2) -> 
      P_tensor (null_pos, (fc_term_to_fc_program e1, fc_term_to_fc_program e2)) 
    | FC_tslet (s1, s2, e1, e2) -> 
      P_tslet (null_pos, (s1, s2, fc_term_to_fc_program e1, fc_term_to_fc_program e2)) 
    | FC_rand -> 
      P_rand null_pos 
    | FC_ret e -> 
      P_ret (null_pos, fc_term_to_fc_program e) 
    | FC_bind (s, e1, e2) -> 
      P_bind (null_pos, (s, fc_term_to_fc_program e1, fc_term_to_fc_program e2)) 
    | FC_hypo (s, args, (e1, e2), e3) -> 
      P_hypo (null_pos, List.length args,  
	      (s, (build_func args (fc_term_to_fc_program e1), build_func args (fc_term_to_fc_program e2)), fc_term_to_fc_program e3) )
;;

let string_to_fc_term (s : string) : fc_term = 
  fc_program_to_fc_term (correct_parsing (parse_string_to_fc_program s))
;;
