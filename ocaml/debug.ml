(* This file includes functions for the use of debugging, especially functions that 
   convert internal structure into strings.
*)
open Printf
open Types
open Common
open Debruijn

let _debug = true
;;

let print_indent (n : int) : unit = 
  for i = 1 to n do print_string "|  " done
;;
(*
let rec print_fc_program (p: fc_program) (n: int) : unit =
  match p with 
      P_var s -> 
	print_indent n; print_string ("ID_"^s); 
    | P_nil -> 
	print_indent n; print_string "NIL"; 
    | P_one -> 
	print_indent n; print_string "B1"; 
    | P_zero -> 
	print_indent n; print_string "B0"; 
    | P_case t -> 
	print_indent n; print_string ("CASE ("^(cslr_type_to_string t)^")"); 
    | P_srec t ->
	print_indent n; print_string ("SREC ("^(cslr_type_to_string t)^")"); 
    | P_if (e, e1, e2) ->
	print_indent n; print_endline "IF"; 
	print_fc_program e (n+1); print_endline "" ;
	print_fc_program e1 (n+1); print_endline "" ;
	print_fc_program e2 (n+1); 
    | P_func (arg, e) -> 
	print_indent n; print_string "FUNC"; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^arg.str); print_endline "";
	print_fc_program e (n+1); 
    | P_app (e1, e2) -> 
	print_indent n; print_string "APP"; print_endline "";
	print_fc_program e1 (n+1); print_endline "";
	print_fc_program e2 (n+1); 
    | P_let (s, e1, e2) -> 
	print_indent n; print_string "LET" ; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^s) ; print_endline "";
	print_fc_program e1 (n+1); print_endline "" ;
	print_fc_program e2 (n+1); 
    | P_pair (e1, e2) -> 
	print_indent n; print_string "PAIR"; print_endline "" ;
	print_fc_program e1 (n+1); print_endline ""; 
	print_fc_program e2 (n+1); 
    | P_fst e ->
	print_indent n; print_string "FIRST"; print_endline "" ;
	print_fc_program e (n+1); 
    | P_snd e ->
	print_indent n; print_string "SECOND"; print_endline "" ;
	print_fc_program e (n+1); 
     | P_tensor (e1, e2) -> 
	print_indent n; print_string "TENSOR"; print_endline "" ;
	print_fc_program e1 (n+1); print_endline ""; 
	print_fc_program e2 (n+1); 
    | P_tslet (s1, s2, e1, e2) -> 
	print_indent n; print_string "TSLET" ; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^s1) ; print_endline "";
	print_indent (n+1); print_string ("ID_"^s2) ; print_endline "";
	print_fc_program e1 (n+1); print_endline "" ;
	print_fc_program e2 (n+1); 
    | P_rand -> 
	print_indent n; print_string "RAND"; 
    | P_ret e -> 
	print_indent n; print_string "RETURN"; print_endline "";
	print_fc_program e (n+1); 
    | P_bind (x, e1, e2) -> 
	print_indent n; print_string "BIND" ; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^x) ; print_endline "";
	print_fc_program e1 (n+1); print_endline "" ;
	print_fc_program e2 (n+1); 
    | P_hypo (x, (e1, e2), e3) ->
	print_indent n; print_string "LETHYPO" ; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^x) ; print_endline "";
	print_fc_program e1 (n+1); print_endline "" ;
	print_fc_program e2 (n+1); print_endline "" ;
	print_fc_program e3 (n+1); 
    | P_paren e -> 
	print_indent n; print_string "PAREN"; print_endline "";
	print_fc_program e (n+1); 
;;

let rec print_fc_term (tm: fc_term) (n: int) : unit =
  match tm with 
    | FC_var s -> 
	print_string s; 
    | FC_nil -> 
	print_string "nil"; 
    | FC_one -> 
	print_string "B1"; 
    | FC_zero -> 
	print_string "B0"; 
    | FC_case t -> 
	print_indent n; print_string ("CASE ("^(cslr_type_to_string t)^")"); 
    | FC_srec t ->
	print_indent n; print_string ("SREC ("^(cslr_type_to_string t)^")"); 
    | FC_if (e, e1, e2) ->
	print_indent n; print_string "IF"; print_endline "" ;
	print_fc_term e (n+1); print_endline "" ;
	print_fc_term e1 (n+1); print_endline "" ;
	print_fc_term e2 (n+1); 
    | FC_func (arg, e) -> 
	print_indent n; print_string "FUNC"; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^arg.str); print_endline "";
	print_fc_term e (n+1); 
    | FC_app (e1, e2) -> 
	print_indent n; print_string "APP"; print_endline "" ;
	print_fc_term e1 (n+1); print_endline ""; 
	print_fc_term e2 (n+1); 
    | FC_let (s, e1, e2) -> 
	print_indent n; print_string "LET" ; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^s) ; print_endline "";
	print_fc_term e1 (n+1); print_endline "" ;
	print_fc_term e2 (n+1); 
    | FC_pair (e1, e2) -> 
	print_indent n; print_string "PAIR"; print_endline "" ;
	print_fc_term e1 (n+1); print_endline ""; 
	print_fc_term e2 (n+1); 
    | FC_fst e ->
	print_indent n; print_string "FIRST"; print_endline "" ;
	print_fc_term e (n+1); 
    | FC_snd e ->
	print_indent n; print_string "SECOND"; print_endline "" ;
	print_fc_term e (n+1); 
     | FC_tensor (e1, e2) -> 
	print_indent n; print_string "TENSOR"; print_endline "" ;
	print_fc_term e1 (n+1); print_endline ""; 
	print_fc_term e2 (n+1); 
    | FC_tslet (s1, s2, e1, e2) -> 
	print_indent n; print_string "TSLET" ; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^s1) ; print_endline "";
	print_indent (n+1); print_string ("ID_"^s2) ; print_endline "";
	print_fc_term e1 (n+1); print_endline "" ;
	print_fc_term e2 (n+1); 
    | FC_rand -> 
	print_indent n; print_string "RAND"; 
    | FC_ret e -> 
	print_indent n; print_string "RETURN"; print_endline "";
	print_fc_term e (n+1); 
    | FC_bind (x, e1, e2) -> 
	print_indent n; print_string "BIND" ; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^x) ; print_endline "";
	print_fc_term e1 (n+1); print_endline "" ;
	print_fc_term e2 (n+1); 
    | FC_hypo (x, (e1, e2), e3) -> 
	print_indent n; print_string "LETHYPO" ; print_endline "" ;
	print_indent (n+1); print_string ("ID_"^x) ; print_endline "";
	print_fc_term e1 (n+1); print_endline "" ;
	print_fc_term e2 (n+1); print_endline "" ;
	print_fc_term e3 (n+1); 
;;
*)

let print_context (ctx : fc_argument list) : unit = 
  let _ = List.iter (fun x -> print_string ((fc_argument_to_string x) ^ ";  "))  ctx in 
  print_endline ""
;;

let print_string_list_per_line (l: string list) : unit = 
  List.iter print_endline l 
;;

let print_string_list_in_line (l : string list) : unit = 
  print_string "[ " ;
  List.iter (fun x -> print_string (x ^ "; ")) l ;
  print_endline " ]"
;;

let print_fc_env (env : fc_environment) : unit =
  let sep = "**************************************************" in 
  print_endline (sep ^ "Type definitions:") ;
  List.iter 
    (fun x -> print_endline ("  " ^ (fst x) ^ " : " ^ (cslr_type_to_string (snd x))) )
    env.types ;
  print_endline (sep ^ "Variable (constant) declarations:") ;
  List.iter
    (fun x -> print_endline ("  " ^ (fc_argument_to_string x)) )
    env.vars ;
  print_endline (sep ^ "Hypothesis:");
  List.iter 
    (fun x -> 
      print_endline ("  " ^ x.h_name ^ ":") ; 
      print_endline ("  " ^ (fc_term_to_string (db_term_to_fc_term x.h_left)) ) ;
      print_endline "==" ;
      print_endline ("  " ^ (fc_term_to_string (db_term_to_fc_term x.h_right)) ) 
    ) 
    env.hypos; 
  print_endline (sep ^ "The program to be transformed:") ;
  print_endline ( "  " ^ (fc_term_to_string (db_term_to_fc_term env.prog)))
;;
