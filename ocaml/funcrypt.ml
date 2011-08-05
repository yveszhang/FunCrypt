open List
open Types (* All defintion of funcrypt types *)
open Fcparsing (* About parsing *)
open Common (* About fc_term and fc_context *)
open Debruijn (* About db_term *)
open Typing (* About cslr type checking *)
open Environment (* About fc_environment *)
open Betareduce (* About reduction and program equivalence *) 
open Bind (* About transformation of programs using binders (bind and let) *)
open Debug 
open Testtactics

type action = Parse | Typing | Coq | Test | ZYtest | Debruijn | Usage | Verbose | Progstr

type argument = 
    Opt of action 
  | File of string (* The name of the file to be parsed, containing program definitions. *)
  | Prog of string (* The program definition provided by the command line as an argument. *)

let usage () = 
  print_endline "funcrypt [option] <program>" ;
  print_endline "Options:";
  print_endline "\t -typing: type checking";
  print_endline "\t -coq: convert to coq files";
  print_endline "\t -debruijn: show De Bruijn structure";
  print_endline "\t -v: verbose mode";
  print_endline "\t -prog <program-def>: parse the given program (must be quoted)"
;;

let parse_arg (s : string) : argument = 
  if s.[0] = '-'
  then 
    match s with 
    | "-parse" -> Opt Parse
    | "-coq" -> Opt Coq
    | "-test" -> Opt Test
    | "-zytest" -> Opt ZYtest
    | "-debruijn" -> Opt Debruijn
    | "-typing" -> Opt Typing
    | "-v" -> Opt Verbose
    | "-prog" -> Opt Progstr
    | _ -> Opt Usage
  else 
    File s
;;

let arglist : argument list = 
  if (Array.length Sys.argv < 2)
  then 
    [Opt Usage]
  else 
    List.map parse_arg (tl (Array.to_list Sys.argv))
;;

let opt_file_list : (action list) * (argument list)  = 
  let rec f (l : argument list) = 
    match l with 
	[] -> ([], [File ""])
      | (Opt Progstr) :: (File s) :: l' -> 
	let (l1, l2) = (f l') in (l1, (Prog s) :: l2)
      | (Opt a) :: l' -> 
	let (l1, l2) = (f l') in (a :: l1, l2)
      | x :: l' -> 
	let (l1, l2) = (f l') in (l1, x :: l2)
  in 
  f arglist
;;

let option : action = 
  let rec f (l : action list)  = 
    match l with 
	[] -> Usage 
      | a :: l' -> if (a <> Verbose) then a else (f l')
  in f (fst opt_file_list)
;;

let print_typing_error (pos : fc_position) (fr : typing_frame) (err : type_error) (verbose : bool) : unit = 
  print_endline ("Typing error " ^ (position_to_string pos) ^ ":") ;
  print_endline (type_error_to_string err) ;
  if verbose 
  then print_endline (typing_frame_to_string fr) 
  else ()
;;

let main () : unit = 
  let verbose = mem Verbose (fst opt_file_list) in 
  if option = Usage 
  then usage ()
  else 
    try 
      let prog_arg = hd (snd opt_file_list) in 
      let parse_tab = match prog_arg with 
	  File s -> parse_file s verbose
	| Prog s -> parse_string s 
	| _ -> {typedef=[]; vardef=[]; progdef=[]; hypodef=[]; warnings=[]}
      in
      match option with
	| Test -> test ()
	| ZYtest -> 
	  let e1 = string_to_db_term 
	    "bind a = X in return mult ( a )  ( Y )" in 
	  let e2 = string_to_db_term 
	    "bind x = carrier in return (mult ( x ) ( if b then second (first (m0_m1_A2)) else first (first (m0_m1_A2)) ))" in 
	  List.iter 
	    (fun x -> print_string ((fst x) ^ " : "); print_endline (db_term_to_pretty_string (snd x))) 
	    (db_pattern e1 e2 ["X"; "Y"])
	  (* let env = build_fc_environment parse_tab false in  *)
	  (* let print_db_term = fun x -> print_endline (db_term_to_pretty_string  x) in  *)
	  (* let e = (db_normalize_at env.prog (db_position "keygen" env.prog) verbose) in  *)
	  (* (print_db_term env.prog ; print_db_term e)*)
	| Typing -> 
	  let typed_parse_tab = typing_parsing_result parse_tab verbose in 
	  let print_name_asp_type = 
	    fun x -> 
	      let s = cslr_type_to_string x.e_typ in 
	      let s' =  match x.e_asp with 
		| Linear -> "< " ^ s ^ " >" | Modal -> "[ " ^ s ^ " ]" | Arrow -> s 
	      in print_endline (x.e_name ^ " : " ^ s') 
	  in
	  List.iter print_name_asp_type (typed_parse_tab.progdef @ typed_parse_tab.hypodef) ;
	  print_endline "\nTyping OK!"
	| Debruijn ->
	  let env = build_fc_environment parse_tab false in 
	  print_endline (db_term_to_rawstring env.prog) ;
      	  print_endline (db_term_to_rawstring (beta_reduce_at env.prog [0; 0; 0; 0]))
	| Parse ->
	  let env = build_fc_environment parse_tab verbose in 
      	  let print_info (e : fc_term) =
      	    print_endline ("    " ^ (fc_term_to_pretty_string  e)) ;  
	    print_string "    !!! The program has free variables: "; 
	    print_string_list_in_line (fc_free_var e)
      	  in
	  print_endline "Predfined types:"; 
	  List.iter 
	    (fun x -> print_endline ("  " ^ (fst x) ^ " = " ^ (cslr_type_to_string (snd x))))
	    env.types ;
	  print_endline "\nPredefined variables:" ;
	  List.iter 
	    (fun x -> print_endline ("  " ^ (fc_argument_to_string x)))
	    env.vars;
	  print_endline "\nPredefined hypotheses:";
	  List.iter 
	    (fun x -> 
	      print_endline ("  " ^ x.h_name ^ " :") ; 
	      print_endline ("    " ^ (db_term_to_pretty_string x.h_left)) ;
	      print_endline "    ==" ;
	      print_endline ("    " ^ (db_term_to_pretty_string x.h_right)) ; 
	      print_endline ("    " ^ (cslr_type_to_string x.h_typ)) ;
	    )
	    env.hypos ;
	  (match env.prog with 
	    | DB_idx _ -> print_endline "\nThere is no program defined!"
	    | _ -> print_endline "\nThe final program:" ; print_info (db_term_to_fc_term env.prog)
	  ) ;
	  print_endline "\nParsing OK!"
    with FC_type_error (pos, st, err) -> 
      print_typing_error pos st err verbose ; print_endline ""
;;

main () ;;
