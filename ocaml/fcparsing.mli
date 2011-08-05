(* This module defines functions for post-parsing, including functions correcting the order of 
   lambda applications, removing parentheses, building fc_terms. 
   The function build_debruin_term is defined in the Debruijn module.
*)

open Types

val correct_parsing : fc_program -> fc_program
  (* The function corrects the pased order of consecutive lambda applications. *)
  (* Parsing by fc_program is necessary, because dealing with the order of applications we need keep parenthesis. *)

val postparsing_env : fc_parsing_result -> fc_parsing_result (* Postparsing: remove entries with duplicate name and undefined types, correct the application order of program. *)

val parse_file : string -> bool -> fc_parsing_result

val parse_string : string -> fc_parsing_result

val parse_string_to_fc_program : string -> fc_program
