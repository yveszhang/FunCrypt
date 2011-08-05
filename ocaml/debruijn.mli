open Types

val db_term_to_rawstring : db_term -> string (* For debugging purpose only *)

val db_term_to_fc_term : db_term -> fc_term

val db_term_to_fc_term_ignore : db_term -> fc_term 

val fc_term_to_db_term : fc_term -> db_term

val string_to_db_term : string -> db_term 

val db_term_to_string : db_term -> string 

val db_term_to_pretty_string : db_term -> string

val db_term_to_fc_program : db_term -> fc_program 

val db_term_to_fc_program_ignore : db_term -> fc_program 

val fc_program_to_db_term : fc_program -> db_term 

val db_navigate : db_term -> int list -> (db_term -> db_term) -> db_term

val db_extract : db_term -> int list -> db_term

(* val increase_db_index : db_term -> int -> db_term *)

(* val reindex_var : db_term -> int -> int -> db_term  *)

val db_alpha_equal : db_term -> db_term -> bool

(* val db_structure_rec : db_term -> (int -> 'a) -> (db_term -> 'a) -> (db_term -> 'a) -> 'a *)

val db_lift : db_term -> db_term (* Increment all NON-LOCAL index by 1 *)

val db_lift_by : db_term -> int -> db_term (* Increment all non-local index by n *)

val db_lift_rec : db_term -> int -> int -> db_term 
  (* db_lift_rec (e, n, lb): Lift RECURSIVELY all the global indexes (by n) that are no smaller than lb *)

val db_rewrite : db_term -> db_term -> db_term -> db_term 
  (* db_rewrite e e1 e2: replace, in e, all subterm e1 by term e2 *)

val db_rewrite_hypo : db_term -> db_term -> db_term -> db_term 

val db_max_index : db_term -> int 

val db_index_is_free : db_term -> int -> bool

val db_position : string -> db_term -> int list

val db_pattern : db_term -> db_term -> string list -> (string * db_term) list
