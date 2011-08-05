open Types 

val beta_reduce : db_term -> db_term 

val db_reduce : db_term -> db_term

val db_reduce_multi : db_term -> int -> db_term

val db_evaluate : db_term -> bool -> db_term

(* val db_reduce_deep : db_term -> db_term *)

(* val db_evaluate_deep : db_term -> bool -> db_term *)

val beta_reduce_at :  db_term -> int list -> db_term 

val db_reduce_at :  db_term -> int list -> db_term 

val db_normalize : db_term -> bool -> db_term 

val db_normalize_one : db_term -> db_term 

val db_normalize_at : db_term -> int list -> bool -> db_term 
