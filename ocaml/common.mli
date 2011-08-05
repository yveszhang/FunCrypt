open Types

val sublist : int -> int -> 'a list -> 'a list 

(* Functions for pretty printing. *)

val aspect_to_string : aspect -> string

val cslr_type_to_string : cslr_type -> string 

val type_error_to_string : type_error -> string 

val fc_argument_to_string : fc_argument -> string 

val fc_context_to_string : fc_context -> string 

val fc_term_to_string : fc_term -> string 

val fc_term_to_pretty_string : fc_term -> string

(* Functions dealing with variables *)

val fc_free_var : fc_term -> string list (* Returns the list of free variables in a term. *)

val fc_is_free : string -> fc_term -> bool (* Testing whether a varaible occurs as free in a term. *)

val fc_rename : fc_term -> string -> string -> fc_term (* Function that replace a free variable by a new name *) 
  (* fc_rename e old_name new_name: in term e, replace (free) old_name by new_name *)

(* Functions dealing with types. *)

val error_type : cslr_type -> bool * string (* The second part of the result gives the error message. *)

val is_error_type : cslr_type -> bool

val is_closed_type : cslr_type -> bool

val unfold_type : (string * cslr_type) list -> cslr_type -> cslr_type  (* Replace all type variables by their definitions which are given in the type definitilist. *)

(* Functions dealing with typing contexts. *)

val found_var : fc_context -> string -> bool * (aspect * cslr_type) 
  (* The function searches a varaible in a context. The first component denotes the result. 
     If found, the aspect and the type will be returned. If not, (Linear, T_error) is returned. *)

val split_context : fc_context -> fc_context * fc_context 
  (* Function splits the given context into linear context and non-linear context. The first 
     context in the result is non-linear. *)

val context_max_arg : fc_context -> fc_argument (* Returns the argument with the max aspect of a context. *)

val context_leq_aspect : fc_context -> aspect -> bool 
  (* The function tests whether the largest aspect of a context is no larger than the given aspect. *)

val reduce_context : fc_context -> fc_term -> fc_context 
  (* The function reduces a context by removing all non-used variables of a given term. *)

val add_arg_to_context : fc_argument -> fc_context -> fc_context * string 
  (* The function adds a variable into a context. When there is already a variable with the same name, 
     the one being added will be renamed (by adding a "^" in front of tha name. The function returns 
     the new context and the new name. If no renaming occurs, the second part will be the empty string. *)

val union_context : fc_context -> fc_context -> fc_context

val sub_context_of_aspect : fc_context -> aspect -> fc_context 

val remove_var_from_context : fc_context -> string -> fc_context 

(* Functions about fc_term *)

val fc_program_to_fc_term : fc_program -> fc_term (* Convert fc_program to fc_term, forgetting parsing positions *)

val fc_term_to_fc_program : fc_term -> fc_program (* Parsing positions are filled with null_pos. Mainly for typing *)

val string_to_fc_term : string -> fc_term (* Parsing a string to fc_term *)

val fc_extract : fc_term -> int list -> fc_term

val fc_navigate : (fc_term -> fc_term) -> int list -> fc_term -> fc_term
