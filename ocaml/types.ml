exception FC_parse_error of string
exception FC_error of string

type aspect = Linear | Modal | Arrow

let asp_leq (a1: aspect) (a2: aspect) = match a1 with 
  | Linear -> if a2 = Linear then true else false
  | Arrow -> if a2 = Modal then false else true 
  | Modal -> true
;;

type cslr_type = 
  | T_var of string
  | T_bits
  | T_prod of (cslr_type * cslr_type)
  | T_tensor of (cslr_type * cslr_type)
  | T_func of (cslr_type * aspect * cslr_type)
  | T_proba of cslr_type
  | T_error of string (* Type error with error information *)

(* A node for storing argument information of functions or an element of typing context *)  
type fc_argument = {str: string; asp: aspect; typ: cslr_type} 

type fc_context = fc_argument list
(*
  | Empty_context
  | Ctxt of (fc_argument * fc_context * fc_context)
*)

(* Every position is a pair of the line number and colum number *)
type fc_position = {start_pos : int * int; end_pos : int * int} 

let null_pos = {start_pos = (-1, -1); end_pos = (-1, -1)} ;; 

type fc_program = 
  | P_var of (fc_position * string) 
  | P_nil of fc_position 
  | P_one of fc_position 
  | P_zero of fc_position 
  | P_case of (fc_position * cslr_type)
  | P_srec of (fc_position * cslr_type) 
  | P_if of (fc_position * (fc_program * fc_program * fc_program))
  | P_func of (fc_position * (fc_position * fc_argument * fc_program)) (* the second position is that of the argument *)
  | P_app of (fc_position * (fc_program * fc_program))
  | P_let of (fc_position * (string * fc_program * fc_program))
  | P_pair of (fc_position * (fc_program * fc_program))
  | P_fst of (fc_position * fc_program) 
  | P_snd of (fc_position * fc_program)
  | P_tensor of (fc_position * (fc_program * fc_program))
  | P_tslet of (fc_position * (string * string * fc_program * fc_program))
  | P_rand of fc_position 
  | P_ret of (fc_position * fc_program)
  | P_bind of (fc_position * (string * fc_program * fc_program))
  | P_hypo of (fc_position * int * (string * (fc_program * fc_program) * fc_program))
  | P_paren of fc_program

let rec get_prog_pos (p : fc_program)  : fc_position = 
  match p with 
    | P_var (pos, _) | P_nil pos | P_one pos | P_zero pos | P_case (pos, _) | P_srec (pos, _) 
    | P_if (pos, _) | P_func (pos, _) | P_app (pos, _) | P_let (pos, _) 
    | P_pair (pos, _) | P_fst (pos, _) | P_snd (pos, _) 
    | P_tensor (pos, _) | P_tslet (pos, _) 
    | P_rand pos | P_ret (pos, _) | P_bind (pos, _) | P_hypo (pos, _, _) -> 
      pos
    | P_paren p' -> get_prog_pos p'
;;

let position_to_string (p : fc_position) : string = 
  let (l1, c1) = p.start_pos in 
  let (l2, c2) = p.end_pos in 
  if l1 = l2 
  then 
    "[ Line " ^ (string_of_int l1) ^ "(" ^ (string_of_int c1) 
    ^ (if c1 = c2 then "" else " -- " ^ (string_of_int c2) ) ^ ") ]"
  else 
    "[ Line " ^ (string_of_int l1) ^ "(" ^ (string_of_int c1) 
    ^ ") -- Line " ^ (string_of_int l2) ^ "(" ^ (string_of_int c2) ^ ") ]"
;;

type fc_term = 
  | FC_var of string
  | FC_nil 
  | FC_one 
  | FC_zero
  | FC_case of cslr_type
  | FC_srec of cslr_type 
  | FC_if of (fc_term * fc_term * fc_term)
  | FC_func of (fc_argument * fc_term)
  | FC_app of (fc_term * fc_term)
  | FC_let of (string * fc_term * fc_term)
  | FC_pair of (fc_term * fc_term)
  | FC_fst of fc_term 
  | FC_snd of fc_term
  | FC_tensor of (fc_term * fc_term)
  | FC_tslet of (string * string * fc_term * fc_term)
  | FC_rand 
  | FC_ret of fc_term
  | FC_bind of (string * fc_term * fc_term)
  | FC_hypo of (string * fc_context *(fc_term * fc_term) * fc_term)

type db_term =
  | DB_var of string
  | DB_idx of int
  | DB_nil 
  | DB_zero
  | DB_one 
  | DB_case of cslr_type
  | DB_srec of cslr_type 
  | DB_if of (db_term * db_term * db_term)
  | DB_func of (fc_argument * db_term)
      (* The field str of fc_argument is kept for pretty-printing purpose only. *)
      (* It is ignored when testing for equality de de Bruijn terms.            *)
  | DB_app of (db_term * db_term)
  | DB_let of (string * db_term * db_term)
  | DB_pair of (db_term * db_term)
  | DB_fst of db_term 
  | DB_snd of db_term
  | DB_tensor of (db_term * db_term)
  | DB_tslet of (string * string * db_term * db_term)
  | DB_rand 
  | DB_ret of db_term
  | DB_bind of (string * db_term * db_term)
  | DB_hypo of (string * fc_context *(db_term * db_term) * db_term) 

type parsing_entry = {e_pos: fc_position; e_name: string; e_asp: aspect; e_typ: cslr_type; e_prog: fc_program}

let default_entry = {e_pos = null_pos; e_name = ""; e_asp = Linear; e_typ = T_error ""; e_prog = P_var (null_pos, "")}

(* 1st list: type definitions; 2nd: varaible declarations; 3rd: program definition; 4th: warning messages.
   The line number (in the original file) of every definition is kept. *)
type fc_parsing_result = {
  typedef: parsing_entry list; 
  vardef:  parsing_entry list; 
  progdef: parsing_entry list;
  hypodef: parsing_entry list ;
  warnings: string list
}

(* type fc_prog_def = {p_name : string; p_def : db_term; p_asp: aspect; p_typ : cslr_type} *)

type fc_hypo_def = {h_name : string; h_left : db_term; h_right : db_term; h_typ : cslr_type}

type fc_environment = {
  types : (string * cslr_type) list;
  vars : fc_argument list; 
  hypos: fc_hypo_def list;
  prog : db_term;
}

let empty_environment = {types=[]; vars=[]; prog=DB_idx (-1); hypos=[]}
;;

type type_error = 
  | TE_var of string (* Variable is not found in the typing context *)
  | TE_non_defined of cslr_type (* Type contains non-defined type varaibles *)
  | TE_if_cond of cslr_type (* The IF condition is not of type bits *)
  | TE_if_branch of (cslr_type * cslr_type) (* The two branches of IF are not of the same type *)
  | TE_func of fc_argument (* No aspect can be assigned to the function argument *)
  | TE_aspect of (fc_argument list) (* The argument term contains variables with too high aspect. *)
  | TE_app of (cslr_type * cslr_type) (* The types of applications do not match *)
  | TE_proj of cslr_type (* The projection is applied to a non-product *)
  | TE_tensor of cslr_type (* The tensor projection is applied to a non-tensor product *)
  | TE_tsaspect of string (* tslet bound variables are used non-linearly *)
  | TE_bind of (cslr_type * cslr_type) (* Computations in the bind are not proba types *) 
  | TE_hypo of (string * cslr_type * cslr_type) (* Types of hypothesis programs do not match *)
