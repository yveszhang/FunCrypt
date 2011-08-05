open List 
open Common
open Types

type db_error = 
  | Invalid_pos of (int list) 
  | Invalid_index of int

exception DB_error of db_error

let rec index x l =
  match l with
  | [] -> raise Not_found
  | y :: l' -> if x = y then 0 else 1 + index x l'

let rec make_spaces n =
  match n with
  | 0 -> ""
  | _ -> " " ^ make_spaces (n-1)

(* For debugging purpose only *)
let rec db_term_to_rawstring_rec (indent:int)(e : db_term) : string =
  match e with
  | DB_var s -> s
  | DB_idx i -> string_of_int i
  | DB_nil -> "nil"
  | DB_one -> "B1"
  | DB_zero -> "B0"
  | DB_case t -> "case " ^ cslr_type_to_string t
  | DB_srec t -> "srec " ^ cslr_type_to_string t
  | DB_if (e0, e1, e2) -> "if " ^ db_term_to_rawstring_rec indent e0 ^
                          " then " ^ db_term_to_rawstring_rec indent e1 ^
                          " else " ^ db_term_to_rawstring_rec indent e2
  | DB_func (x, e0) -> "func " ^ x.str ^ " -> " ^ db_term_to_rawstring_rec indent e0
  | DB_app (e1, e2) -> "(" ^ db_term_to_rawstring_rec indent e1 ^ ") " ^ db_term_to_rawstring_rec indent e2
  | DB_let (x, e1, e2) ->
      make_spaces indent ^ "let " ^ x ^ " =\n" ^ make_spaces (indent+1) ^ db_term_to_rawstring_rec (indent+1) e1 ^ " in\n" ^ db_term_to_rawstring_rec indent e2
  | DB_pair (e1, e2) ->
      "<" ^ db_term_to_rawstring_rec indent e1 ^ ", " ^ db_term_to_rawstring_rec indent e2 ^ ">"
  | DB_fst e0 -> "first " ^ db_term_to_rawstring_rec indent e0
  | DB_snd e0 -> "second " ^ db_term_to_rawstring_rec indent e0
  | DB_tensor (e1, e2) ->
      "<" ^ db_term_to_rawstring_rec indent e1 ^ " * " ^ db_term_to_rawstring_rec indent e2 ^ ">"
  | DB_tslet (x, y, e1, e2) ->
      make_spaces indent ^ "let (" ^ x ^ ", " ^ y ^ ") =\n" ^ make_spaces (indent+1) ^ db_term_to_rawstring_rec (indent+1) e1 ^ " in\n" ^ db_term_to_rawstring_rec indent e2
  | DB_rand -> "rand"
  | DB_ret e0 -> "return " ^ db_term_to_rawstring_rec indent e0
  | DB_bind (x, e1, e2) ->
      make_spaces indent ^ "bind " ^ x ^ " =\n" ^ make_spaces (indent+1) ^ db_term_to_rawstring_rec (indent+1) e1 ^ " in\n" ^ db_term_to_rawstring_rec indent e2
  | DB_hypo (h, args, (e1, e2), e3) ->
      make_spaces indent ^ "lethypo " ^ h ^ "( " ^ (fc_context_to_string args) ^ " ) =\n" 
    ^ make_spaces (indent+1) ^ db_term_to_rawstring_rec (indent+1) e1 ^ "\n==\n" 
    ^ make_spaces (indent+1) ^ db_term_to_rawstring_rec (indent+1) e2 ^ " in\n" 
    ^ db_term_to_rawstring_rec indent e3

let db_term_to_rawstring (e : db_term) : string =
  db_term_to_rawstring_rec 0 e

let rec db_term_to_fc_term_rec (context:string list) (e : db_term) (ignore : bool) : fc_term =
  match e with
  | DB_var s -> FC_var s
  | DB_idx n -> (
      try
        FC_var (nth context n)
      with
      |  _ -> 
	if ignore 
	then 
	  FC_var ("#DB_" ^ (string_of_int n))
	else
	  failwith ("db_term_to_fc_term: invalid De Bruijn index: " ^ (string_of_int n) ^ "\n" 	
		    ^ (db_term_to_rawstring e))
    )
  | DB_nil -> FC_nil
  | DB_zero -> FC_zero
  | DB_one -> FC_one
  | DB_case t -> FC_case t
  | DB_srec t -> FC_srec t
  | DB_if (e1, e2, e3) ->
      FC_if (db_term_to_fc_term_rec context e1 ignore, db_term_to_fc_term_rec context e2 ignore,
             db_term_to_fc_term_rec context e3 ignore)
  | DB_func (x, e1) -> FC_func (x, db_term_to_fc_term_rec (x.str :: context) e1 ignore)
  | DB_app (e1, e2) ->
      FC_app (db_term_to_fc_term_rec context e1 ignore, db_term_to_fc_term_rec context e2 ignore)
  | DB_let (x, e1, e2) ->
      FC_let (x, db_term_to_fc_term_rec context e1 ignore, db_term_to_fc_term_rec (x :: context) e2 ignore)
  | DB_pair (e1, e2) ->
      FC_pair (db_term_to_fc_term_rec context e1 ignore, db_term_to_fc_term_rec context e2 ignore)
  | DB_fst e1 -> FC_fst (db_term_to_fc_term_rec context e1 ignore)
  | DB_snd e1 -> FC_snd (db_term_to_fc_term_rec context e1 ignore)
  | DB_tensor (e1, e2) ->
      FC_tensor (db_term_to_fc_term_rec context e1 ignore, db_term_to_fc_term_rec context e2 ignore)
  | DB_tslet (x, y, e1, e2) ->
      FC_tslet  (x, y, db_term_to_fc_term_rec context e1 ignore, db_term_to_fc_term_rec (y :: x :: context) e2 ignore)
  | DB_rand -> FC_rand
  | DB_ret e1 -> FC_ret (db_term_to_fc_term_rec context e1 ignore)
  | DB_bind (x, e1, e2) ->
      FC_bind (x, db_term_to_fc_term_rec context e1 ignore, db_term_to_fc_term_rec (x :: context) e2 ignore)
  | DB_hypo (h, args, (e1, e2), e3) ->
      FC_hypo (h, args, (db_term_to_fc_term_rec context e1 ignore, db_term_to_fc_term_rec context e2 ignore), 
	       db_term_to_fc_term_rec context e3 ignore)

let db_term_to_fc_term (e : db_term) : fc_term =
  db_term_to_fc_term_rec [] e false

let db_term_to_fc_term_ignore (e : db_term) : fc_term = 
  db_term_to_fc_term_rec [] e true

let rec fc_term_to_db_term_rec (context : string list)(p : fc_term) : db_term =
  match p with
  | FC_var s -> (
      try
        DB_idx (index s context)
      with
      | Not_found -> DB_var s
    )
  | FC_nil -> DB_nil
  | FC_one -> DB_one
  | FC_zero -> DB_zero
  | FC_case t -> DB_case t
  | FC_srec t -> DB_srec t
  | FC_if (e0, e1, e2) -> 
      DB_if (fc_term_to_db_term_rec context e0, fc_term_to_db_term_rec context e1, fc_term_to_db_term_rec context e2)
  | FC_func (arg, e0) -> 
      DB_func (arg,  fc_term_to_db_term_rec (arg.str :: context) e0)
  | FC_app (e1, e2) -> 
      DB_app (fc_term_to_db_term_rec context e1, fc_term_to_db_term_rec context e2)
  | FC_let (s, e1, e2) -> 
      DB_let (s, fc_term_to_db_term_rec context e1, fc_term_to_db_term_rec (s :: context) e2)
  | FC_pair (e1, e2) -> 
      DB_pair (fc_term_to_db_term_rec context e1, fc_term_to_db_term_rec context e2)
  | FC_fst e0 -> 
      DB_fst (fc_term_to_db_term_rec context e0)
  | FC_snd e0 -> 
      DB_snd (fc_term_to_db_term_rec context e0)
  | FC_tensor (e1, e2) ->
      DB_tensor (fc_term_to_db_term_rec context e1, fc_term_to_db_term_rec context e2)
  | FC_tslet (s1, s2, e0, e') -> 
      DB_tslet (s1, s2, fc_term_to_db_term_rec context e0, fc_term_to_db_term_rec (s2 :: s1 :: context) e')
  | FC_rand -> DB_rand
  | FC_ret e0 -> 
      DB_ret (fc_term_to_db_term_rec context e0)
  | FC_bind (x, e1, e2) -> 
      DB_bind (x, fc_term_to_db_term_rec context e1, fc_term_to_db_term_rec (x :: context) e2)
  | FC_hypo (h, args, (e1, e2), e3) ->
      DB_hypo (h, args, (fc_term_to_db_term_rec context e1, fc_term_to_db_term_rec context e2), fc_term_to_db_term_rec context e3)

let fc_term_to_db_term (p : fc_term) : db_term =
  fc_term_to_db_term_rec [] p

let string_to_db_term (s : string) : db_term =
  fc_term_to_db_term (string_to_fc_term s)

let db_term_to_string (e : db_term) : string = 
  (fc_term_to_string (db_term_to_fc_term_ignore e))

let db_term_to_pretty_string (e : db_term) : string =
  (fc_term_to_pretty_string (db_term_to_fc_term_ignore e))

let db_term_to_fc_program (e : db_term) : fc_program = 
  fc_term_to_fc_program (db_term_to_fc_term e)

let db_term_to_fc_program_ignore (e : db_term) : fc_program = 
  fc_term_to_fc_program (db_term_to_fc_term_ignore e)

let fc_program_to_db_term (p : fc_program) : db_term =
  fc_term_to_db_term (fc_program_to_fc_term p)

let rec list_prefix (l : 'a list) (n : int) : 'a list = 
  if n <= 0 then [] 
  else 
    match l with 
      | [] -> []
      | x :: l' -> x :: list_prefix l' (n-1) 


let rec navigate f pos expr =
  match pos, expr with
  | [], _ -> f expr
  | 0::tl, DB_if (e1, e2, e3) -> DB_if (navigate f tl e1, e2, e3)
  | 1::tl, DB_if (e1, e2, e3) -> DB_if (e1, navigate f tl e2, e3)
  | 2::tl, DB_if (e1, e2, e3) -> DB_if (e1, e2, navigate f tl e3)
  | 0::tl, DB_func (x, e) -> DB_func (x, navigate f tl e)
  | 0::tl, DB_app (e1, e2) -> DB_app (navigate f tl e1, e2)
  | 1::tl, DB_app (e1, e2) -> DB_app (e1, navigate f tl e2)
  | 0::tl, DB_let (x, e1, e2) -> DB_let (x, navigate f tl e1, e2)
  | 1::tl, DB_let (x, e1, e2) -> DB_let (x, e1, navigate f tl e2)
  | 0::tl, DB_pair (e1, e2) -> DB_pair (navigate f tl e1, e2)
  | 1::tl, DB_pair (e1, e2) -> DB_pair (e1, navigate f tl e2)
  | 0::tl, DB_fst e -> DB_fst (navigate f tl e)
  | 0::tl, DB_snd e -> DB_snd (navigate f tl e)
  | 0::tl, DB_tensor (e1, e2) -> DB_tensor (navigate f tl e1, e2)
  | 1::tl, DB_tensor (e1, e2) -> DB_tensor (e1, navigate f tl e2)
  | 0::tl, DB_tslet (x, y, e1, e2) -> DB_tslet (x, y, navigate f tl e1, e2)
  | 1::tl, DB_tslet (x, y, e1, e2) -> DB_tslet (x, y, e1, navigate f tl e2)
  | 0::tl, DB_ret e ->DB_ret ( navigate f tl e)
  | 0::tl, DB_bind (x, e1, e2) -> DB_bind (x, navigate f tl e1, e2)
  | 1::tl, DB_bind (x, e1, e2) -> DB_bind (x, e1, navigate f tl e2)
  | 0::tl, DB_hypo (x, a, (e1, e2), e3) -> DB_hypo (x, a, (navigate f tl e1, e2), e3)
  | 1::tl, DB_hypo (x, a, (e1, e2), e3) -> DB_hypo (x, a, (e1, navigate f tl e2), e3)
  | 2::tl, DB_hypo (x, a, (e1, e2), e3) -> DB_hypo (x, a, (e1, e2), navigate f tl e3)
  | l, e -> raise (DB_error (Invalid_pos (list_prefix pos ((length pos) - (length l)))))
;;

let db_navigate (e : db_term) (pos : int list) (f : db_term -> db_term) : db_term = 
  navigate f pos e

let rec db_extract (expr : db_term)  (pos : int list) : db_term =
  match pos, expr with
  | [], _ -> expr
  | 0::tl, DB_if (e1, _, _) -> db_extract e1 tl
  | 1::tl, DB_if (_, e2, _) -> db_extract e2 tl
  | 2::tl, DB_if (_, _, e3) -> db_extract e3 tl
  | 0::tl, DB_func (_, e) -> db_extract e tl
  | 0::tl, DB_app (e1, _) -> db_extract e1 tl
  | 1::tl, DB_app (_, e2) -> db_extract e2 tl
  | 0::tl, DB_let (_, e1, _) -> db_extract e1 tl
  | 1::tl, DB_let (_, _, e2) -> db_extract e2 tl
  | 0::tl, DB_pair (e1, _) -> db_extract e1 tl
  | 1::tl, DB_pair (_, e2) -> db_extract e2 tl
  | 0::tl, DB_fst e -> db_extract e tl
  | 0::tl, DB_snd e -> db_extract e tl
  | 0::tl, DB_tensor (e1, _) -> db_extract e1 tl
  | 1::tl, DB_tensor (_, e2) -> db_extract e2 tl
  | 0::tl, DB_tslet (_, _, e1, _) -> db_extract e1 tl
  | 1::tl, DB_tslet (_, _, _, e2) ->db_extract e2 tl
  | 0::tl, DB_ret e -> db_extract e tl
  | 0::tl, DB_bind (_, e1, _) -> db_extract e1 tl
  | 1::tl, DB_bind (_, _, e2) -> db_extract e2 tl
  | 0::tl, DB_hypo (_, _, (e1, _), _) -> db_extract e1 tl
  | 1::tl, DB_hypo (_, _, (_, e2), _) ->db_extract e2 tl
  | 2::tl, DB_hypo (_, _, (_, _), e3) -> db_extract e3 tl
  | l, e -> raise (Invalid_argument "db_extract: invalid position")
;;

(* let db_structure_rec (e : db_term) (f_idx : int -> 'a) (f_rec : db_term -> 'a) (f_default : db_term -> 'a) : 'a =  *)
(*   match e with  *)
(*     | DB_idx i -> f_idx i *)
(*     | DB_if (e0, e1, e2) -> *)
(*       DB_if (f_rec e0, f_rec e1, f_rec e2)  *)
(*     | DB_func (a, e0) -> *)
(*       DB_func (a, f_rec e0) *)
(*     | DB_app (e1, e2) ->  *)
(*       DB_app (f_rec e1, f_rec e2) *)
(*     | DB_let (s, e1, e2) ->  *)
(*       DB_let (s, f_rec e1, f_rec e2) *)
(*     | DB_pair (e1, e2) ->  *)
(*       DB_pair (f_rec e1, f_rec e2) *)
(*     | DB_fst e0 ->  *)
(*       DB_fst (f_rec e0) *)
(*     | DB_snd e0 ->  *)
(*       DB_snd (f_rec e0) *)
(*     | DB_tensor (e1, e2) ->  *)
(*       DB_tensor (f_rec e1, f_rec e2) *)
(*     | DB_tslet (s1, s2, e1, e2) ->  *)
(*       DB_tslet (s1, s2, f_rec e1, f_rec e2) *)
(*     | DB_ret e0 ->  *)
(*       DB_ret (f_rec e0) *)
(*     | DB_bind (s, e1, e2) ->  *)
(*       DB_bind (s, f_rec e1, f_rec e2) *)
(*     | DB_hypo (s, (e1, e2), e3) ->  *)
(*       DB_hypo (s, (f_rec e1, f_rec e2), f_rec e3) *)
(*     | _ -> f_default e *)
(* ;; *)

let rec db_alpha_equal (e1 : db_term) (e2 : db_term) : bool = 
  match (e1, e2) with 
    | (DB_if (e10, e11, e12), DB_if (e20, e21, e22)) -> 
      (db_alpha_equal e10 e20) && (db_alpha_equal e11 e21) && (db_alpha_equal e12 e22)
    | (DB_func (_, e10), DB_func (_, e20)) | (DB_ret e10, DB_ret e20) 
    | (DB_fst e10, DB_fst e20) | (DB_snd e10, DB_snd e20) -> 
      db_alpha_equal e10 e20
    | (DB_app (e11, e12), DB_app (e21, e22)) | (DB_tensor (e11, e12), DB_tensor (e21, e22)) 
    | (DB_let (_, e11, e12), DB_let (_, e21, e22)) | (DB_bind (_, e11, e12), DB_bind (_, e21, e22)) 
    | (DB_tslet (_, _, e11, e12), DB_tslet (_, _, e21, e22)) -> 
      (db_alpha_equal e11 e21) && (db_alpha_equal e12 e22)
    | (DB_hypo (_, a1, (e11, e12), e13), DB_hypo (_, a2, (e21, e22), e23)) ->
        (db_alpha_equal e11 e21) && (db_alpha_equal e12 e22) && (db_alpha_equal e13 e23)
    | _ -> e1 = e2 
;;

let rec db_lift_rec (e : db_term) (n : int) (lb : int) : db_term = (* For implementing db_lift *)
  (* Lift RECURSIVELY all the global indexes that are no smaller than lb *)
  match e with 
    | DB_idx i -> 
      if i >= lb then DB_idx (i+n) else DB_idx i (* Non-local indexes will be incremented *)
    | DB_if (e0, e1, e2) -> 
      DB_if (db_lift_rec e0 n lb, db_lift_rec e1 n lb, db_lift_rec e2 n lb)
    | DB_func (a, e0) -> 
      DB_func (a, db_lift_rec e0 n (lb+1)) (* Local index bound is incremented by 1 inside a function *)
    | DB_app (e1, e2) -> 
      DB_app (db_lift_rec e1 n lb, db_lift_rec e2 n lb)
    | DB_let (s, e1, e2) -> 
      DB_let (s, db_lift_rec e1 n lb, db_lift_rec e2 n (lb+1))
    | DB_pair (e1, e2) -> 
      DB_pair (db_lift_rec e1 n lb, db_lift_rec e2 n lb)
    | DB_fst e0 -> 
      DB_fst (db_lift_rec e0 n lb)
    | DB_snd e0 -> 
      DB_snd (db_lift_rec e0 n lb)
    | DB_tensor (e1, e2) -> 
      DB_tensor (db_lift_rec e1 n lb, db_lift_rec e2 n lb)
    | DB_tslet (s1, s2, e1, e2) -> 
      DB_tslet (s1, s2, db_lift_rec e1 n lb, db_lift_rec e2 n (lb+2))
    | DB_ret e0 -> 
      DB_ret (db_lift_rec e0 n lb)
    | DB_bind (s, e1, e2)  -> 
      DB_bind (s, db_lift_rec e1 n lb, db_lift_rec e2 n (lb+1))
    | DB_hypo (s, c, (e1, e2), e3) -> 
      DB_hypo (s, c, (db_lift_rec e1 n lb, db_lift_rec e2 n lb), db_lift_rec e3 n lb)
    | _ -> e
;;

let db_lift (e : db_term) : db_term = (* Lift all non-local index by 1 *)
  db_lift_rec e 1 0
;;

let db_lift_by (e : db_term) (n : int) : db_term = (* Lift all non-local index by n *)
  db_lift_rec e n 0
;;

let rec db_rewrite (e : db_term) (e_old : db_term) (e_new : db_term) : db_term =
  if db_alpha_equal e e_old 
  then e_new 
  else match e with 
    | DB_if (e0, e1, e2) -> 
      DB_if (db_rewrite e0 e_old e_new, db_rewrite e1 e_old e_new, db_rewrite e2 e_old e_new) 
    | DB_func (a, e0) -> 
      DB_func (a, db_rewrite e0 (db_lift e_old) (db_lift e_new))
    | DB_app (e1, e2) -> 
      DB_app (db_rewrite e1 e_old e_new, db_rewrite e2 e_old e_new) 
    | DB_let (s, e1, e2) -> 
      DB_let (s, db_rewrite e1 e_old e_new, db_rewrite e2 (db_lift e_old) (db_lift e_new))
    | DB_pair (e1, e2) -> 
      DB_pair (db_rewrite e1 e_old e_new, db_rewrite e2 e_old e_new) 
    | DB_fst e0 -> 
      DB_fst (db_rewrite e0 e_old e_new)
    | DB_snd e0 -> 
      DB_snd (db_rewrite e0 e_old e_new) 
    | DB_tensor (e1, e2) -> 
      DB_tensor (db_rewrite e1 e_old e_new, db_rewrite e2 e_old e_new) 
    | DB_tslet (s1, s2, e1, e2) -> 
      DB_tslet (s1, s2, db_rewrite e1 e_old e_new, db_rewrite e2 (db_lift_by e_old 2) (db_lift_by e_new 2)) 
    | DB_ret e0 -> 
      DB_ret (db_rewrite e0 e_old e_new) 
    | DB_bind (s, e1, e2) -> 
      DB_bind (s, db_rewrite e1 e_old e_new, db_rewrite e2 (db_lift e_old) (db_lift e_new))
    | DB_hypo (s, a, (e1, e2), e3) -> 
      DB_hypo (s, a, (e1, e2), db_rewrite e3 e_old e_new)
    | _ -> e
;;

let rec db_rewrite_hypo (e : db_term) (e_old : db_term) (e_new : db_term) : db_term =
  if db_alpha_equal e e_old 
  then e_new 
  else match e with 
    | DB_if (e0, e1, e2) -> 
      DB_if (db_rewrite_hypo e0 e_old e_new, db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 e_old e_new) 
    | DB_func (a, e0) -> 
      DB_func (a, db_rewrite_hypo e0 (db_lift e_old) (db_lift e_new))
    | DB_app (e1, e2) -> 
      DB_app (db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 e_old e_new) 
    | DB_let (s, e1, e2) -> 
      DB_let (s, db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 (db_lift e_old) (db_lift e_new))
    | DB_pair (e1, e2) -> 
      DB_pair (db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 e_old e_new) 
    | DB_fst e0 -> 
      DB_fst (db_rewrite_hypo e0 e_old e_new)
    | DB_snd e0 -> 
      DB_snd (db_rewrite_hypo e0 e_old e_new) 
    | DB_tensor (e1, e2) -> 
      DB_tensor (db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 e_old e_new) 
    | DB_tslet (s1, s2, e1, e2) -> 
      DB_tslet (s1, s2, db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 (db_lift_by e_old 2) (db_lift_by e_new 2)) 
    | DB_ret e0 -> 
      DB_ret (db_rewrite_hypo e0 e_old e_new) 
    | DB_bind (s, e1, e2) -> 
      DB_bind (s, db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 (db_lift e_old) (db_lift e_new))
    | DB_hypo (s, c, (e1, e2), e3) -> 
      ( match e_old with 
	| DB_var v -> 
	  if List.mem v (List.map (fun x -> x.str) c)
	  then       
	    DB_hypo (s, c, (e1,  e2), db_rewrite_hypo e3 e_old e_new)
	  else 
	    DB_hypo (s, c, (db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 e_old e_new), db_rewrite_hypo e3 e_old e_new)
	| _ -> 
	  DB_hypo (s, c, (db_rewrite_hypo e1 e_old e_new, db_rewrite_hypo e2 e_old e_new), db_rewrite_hypo e3 e_old e_new)
      )
    | _ -> e
;;

let db_rewrite_at (e : db_term) (e_old : db_term) (e_new : db_term) (pos : int list) : db_term = 
  navigate (fun x -> db_rewrite x e_old e_new) pos e
;;

let rec db_max_index (e : db_term) : int =
  match e with 
    | DB_idx i -> i 
    | DB_if (e0, e1, e2) | DB_hypo (_, _, (e1, e2), e0) -> 
      max (db_max_index e0) (max (db_max_index e1) (db_max_index e2))
    | DB_func (_, e0) | DB_fst e0 | DB_snd e0 | DB_ret e0 -> 
      db_max_index e0
    | DB_app (e1, e2) | DB_let (_, e1, e2) | DB_pair (e1, e2) 
    | DB_tensor (e1, e2) | DB_tslet (_, _, e1, e2) | DB_bind (_, e1, e2) ->
      max (db_max_index e1) (db_max_index e2)
    | _ -> -1
;;

let rec db_index_is_free (e : db_term) (i : int) : bool =
  match e with 
    | DB_idx i' -> (i <> i') 
    | DB_if (e0, e1,e2)  -> 
      (db_index_is_free e0 i) && (db_index_is_free e1 i) && (db_index_is_free e2 i) 
    | DB_func (_, e0) -> 
      db_index_is_free e0 (i+1) 
    | DB_app (e1, e2) | DB_pair (e1, e2) | DB_tensor (e1, e2) -> 
      (db_index_is_free e1 i) && (db_index_is_free e2 i) 
    | DB_let(_, e1, e2) | DB_bind (_, e1, e2) -> 
      (db_index_is_free e1 i) && (db_index_is_free e2 (i+1)) 
    | DB_fst e0 | DB_snd e0 | DB_ret e0 -> 
      (db_index_is_free e0 i)
    | DB_tslet (_, _, e1, e2) -> 
      (db_index_is_free e1 i) && (db_index_is_free e2 (i+2)) 
    | DB_hypo (_, _, (e1, e2), e3) -> 
      (db_index_is_free e1 i) && (db_index_is_free e2 i) && (db_index_is_free e3 i) 
    | _ -> true
;;

(**
[position id e] returns the position of the definition of [id] in [e].
[id] can be the identifier of a let, a bind or a lethypo.
*)
let rec position id e =
  match e with
  | DB_if (e0, e1, e2) -> (
      try
        0 :: position id e0
      with
      | Not_found -> (
          try
            1 :: position id e1
          with
          | Not_found -> 2 :: position id e2
        )
    )
  | DB_func (_, e0) -> 0 :: position id e0
  | DB_app (e0, e1) -> (
      try
        0 :: position id e0
      with
      | Not_found -> 1 :: position id e1
    )
  | DB_let (x, e0, e1) -> (
      if id = x then [] else
      try
        0 :: position id e0
      with
      | Not_found -> 1 :: position id e1
    )
  | DB_pair (e0, e1) -> (
      try
        0 :: position id e0
      with
      | Not_found -> 1 :: position id e1
    )
  | DB_fst e0 -> 0 :: position id e0
  | DB_snd e0 -> 0 :: position id e0
  | DB_tensor (e0, e1) -> (
      try
        0 :: position id e0
      with
      | Not_found -> 1 :: position id e1
    )
  | DB_tslet (_, _, e0, e1) -> (
      try
        0 :: position id e0
      with
      | Not_found -> 1 :: position id e1
    )
  | DB_ret e0 -> 0 :: position id e0
  | DB_bind (x, e0, e1) -> (
      if id = x then [] else
      try
        0 :: position id e0
      with
      | Not_found -> 1 :: position id e1
    )
  | DB_hypo (x, _, (e0, e1), e2) -> (
      if id = x then [] else
(*      try
        0 :: position id e0
      with
      | Not_found -> (
          try
            1 :: position id e1
          with
          | Not_found -> *) 2 :: position id e2
    )
  | _ -> raise Not_found

let db_position (id : string) (e : db_term) : int list = position id e

let rec has_neg_index (e : db_term) : bool = 
  match e with 
    | DB_idx i -> (i < 0)
    | DB_if (e0, e1, e2) | DB_hypo (_, _, (e1, e2), e0) -> 
      (has_neg_index e0) || (has_neg_index e1) || (has_neg_index e2)
    | DB_func (_, e0) | DB_fst e0 | DB_snd e0 | DB_ret e0 -> 
      (has_neg_index e0)
    | DB_app (e1, e2) | DB_pair (e1, e2) | DB_tensor (e1, e2) 
    | DB_let (_, e1, e2) | DB_bind (_, e1, e2) | DB_tslet (_ , _, e1, e2) -> 
      (has_neg_index e1) || (has_neg_index e2)
    | _ -> false

let rec db_pattern_rec (e1 : db_term) (e2 : db_term) (vars : string list) (level : int) : (string * (int * db_term)) list = 
  match (e1, e2) with 
    | (DB_var s, e) -> 
      if List.mem s vars then [(s, (level, e))] else []
    | (DB_idx i1, DB_idx i2) -> 
      if i1 = i2 then [] else failwith "FC.Debruijn.db_pattern_rec" 
    | (DB_nil, DB_nil) | (DB_zero, DB_zero) | (DB_one, DB_one) | (DB_rand, DB_rand) 
      -> []
    | (DB_case t1, DB_case t2) | (DB_srec t1, DB_srec t2) 
      -> if t1 = t2 then [] else failwith "FC.Debruijn.db_pattern_rec" 
    | (DB_if (e10, e11, e12), DB_if (e20, e21, e22)) -> 
      db_pattern_rec e10 e20 vars level @ db_pattern_rec e11 e21 vars level @ db_pattern_rec e12 e22 vars level 
    | (DB_func (_, e10), DB_func (_, e20)) -> 
      db_pattern_rec e10 e20 vars (level+1)
    | (DB_fst e10, DB_fst e20) | (DB_snd e10, DB_snd e20) 
    | (DB_ret e10, DB_ret e20) | (DB_hypo (_, _, _, e10), DB_hypo (_, _, _, e20))-> 
      db_pattern_rec e10 e20 vars level 
    | (DB_app (e11, e12), DB_app (e21, e22)) 
    | (DB_pair (e11, e12), DB_pair (e21, e22)) | (DB_tensor (e11, e12), DB_tensor (e21, e22)) -> 
      db_pattern_rec e11 e21 vars level @ db_pattern_rec e12 e22 vars level 
    | (DB_tslet (_, _, e11, e12), DB_tslet (_, _, e21, e22)) ->
      db_pattern_rec e11 e21 vars level @ db_pattern_rec e12 e22 vars (level+2)
    | (DB_let (_, e11, e12), DB_let (_, e21, e22)) | (DB_bind (_, e11, e12), DB_bind (_, e21, e22)) -> 
      db_pattern_rec e11 e21 vars level @ db_pattern_rec e12 e22 vars (level+1)
    | (_, _) -> failwith "FC.Debruijn.db_pattern_rec" 

let db_pattern (e1 : db_term) (e2 : db_term) (vars : string list) : (string * db_term) list = 
  let matches = List.map (fun x -> (fst x, db_lift_by (snd (snd x)) (0 - fst (snd x))) ) (db_pattern_rec e1 e2 vars 0) in 
  let matches2 = List.map (fun x -> (x, snd (List.split (List.find_all (fun y -> (fst y) = x) matches)) )) vars in 
  let rec verify_match (m : string * (db_term list)) = 
    match m with 
      | (s, []) -> 
	failwith ("FC.Debruijn.db_pattern: no match for " ^ s)
      | (s, [e])  -> 
	if has_neg_index e
	then 
	  failwith ("FC.Debruijn.db_pattern: negative index in match for " ^ s)
	else 
	  (s, e)
      | (s, e1 :: e2 :: l') -> 
	if db_alpha_equal e1 e2 
	then verify_match (s, e1 :: l')
	else 
	  failwith ("FC.Debruijn.db_pattern: inconsistent matches for " ^ s)
  in 
  List.map verify_match matches2



