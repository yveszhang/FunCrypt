open Types
open Debruijn
open Common
open Betareduce

let change_at (e : db_term) (pos : int list) (s : string) : db_term =
  let e2 = string_to_fc_term s in
  let e1 = fc_extract (db_term_to_fc_term e) pos in
  if db_alpha_equal (db_normalize (fc_term_to_db_term e1) false) (db_normalize (fc_term_to_db_term e2) false) then
    fc_term_to_db_term (fc_navigate (fun _ -> e2) pos (db_term_to_fc_term e))
  else  (

    print_endline "----------------------------------------------------------------------";
    print_endline "----------------------------------------------------------------------";
    print_endline (fc_term_to_pretty_string (db_term_to_fc_term_ignore (db_normalize (fc_term_to_db_term e1) false )));
    print_endline "----------------------------------------------------------------------";
    print_endline "----------------------------------------------------------------------";
    print_endline (fc_term_to_pretty_string (db_term_to_fc_term_ignore (db_normalize (fc_term_to_db_term e2) false )));
    print_endline "----------------------------------------------------------------------";
    print_endline "----------------------------------------------------------------------";

    raise (Invalid_argument "change_at")
  )

let rec db_instantiate (expr:db_term)(args:(string * db_term) list) : db_term =
  match args with
  | [] -> expr
  | (_, e)::args' -> db_instantiate (DB_app (expr, e)) args'

 (* TODO: We should check that pos is a position in e3, i.e., a position after pos_hypo. *)
let apply_at (expr:db_term)(pos_hypo:int list)(pos:int list) : db_term =
  let hypo = db_extract expr pos_hypo in
  match hypo with
  | DB_hypo (x, l, (e1, e2), e3) ->
      db_navigate expr pos (
        fun e ->
          let args = db_pattern e1 e (List.map (fun x -> x.str) l) in
          db_instantiate e2 args
      )
  | _ -> failwith "apply_at: Not an hypothesis"

(* TO BE DELETED after removing calls in testtactics.ml *)
let apply (expr:db_term)(pos_hypo:int list) =
  let hypo = db_extract expr pos_hypo in
  match hypo with
  | DB_hypo (_, _, (e1, e2), _) ->
      db_navigate expr (pos_hypo @ [2]) (fun e -> db_rewrite e e1 e2)
  | _ -> failwith "format: Not an hypothesis"

let rec fc_instantiate (expr:fc_term)(args:fc_term list) =
  let merged_term = List.fold_left (fun f x -> FC_app (f, x)) expr args in 
  db_term_to_fc_term (db_normalize (fc_term_to_db_term merged_term) false)

let apply_with (expr:db_term) (pos_hypo:int list) (args : fc_context) =
  apply  expr pos_hypo
(***************)

let clear_hypo prog pos =
  db_navigate prog pos (fun x -> match x with DB_hypo (_, _, _, e) -> e | _ -> failwith "clear_hypo")

let rec permute3 idx func expr =
  if expr = func then DB_idx idx else
  match expr with
  | DB_idx n -> if n = idx then failwith "permute: Invalid occurrence of the variable" else expr
  | DB_if (e1, e2, e3) -> DB_if (permute3 idx func e1, permute3 idx func e2, permute3 idx func e3)
  | DB_func (x, e1) -> DB_func (x, permute3 (idx+1) func e1)
  | DB_app (e1, e2) -> DB_app (permute3 idx func e1, permute3 idx func e2)
  | DB_let (x, e1, e2) -> DB_let (x, permute3 idx func e1, permute3 (idx+1) func e2)
  | DB_pair (e1, e2) -> DB_pair (permute3 idx func e1, permute3 idx func e2)
  | DB_fst e1 -> DB_fst (permute3 idx func e1)
  | DB_snd e1 -> DB_snd (permute3 idx func e1)
  | DB_tensor (e1, e2) -> DB_tensor (permute3 idx func e1, permute3 idx func e2)
  | DB_tslet (x, y, e1, e2) -> DB_tslet (x, y, permute3 idx func e1, permute3 (idx+2) func e2)
  | DB_ret e1 -> DB_ret (permute3 idx func e1)
  | DB_bind (x, e1, e2) -> DB_bind (x, permute3 idx func e1, permute3 (idx+1) func e2)
  | DB_hypo (x, c, (e1, e2), e3) -> DB_hypo (x, c, (permute3 idx func e1, permute3 (idx+1) func e2), permute3 (idx+1) func e3)
  | _ -> expr

let permute2 set f expr =
  match f with
  | DB_func (_, func) -> (
      match expr with
      | DB_bind (x, e1, e2) -> DB_bind (x, set, permute3 0 func e2)
      | _ -> failwith "permute: There is no random choice at the specified position."
    )
  | _ -> failwith "permute: This is not a function!"

let permute set func pos expr =
  db_navigate expr pos (permute2 set func) 

let rec add_func (e:db_term)(n:int) : db_term =
  match n with
  | 0 -> e
  | _ -> add_func (DB_func ({str = "?" ^ string_of_int (n-1); asp = Arrow ; typ = T_var "UNKNOWN"}, e)) (n-1)

(* let format (expr:db_term)(pos_hypo:int list)(pos:int list)(l:db_term list) : db_term = *)
(*   let hypo = db_extract expr pos_hypo in *)
(*   match hypo with *)
(*   | DB_hypo (_, (hypo_left, _), _) -> *)
(*       db_navigate expr pos ( *)
(*         fun e -> *)
(*           add_func *)
(*             (List.fold_left (fun e e1 -> db_rewrite e e1 (DB_idx 0)) (db_lift_by e (List.length l)) l) *)
(*             (List.length l) *)
(*       ) *)
(*   | _ -> failwith "format: Not an hypothesis" *)
