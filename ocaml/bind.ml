open Types
open Common
open Debruijn

let swap_bind (e : db_term) : db_term =
  let swap_index = fun x -> (* swap the indices of the swapped two binders in x: let s1 = .. in let s2 = .. in x *)
    let m = (db_max_index x) + 1 in  
    db_rewrite_hypo (db_rewrite_hypo (db_rewrite_hypo x (DB_idx 0) (DB_idx m)) (DB_idx 1) (DB_idx 0)) (DB_idx m) (DB_idx 1)
	(* first change the index of s2 to m to avoid index conflict, then change the index of s1 to 0, 
	   lastly change the index of s2 (m) to 1 *)
  in 
  match e with 
    | DB_let (s1, e1, DB_let (s2, e2, e3)) -> 
      if db_index_is_free e2 0 
      then 
	DB_let (s2, (db_lift_by e2 (-1)), DB_let (s1, (db_lift e1), swap_index e3))
      else 
	failwith ("FC.Bind.swap_bind " ^ s1 ^ " " ^ s2)
    | DB_let (s1, e1, DB_bind (s2, e2, e3)) -> 
      if db_index_is_free e2 0 
      then 
	DB_bind (s2, (db_lift_by e2 (-1)), DB_let (s1, (db_lift e1), swap_index e3))
      else 
	failwith ("FC.Bind.swap_bind " ^ s1 ^ " " ^ s2)
    | DB_bind (s1, e1, DB_let (s2, e2, e3)) -> 
      if db_index_is_free e2 0 
      then 
	DB_let (s2, (db_lift_by e2 (-1)), DB_bind (s1, (db_lift e1), swap_index e3))
      else 
	failwith ("FC.Bind.swap_bind " ^ s1 ^ " " ^ s2)
    | DB_bind (s1, e1, DB_bind (s2, e2, e3)) -> 
      if db_index_is_free e2 0 
      then 
	DB_bind (s2, (db_lift_by e2 (-1)), DB_bind (s1, (db_lift e1), swap_index e3))
      else 
	failwith ("FC.Bind.swap_bind " ^ s1 ^ " " ^ s2)
    | DB_bind (s1, e1, DB_hypo (s2, c, (e2,e2'), e3)) -> 
      if db_index_is_free e2 0 && db_index_is_free e2' 0
      then 
	DB_hypo (s2, c, (db_lift_by e2 (-1), (db_lift_by e2' (-1))), DB_bind (s1, e1, e3))
      else 
	failwith ("FC.Bind.swap_bind " ^ s1 ^ " " ^ s2)
    | DB_let (s1, e1, DB_hypo (s2, c, (e2,e2'), e3)) -> 
      if db_index_is_free e2 0 && db_index_is_free e2' 0
      then 
	DB_hypo (s2, c, (db_lift_by e2 (-1), (db_lift_by e2' (-1))), DB_let (s1, e1, e3))
      else 
	failwith ("FC.Bind.swap_bind " ^ s1 ^ " " ^ s2)
    | DB_hypo (s1, c, (e1, e1'), DB_bind (s2, e2, e3)) -> 
	DB_bind (s2, e2, DB_hypo (s1, c, (db_lift e1, db_lift e1'), e3))
    | DB_hypo (s1, c, (e1, e1'), DB_let (s2, e2, e3)) -> 
	DB_let (s2, e2, DB_hypo (s1, c, (db_lift e1, db_lift e1'), e3))
    | _ -> failwith "FC.Bind.swap_bind"
;;

let swap_bind_at (e : db_term) (pos : int list) : db_term  =
  db_navigate e pos swap_bind 
;;

let move_bind_up (e : db_term) (pos : int list) : db_term =
  if List.length pos <= 1 
  then 
    failwith "FC.Bind.move_bind_up"
  else 
    let pos' = sublist 0 (List.length pos -2) pos in 
    swap_bind_at e pos' 
;;

let move_bind_down (e : db_term) (pos : int list) : db_term =
  swap_bind_at e pos
;;
