open Types
open Common
open Debruijn

let beta_reduce (e : db_term) : db_term = (* beta_reduce only reduces when the term is a redex *)
  match e with 
    | DB_app (DB_func (a, e1), e2) -> (* lambda beta-reduction *)
      db_lift_by (db_rewrite e1 (DB_idx 0) (db_lift e2)) (-1)
    | DB_app (DB_app (DB_case _, e_bits),  DB_pair (e_nil, DB_pair (e0, e1))) -> 
      (* Reduction of safe recursion *)
      (match e_bits with 
	| DB_nil -> e_nil
	| DB_app (DB_zero, _) -> DB_app (e0, e_bits) 
	| DB_app (DB_one, _) -> DB_app (e1, e_bits) 
	| _ -> e 
      )
    | DB_app (DB_app (DB_app (DB_srec t, e_nil), e_rec), e_bits) -> (* reduction of case *)
      (match e_bits with 
	| DB_nil -> e_nil
	| DB_app (DB_zero, n) | DB_app (DB_one, n) -> 
	  DB_app (DB_app (e_rec, n), DB_app (DB_app (DB_app (DB_srec t, e_nil), e_rec), n))
	| _ -> e
      )
    | DB_func (_, DB_app (e0, DB_idx 0)) -> (* eta-reduction *)
      if db_index_is_free e0 0 then e0 else e
    | DB_let (s, e1, e2) -> (* let x = e1 in e2 --> e2 [e1 / x] *)
      db_lift_by (db_rewrite_hypo e2 (DB_idx 0) (db_lift e1)) (-1)
    | DB_if (DB_nil, e1, e2) -> (* if nil then e1 else e2 --> e2 *)
      e2  
    | DB_if (DB_one _, e1, e2) | DB_if (DB_zero _, e1, e2) -> (* if B _ then e1 else e2 --> e1 *)
      e1  
    | DB_fst (DB_pair (e1, e2))  -> 
      e1
    | DB_snd (DB_pair (e1, e2))  -> 
      e2
    | DB_tslet (s1, s2, DB_tensor (e1, e2), e0) -> 
      let e1' = db_lift_by e1 2 in (* Adjust the index of e1 by increasing every global index by 2 *)
      (* The global indexeswill be adjusted back when the substitution is done. Same for e2 *)
      let e2' = db_lift_by e2 2 in 
      db_lift_by (db_rewrite (db_rewrite e0 (DB_idx 0) e2') (DB_idx 1) e1') (-2) (* Change the adjusted index back *)
    | DB_bind (_, DB_ret e1, e2) -> (* bind x = return(e1) in e2 --> e2 [e1 / x] *)
      db_lift_by (db_rewrite e2 (DB_idx 0) (db_lift e1)) (-1)
    | DB_bind (_, e1, DB_ret (DB_idx 0)) -> (* bind x = e1 in return(x) --> e1 *)
      e1 
    | DB_bind (s2, DB_bind (s1, e1, e2), e3) ->  
	(* bind s2 = (bind s1 = e1 in e2) in e3 --> bind s1 = e1 in bind s2 = e2 in e3 *)
      DB_bind (s1, e1, DB_bind (s2, e2, (db_lift_rec e3 1 1))) 
    (* All the indexes in e3 referring a var outside the whole term must be incremented, as s1 is inserted! *)
    | _ -> e 
;;

let rec db_reduce (e : db_term) : db_term =  (* Left-most, call-by-value reduction *)
  match e with 
    | DB_if (e0, e1, e2) -> 
      let e' = beta_reduce e in 
      if db_alpha_equal e e'  
      then DB_if (db_reduce e0, e1, e2) else e'
    | DB_app (e1, e2) -> 
      let e1' = db_reduce e1 in 
      if db_alpha_equal e1 e1' 
      then 
	let e2' = db_reduce e2 in 
	if db_alpha_equal e2 e2' 
	then beta_reduce e 
	else DB_app (e1, e2') 
      else DB_app (e1', e2)
    | DB_let (s, e1, e2) -> 
      let e1' = db_reduce e1 in 
      if db_alpha_equal e1 e1' then beta_reduce e else DB_let (s, e1', e2)
    | DB_pair (e1, e2) -> 
      let e1' = db_reduce e1 in 
      if db_alpha_equal e1 e1' then DB_pair (e1, db_reduce e2) else DB_pair (e1', e2)
    | DB_fst e0 -> 
      DB_fst (db_reduce e0)
    | DB_snd e0 -> 
      DB_snd (db_reduce e0)
    | DB_tensor (e1, e2) -> 
      let e1' = db_reduce e1 in 
      if db_alpha_equal e1 e1' then DB_tensor (e1, db_reduce e2) else DB_tensor (e1', e2)
    | DB_tslet (s1, s2, e1, e2) -> 
      let e1' = db_reduce e1 in 
      if db_alpha_equal e1 e1' 
      then beta_reduce e else DB_tslet (s1, s2, e1', e2) 
    | DB_bind (s, e1, e2) ->  
      let e' = beta_reduce e in 
      if db_alpha_equal e e' 
      then DB_bind (s, db_reduce e1, e2) else e'
    | DB_hypo (s, c, (e1, e2), e3) -> 
      DB_hypo (s, c, (e1, e2), db_reduce e3) 
    | _ -> beta_reduce e
;;

(* let rec db_reduce_deep (e : db_term) : db_term =  *)
(*   let e' = db_reduce e in  *)
(*   if db_alpha_equal e e'  *)
(*   then  *)
(*     match e with  *)
(*       | DB_if (e0, e1, e2) ->  *)
(* 	let e0' = db_reduce_deep e0 in  *)
(* 	if db_alpha_equal e0 e0'  *)
(* 	then  *)
(* 	  let e1' = db_reduce_deep e1 in  *)
(* 	  if db_alpha_equal e1 e1'  *)
(* 	  then DB_if (e0, e1, db_reduce_deep e2) *)
(* 	  else DB_if (e0, e1', e2)  *)
(* 	else DB_if (e0', e1, e2)  *)
(*       | DB_func (a, e0) ->  *)
(* 	DB_func (a, db_reduce_deep e0) *)
(*       | DB_app (e1, e2) ->  *)
(* 	let e1' = db_reduce_deep e1 in  *)
(* 	if db_alpha_equal e1 e1'  *)
(* 	then DB_app (e1, db_reduce_deep e2) *)
(* 	else DB_app (e1', e2) *)
(*       | DB_let (s, e1, e2) -> *)
(* 	let e1' = db_reduce_deep e1 in  *)
(* 	if db_alpha_equal e1 e1'  *)
(* 	then DB_let (s, e1, db_reduce_deep e2) *)
(* 	else DB_let (s, e1', e2) *)
(*       | DB_pair (e1, e2) ->  *)
(* 	let e1' = db_reduce_deep e1 in  *)
(* 	if db_alpha_equal e1 e1'  *)
(* 	then DB_pair (e1, db_reduce_deep e2) *)
(* 	else DB_pair (e1', e2) *)
(*       | DB_fst e0 ->  *)
(* 	DB_fst (db_reduce_deep e0)  *)
(*       | DB_snd e0 ->  *)
(* 	DB_snd (db_reduce_deep e0)  *)
(*       | DB_tensor (e1, e2) ->  *)
(* 	let e1' = db_reduce_deep e1 in  *)
(* 	if db_alpha_equal e1 e1'  *)
(* 	then DB_tensor (e1, db_reduce_deep e2) *)
(* 	else DB_tensor (e1', e2) *)
(*       | DB_tslet (s1, s2, e1, e2) ->  *)
(* 	let e1' = db_reduce_deep e1 in  *)
(* 	if db_alpha_equal e1 e1'  *)
(* 	then DB_tslet (s1, s2, e1, db_reduce_deep e2) *)
(* 	else DB_tslet (s1, s2, e1', e2) *)
(*       | DB_ret e0 ->  *)
(* 	DB_ret (db_reduce_deep e0)  *)
(*       | DB_bind (s, e1, e2) ->  *)
(* 	let e1' = db_reduce_deep e1 in  *)
(* 	if db_alpha_equal e1 e1'  *)
(* 	then DB_bind (s, e1, db_reduce_deep e2) *)
(* 	else DB_bind (s, e1', e2) *)
(*       | DB_hypo (s, (e1, e2), e3) ->  *)
(* 	DB_hypo (s, (e1, e2), db_reduce_deep e3)  *)
(*       | _ -> e *)
(*   else *)
(*     e' *)
(* ;; *)

let rec db_reduce_multi (e : db_term) (n : int) : db_term = 
  if n = 0 
  then e 
  else 
    let e' = db_reduce e  in 
    if db_alpha_equal e e' then e else db_reduce_multi e' (n-1) 
;;

let rec db_evaluate (e : db_term) (verbose : bool) : db_term =  
  let _ = 
    if verbose 
    then print_endline ( (fc_term_to_pretty_string  (db_term_to_fc_term e)) ^ "\n--->")
    else () 
  in 
  let e' = db_reduce e in 
  if db_alpha_equal e e' then e else db_evaluate e' verbose
;;

(* let rec db_evaluate_deep (e : db_term) (verbose : bool) : db_term =   *)
(*   let _ =  *)
(*     if verbose  *)
(*     then print_endline ( (fc_term_to_pretty_string  (db_term_to_fc_term_ignore e)) ^ "\n--->") *)
(*     else ()  *)
(*   in  *)
(*   let e' = db_reduce_deep e in  *)
(*   if db_alpha_equal e e' then e else db_evaluate_deep e' verbose *)
(* ;; *)

let beta_reduce_at (e : db_term) (pos : int list) : db_term = 
  db_navigate e pos beta_reduce 
;;

let db_reduce_at (e : db_term) (pos : int list) : db_term = 
  db_navigate e pos db_reduce 
;;

let rec db_normalize_one (e : db_term) : db_term =
  let e' = beta_reduce e in 
  if db_alpha_equal e e' 
  then 
    match e with 
      | DB_if (e0, e1, e2) -> 
	let e0' = db_normalize_one e0 in 
	if db_alpha_equal e0 e0' 
	then 
	  let e1' = db_normalize_one e1 in 
	  if db_alpha_equal e1 e1' then DB_if (e0, e1, db_normalize_one e2) else DB_if (e0, e1', e2)
	else DB_if (e0', e1, e2) 
      | DB_func (a, e0) -> 
	DB_func (a, db_normalize_one e0) 
      | DB_app (e1, e2) ->
	let e1' = db_normalize_one e1 in 
	if db_alpha_equal e1 e1' then DB_app (e1, db_normalize_one e2) else DB_app (e1', e2)
      | DB_let (s1, e1, e2) -> 
	let e1' = db_normalize_one e1 in 
	if db_alpha_equal e1 e1' then DB_let (s1, e1, db_normalize_one e2) else DB_let (s1, e1', e2)
      | DB_pair (e1, e2) -> 
	let e1' = db_normalize_one e1 in 
	if db_alpha_equal e1 e1' then DB_pair (e1, db_normalize_one e2) else DB_pair (e1', e2)
      | DB_fst e0 ->
	DB_fst (db_normalize_one e0)
      | DB_snd e0 -> 
	DB_snd (db_normalize_one e0)
      | DB_tensor (e1, e2) ->
	let e1' = db_normalize_one e1 in 
	if db_alpha_equal e1 e1' then DB_tensor (e1, db_normalize_one e2) else DB_tensor (e1', e2)
      | DB_tslet (s1, s2, e1, e2) -> 
	let e1' = db_normalize_one e1 in 
	if db_alpha_equal e1 e1' 
	then DB_tslet (s1, s2, e1, db_normalize_one e2) 
	else DB_tslet (s1, s2, e1', e2)
      | DB_ret e0 -> 
	DB_ret (db_normalize_one e0 )
      | DB_bind (s1, e1, e2) -> 
	let e1' = db_normalize_one e1 in 
	if db_alpha_equal e1 e1' then DB_bind (s1, e1, db_normalize_one e2) else DB_bind (s1, e1', e2)
      | DB_hypo (s, c, (e1, e2), e3) -> 
	DB_hypo (s, c, (e1, e2), db_normalize_one e3)
      | _ -> e
  else 
    e'
;;

let rec db_normalize (e : db_term) (verb : bool) : db_term = 
  let _ = if verb then ( print_endline (db_term_to_pretty_string e); ignore (read_line ()) ) else () in 
  let e' = db_normalize_one e in 
  if db_alpha_equal e e' then e else db_normalize e' verb 
;;

let db_normalize_at (e : db_term) (pos : int list) (verb : bool) : db_term = 
  db_navigate e pos (fun x -> db_normalize x verb) 
;;

