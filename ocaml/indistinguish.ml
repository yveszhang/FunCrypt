open Types
open Debruijn
open Betareduce

let rec db_equiv (e1 : db_term) (e2 : db_term) : bool =
  if (db_alpha_equal e1 e2) || (db_alpha_equal (beta_reduce e1) e2)
    || (db_alpha_equal e1 (beta_reduce e2))
  then true
  else
    if (db_alpha_equal (db_normalize e1 false) (db_normalize e2 false))
    then true
    else
      match (e1, e2) with
	| (DB_app (DB_app (DB_case _, _), DB_pair (e11, DB_pair (DB_func (_, e12), DB_func (_, e13)))), e2') ->
	  (db_alpha_equal e11 e2') && (db_alpha_equal e12 (db_lift e2')) && (db_alpha_equal e13 (db_lift e2'))
	| (DB_pair (DB_fst e11, DB_snd e12), e2') 	
	| (DB_tensor (DB_tslet (_, _, e11, DB_idx 1), DB_tslet (_, _, e12, DB_idx 0)), e2') -> 
	  (db_alpha_equal e11 e2') && (db_alpha_equal e12 e2')
	| (DB_if (e10, e11, e12), DB_if (e20, e21, e22)) -> 
	  (db_equiv e10 e20) && (db_equiv e11 e21) && (db_equiv e12 e22)
	| (DB_func (_, e10), DB_func (_, e20))
	| (DB_hypo (_, _, _, e10), DB_hypo (_, _, _, e20))
	| (DB_fst e10, DB_fst e20) | (DB_snd e10, DB_snd e20) | (DB_ret e10, DB_ret e20) ->
	  db_equiv e10 e20
	| (DB_app (e11, e12), DB_app (e21, e22)) | (DB_pair (e11, e12), DB_pair (e21, e22)) 
	| (DB_tensor (e11, e12), DB_tensor (e21, e22)) | (DB_tslet (_, _, e11, e12), DB_tslet (_, _, e21, e22))
	| (DB_let (_, e11, e12), DB_let (_, e21, e22)) | (DB_bind (_, e11, e12), DB_bind (_, e21, e22))-> 
	  (db_equiv e11 e21) && (db_equiv e12 e22)
	| _ -> false 
;;

