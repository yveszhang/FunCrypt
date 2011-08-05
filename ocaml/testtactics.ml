open Common
open Types
open Fcparsing
open Environment
open Debruijn
open Betareduce
open Bind
open Tactic

let test () =

  let env = build_fc_environment (parse_file "elgamal5.cslr" false) false in

  let prog = env.prog in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------" in

  let prog = List.fold_left (fun e id -> beta_reduce_at e (db_position id e)) prog
    ["keygen"; "pk_sk"; "pk_sk"; "pk"; "sk"; "m0_m1"; "m0"; "m1"; "A2"; "encrypt"] in

  let prog = beta_reduce_at prog (db_position "m0_m1_A2" prog @ [0; 1]) in

  let prog = beta_reduce_at prog (db_position "c" prog @ [0; 0]) in
  let prog = beta_reduce_at prog (db_position "c" prog @ [0]) in
  let prog = beta_reduce_at prog (db_position "c" prog) in
  let prog = beta_reduce_at prog (db_position "c" prog) in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "A---------------------------------------------------------------------" in

  let prog = move_bind_up prog (db_position "semantic_security" prog @ [0;0;1;1;1;2]) in
  let prog = move_bind_up prog (db_position "semantic_security" prog @ [0;0;1;1;1]) in
  let prog = move_bind_up prog (db_position "semantic_security" prog @ [0;0;1;1]) in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "B---------------------------------------------------------------------" in

  let prog = swap_bind_at prog (db_position "m0_m1_A2" prog) in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------" in

  let prog = beta_reduce_at prog (db_position "b2" prog @ [0;1;1;0;1;0;1]) in

(*
  let prog = format prog (db_position "DDH" prog) (db_position "semantic_security" prog @ [0;0])
     [DB_nil] in
*)

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------" in

  let prog = change_at prog (db_position "semantic_security" prog @ [0;0])
   "bind ddh_left =
      bind x = Zstar in
      bind y = Zstar in
      return <power generator x, power generator y, power (power generator x) y> in
    bind b = rand in
    bind m0_m1_A2 = A ( first (first ddh_left) ) in
    lethypo mult_permutation_adversary = 
      bind x = carrier in return (mult ( x ) ( if b then second (first (m0_m1_A2)) else first (first (m0_m1_A2)) ))
     == 
      carrier in
    bind b2 = second m0_m1_A2  
    < second (first ddh_left)  ,
      mult ( second ddh_left ) ( if b then second (first (m0_m1_A2)) else first (first (m0_m1_A2)) ) > in
    let b2' = case bits b2 <B0 nil, <func (x : bits) -> B1 nil, func (x : bits) -> B1 nil>> in
    return (equal ( b2' ) ( b ))" in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------" in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------"; exit 0 in

  let prog = apply prog (db_position "DDH" prog) in

  let prog = clear_hypo prog (db_position "DDH" prog) in

  let prog = List.fold_left (fun e id -> beta_reduce_at e (db_position id e)) prog
    ["ddh_left"; "ddh_left"; "ddh_left"; "ddh_left"] in

  let prog = beta_reduce_at prog (db_position "b2" prog @ [0;1;0;0]) in
  let prog = beta_reduce_at prog (db_position "b2" prog @ [0;1;0]) in

  let prog = beta_reduce_at prog (db_position "b2" prog @ [0;1;1;0;1;0]) in
  let prog = beta_reduce_at prog (db_position "b2" prog @ [0;1;1;0;1]) in

  let prog = move_bind_up prog (db_position "b" prog) in
  let prog = move_bind_up prog (db_position "b" prog) in

(*
  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore (db_extract prog (db_position "z" prog)))) in exit 0;
*)

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------" in

  let prog = change_at prog (db_position "z" prog)
  "bind power_permutation_left =
     bind x = Zstar in return power ( generator )  ( x ) in
    bind m0_m1_A2 = A ( first (first (< < power ( generator )  ( x )  , power ( generator )  ( y )  > , power_permutation_left  >)) ) in
    lethypo mult_permutation_adversary = 
      bind x = carrier in
      return (mult ( x ) ( if b then second (first (m0_m1_A2)) else first (first (m0_m1_A2)) ))
     == 
      carrier
    in 
    bind b2 = second (m0_m1_A2) ( 
    < power ( generator ) ( y ) ,
      mult ( power_permutation_left ) ( if b then second (first (m0_m1_A2)) else first (first (m0_m1_A2)) ) > ) in
    let b2' = case bits b2 <B0 nil, <func (x : bits) -> B1 nil, func (x : bits) -> B1 nil>> in
    return (equal ( b2' ) ( b ))" in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------" in

  let prog = apply prog (db_position "power_permutation" prog) in

  let prog = clear_hypo prog (db_position "power_permutation" prog) in

  let prog = db_normalize_at prog (db_position "m0_m1_A2" prog) false in

  let prog = move_bind_down prog (db_position "power_permutation_left" prog) in
  let prog = move_bind_down prog (db_position "power_permutation_left" prog) in

  let prog = change_at prog (db_position "power_permutation_left" prog)
  "bind power_permutation_left = 
     bind x = carrier in return (mult ( x )  ( if b then second (first (m0_m1_A2)) else first (first (m0_m1_A2)) )) in
    bind b2 = second (m0_m1_A2) ( 
    < power ( generator ) ( y ) ,
       power_permutation_left > ) in
    let b2' = case bits b2 <B0 nil, <func (x : bits) -> B1 nil, func (x : bits) -> B1 nil>> in
    return (equal ( b2' ) ( b ))" in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------" in

(*  let prog = apply_with prog (db_position "mult_permutation" prog) [string_to_fc_term "if b then second (first (m0_m1_A2)) else first (first (m0_m1_A2))"] in *)
(*  let prog = apply prog (db_position "mult_permutation_adversary" prog) in*)

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in exit 0;
(* apply_with does not work here: binded varaibles (e.g., b) become free when instantiated and the hypo do not match target program. *)
  let _ = print_endline "A----------------------------------------------------------------------" in
  
  let prog = clear_hypo prog (db_position "mult_permutation" prog) in

  let prog = apply prog (db_position "mult_permutation_adversary" prog) in 

  let prog = clear_hypo prog (db_position "mult_permutation_adversary" prog) in

  let prog = move_bind_down prog (db_position "b" prog) in
  let prog = move_bind_down prog (db_position "b" prog) in
  let prog = move_bind_down prog (db_position "b" prog) in
  let prog = move_bind_down prog (db_position "b" prog) in
  let prog = move_bind_down prog (db_position "b" prog) in

  let prog = change_at prog (db_position "semantic_security" prog @ [0;0])
  " bind x = 
      bind x = Zstar in
      bind y = Zstar in
      bind m0_m1_A2 = A ( power ( generator ) ( x ) ) in
      bind power_permutation_left = carrier in
      second (m0_m1_A2) ( < power ( generator )  ( y )  , power_permutation_left > ) 
    in
    let b2' = case (bits) ( x ) ( < B0 ( nil )  , < func (x : bits) -> B1 ( nil )  , func (x : bits) -> B1 ( nil )  > > ) in
    bind b = rand in return equal ( b2' )  ( b ) "
  in

  let _ = print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog)) in
  let _ = print_endline "----------------------------------------------------------------------" in

(*  let prog = apply_with prog (db_position "random_bit_equality" prog) 
    [string_to_fc_term 
     "bind x = Zstar in 
      bind y = Zstar in
      bind m0_m1_A2 = A ( power ( generator ) ( x ) ) in
      bind power_permutation_left = carrier in
      second (m0_m1_A2) ( < power ( generator )  ( y )  , power_permutation_left > ) 
     "] in
*)
print_endline (fc_term_to_pretty_string  (db_term_to_fc_term_ignore prog))

(*
open Common
open Types
open Fcparser
open Fcparsing
open Debruijn
open Inline
open Permute
;;

let a = fc_term_to_debruijn_term (parse_string ("let x = nil in func (y : bits) -> x"));;

let b = inline [0] a;;



let a = fc_term_to_debruijn_term (parse_string ("func (x:bits) -> let z = x in let y = mult x x in y"))
;;

let b = inline [0] a
;;

let _ = fc_term_to_string (debruijn_term_to_fc_term b)
;;

let _ =
  fc_term_to_string (debruijn_term_to_fc_term (
  permute (DB_var "QR_n") (fc_term_to_debruijn_term (parse_string "func (x:bits) -> mult x x")) [0] (fc_term_to_debruijn_term (parse_string "bind x = Zstar_n in plus (mult x x) x"))));;
*)
