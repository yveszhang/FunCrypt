%{
  open Types
  open Debug
  open Lexing
  
  exception FC_parse_error of string

  let parse_error s = 
    let _line = Parsing.symbol_start_pos () in
    let _pos = Parsing.rhs_start_pos 1 in
    let start_pos = _pos.pos_cnum - _pos.pos_bol in
    print_endline (s ^ "Line " ^ (string_of_int _line.pos_lnum) ^ ":" ^ (string_of_int start_pos))
  ;;
      
  let rec build_func (aseq: (fc_position * fc_argument) list) (func_def: fc_program) : fc_program =
    match aseq with 
      |	[] -> func_def
      | (pos, arg) ::aseq' -> 
	P_func ({pos with end_pos = (get_prog_pos func_def).end_pos}, (pos, arg, build_func aseq' func_def))
  ;;

  let compute_prog_pos (s : int) (e : int) : fc_position = 
    (* s is the index of the start symbol, e is the index of the end symbol *)
    let spos = Parsing.rhs_start_pos s in 
    let epos = Parsing.rhs_end_pos e in 
    {start_pos = (spos.pos_lnum, spos.pos_cnum - spos.pos_bol); 
     end_pos = (epos.pos_lnum, epos.pos_cnum - epos.pos_bol)} 
  ;;
  
%}
%token TYPE IMPORT PARAMETER DEFINITION HYPOTHESIS
%token <string> IDENT BS
%token <char> LEXERROR
%token LPAREN RPAREN 
%token LPAIR RPAIR /* for parenthesis < > */
%token LBOX RBOX /* for parentheis [ ] */
%token COLON SEMICOLON COMMA DOLLAR EQUAL STAR AND ARROW
%token BITS ZERO ONE NIL SREC CASE IF THEN ELSE
%token FUNC LET IN FIRST SECOND 
%token BIND RET RAND LETHYPO
%token END EOF
%right ARROW 
%left STAR AND
%start fc_file fc_prog 
%type <Types.fc_parsing_result> fc_file
%type <Types.fc_program> fc_prog fc_app_prog fc_pair
%type <cslr_type> fc_type 
%type <aspect * cslr_type> fc_argument
%type <(fc_position * fc_argument) list> fc_argseq
%%
fc_file:
    | EOF 
	{{typedef=[]; vardef=[]; progdef=[]; hypodef=[]; warnings=[]}}
    | TYPE IDENT EQUAL fc_type fc_file 
	{
	  let pos = compute_prog_pos 1 4 in 
	  {$5 with 
	    typedef = {default_entry with e_pos=pos; e_name=$2; e_typ=$4} :: $5.typedef
	  }
	}
    | PARAMETER IDENT COLON fc_argument fc_file
	{
	  let pos = compute_prog_pos 1 4 in 
	  {$5 with 
	    vardef = {default_entry with e_pos=pos; e_name=$2; e_asp=(fst $4); e_typ=(snd $4)} :: $5.vardef
	  }
	}
    | DEFINITION IDENT EQUAL fc_prog fc_file
	{
	  let pos = compute_prog_pos 1 4 in 
	  {$5 with 
	    progdef = {default_entry with e_pos=pos; e_name=$2; e_asp=Arrow; e_prog=$4} :: $5.progdef
	  }
	}
    | HYPOTHESIS IDENT COLON fc_prog EQUAL EQUAL fc_prog fc_file
	{
	  let pos1 = compute_prog_pos 4 4 in 
	  let pos2 = compute_prog_pos 7 7 in 
	  let name_1 = $2 ^ "{left}" in 
	  let name_2 = $2 ^ "{right}" in 
	  {$8 with 
	    hypodef = {default_entry with e_pos = pos1; e_name = name_1; e_prog = $4} :: 
	      ({default_entry with e_pos = pos2; e_name = name_2; e_prog = $7} :: $8.hypodef)
	  }
	}
;
fc_type: 
  | BITS 
      { T_bits }
  | IDENT 
      { T_var $1 }
  | fc_type ARROW fc_type 
      {	T_func ($1, Arrow, $3) }
  | LPAIR fc_type RPAIR ARROW fc_type 
      { T_func ($2, Linear, $5) }
  | LBOX fc_type RBOX ARROW fc_type 
      { T_func ($2, Modal, $5) }
  | fc_type AND fc_type 
      { T_prod ($1, $3) }
  | fc_type STAR fc_type 
      { T_tensor ($1, $3) }
  | DOLLAR fc_type 
      { T_proba ($2) }
  | LPAREN fc_type RPAREN 
      { $2 } 
;
fc_argseq:
  | { [] }
  | LPAREN IDENT COLON fc_argument RPAREN fc_argseq 
      {
	let arg = {str=$2; asp=(fst $4); typ=(snd $4)} in 
	let pos = compute_prog_pos 1 5 in 
	(pos, arg) :: $6
      }
;
fc_argument:
  | fc_type {(Arrow,  $1)}
  | LPAIR fc_type RPAIR {(Linear,  $2)}
  | LBOX fc_type RBOX {(Modal,  $2)}
;
fc_app_prog: /* Defines applicable prgrams --- those can be applied to another program. */
  | IDENT 
      {	let pos = compute_prog_pos 1 1 in P_var (pos, $1) }
  | ZERO 
      {	let pos = compute_prog_pos 1 1 in P_zero pos }
  | ONE 
      { let pos = compute_prog_pos 1 1 in P_one pos } 
  | SREC fc_type 
      { let pos = compute_prog_pos 1 2 in P_srec (pos, $2) }      
    /* srec <type> */
  | CASE fc_type 
      { let pos = compute_prog_pos 1 2 in P_case (pos, $2) }      
    /* case <type> */
  | fc_app_prog fc_prog 
      { let pos = compute_prog_pos 1 2 in P_app (pos, ($1, $2)) }
      /* <prog> <prog> */
  | LPAREN fc_prog RPAREN 
      { P_paren $2}
      /* ( <prog> ) */
      /* Keeping the parenthesis is necessary for dealing with the order of applications. */
;
fc_pair:
  | fc_prog COMMA fc_prog 
      { let pos = compute_prog_pos 1 3 in P_pair (pos, ($1, $3)) }
      /* < <prog> , <prog> > */
  | fc_pair COMMA fc_prog
      { let pos = compute_prog_pos 1 3 in P_pair (pos, ($1, $3)) }
      /* < <prog> , ... , <prog> > */
;
fc_prog: 
  | fc_app_prog 
      { $1 }
  | NIL 
      { let pos = compute_prog_pos 1 1 in P_nil pos }
  | IF fc_prog THEN fc_prog ELSE fc_prog 
      { let pos = compute_prog_pos 1 6 in P_if (pos, ($2, $4, $6)) }
      /* if <prog> then <prog> else <prog> */
  | FUNC fc_argseq ARROW fc_prog 
      { build_func $2 $4 }
      /* func (<id> : <type>) ... (<id> : <type>) -> <prog> */
  | LET IDENT EQUAL fc_prog IN fc_prog 
      { let pos = compute_prog_pos 1 6 in P_let (pos, ($2, $4, $6))}
      /* let <id> = <prog> in <prog> */
  | LPAIR fc_pair RPAIR 
      { let pos = compute_prog_pos 1 2 in 
	match $2 with | P_pair (_, p) -> P_pair(pos, p) | _ -> $2 
      }
  | FIRST fc_prog 
      { let pos = compute_prog_pos 1 2 in P_fst (pos, $2) }
      /* first <prog> */
  | SECOND fc_prog 
      { let pos = compute_prog_pos 1 2 in P_snd (pos, $2) }
      /* second <prog> */
  | fc_prog STAR fc_prog 
      { let pos = compute_prog_pos 1 3 in P_tensor (pos, ($1, $3))}
      /* <prog> * <prog> */  
  | LET LPAREN IDENT STAR IDENT RPAREN EQUAL fc_prog IN fc_prog 
      { let pos = compute_prog_pos 1 10 in P_tslet (pos, ($3, $5, $8, $10))}
      /* let (<id> * <id>) = <prog> in <prog> */
  | RAND 
      { let pos = compute_prog_pos 1 1 in P_rand pos }
  | RET fc_prog 
      { let pos = compute_prog_pos 1 2 in P_ret (pos, $2) }
      /* return <prog> */
  | BIND IDENT EQUAL fc_prog IN fc_prog 
      { let pos = compute_prog_pos 1 6 in P_bind (pos, ($2, $4, $6)) }
      /* bind <id> = <prog> in <prog> */
  | LETHYPO IDENT fc_argseq EQUAL fc_prog EQUAL EQUAL fc_prog IN fc_prog 
      { let pos = compute_prog_pos 1 10 in 
	let arg_num = List.length $3 in 
	P_hypo (pos, arg_num, ($2, (build_func $3 $5, build_func $3 $8), $10)) }
;
%%
  let parse_error s = print_endline s; flush stdout ;;
  Parsing.set_trace false ;;
  let _debug = false ;;

