{
  open Parser
  open Lexing 
  
  let increment_line_no n lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + n;
      pos_bol = pos.pos_cnum;
    }
  ;;
  
  let newline_count (s : string) : int = 
    let rec count_from (i : int) = 
      try let i' = String.index_from s i '\n'in (count_from (i'+1)) + 1
      with Not_found | Invalid_argument _ -> 0 
    in count_from 0
    ;;
}

let comment = ([^'(']*) | (([^ '(']* '(' [^ '*'])+ [^'(']*)

rule token = parse
    [' ' '\t'] {token lexbuf}
  | ['\n'] {increment_line_no 1 lexbuf; token lexbuf}
  | "->" {ARROW}
  | '(' {LPAREN}
  | ')' {RPAREN}
  | '<' {LPAIR}
  | '>' {RPAIR}
  | '[' {LBOX}
  | ']' {RBOX}
  | "Import" {IMPORT}
  | "Type" {TYPE}
  | "Parameter" {PARAMETER}
  | "Definition" {DEFINITION}
  | "Hypothesis" {HYPOTHESIS}
  | "bits" {BITS}
  | "func" {FUNC}
  | "let" {LET}
  | "in" {IN}
  | "first" {FIRST}
  | "second" {SECOND}
  | "B0" {ZERO}
  | "B1" {ONE}
  | "nil" {NIL}
  | "srec" {SREC}
  | "case" {CASE}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "bind" {BIND}
  | "return" {RET}
  | "rand" {RAND}
  | "lethypo" {LETHYPO}
  | ':' {COLON}
  | ';' {SEMICOLON}
  | ',' {COMMA}
  | '$' {DOLLAR}
  | '&' {AND}
  | '*' {STAR}
  | '=' {EQUAL}
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* ['\'']? as id {IDENT id}
  | ['0' '1']+ as bs {BS bs}
  | "End" {END}
  | "(*" comment "*)" as c { increment_line_no (newline_count c) lexbuf; token lexbuf}
  | _ as c {LEXERROR c}
  | eof {EOF}
