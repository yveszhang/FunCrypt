{
module Lexer 
       ( Token (..) ,
         FcPosn ,
         fcLexing ,
	 getTokenPosn ,
	 fcPosnToString ,
       ) where
}

%wrapper "posn" 

$digit = 0-9
$letter = [a-zA-Z]

@ident = [a-z][a-zA-Z0-9'_]*
@identcap = [A-Z][a-zA-Z0-9'_]*
@bits = [01]+

tokens :-

$white+		;
"--".*		;
"->"		{ \p s -> ARROW (convertPosition p s) }
\(		{ \p s -> LPAREN (convertPosition p s) }
\)		{ \p s -> RPAREN (convertPosition p s) }
\<		{ \p s -> LPAIR (convertPosition p s) }
\> 		{ \p s -> RPAIR (convertPosition p s) }
\[		{ \p s -> LBOX (convertPosition p s) }
\]		{ \p s -> RBOX (convertPosition p s) }
"Import"	{ \p s -> IMPORT (convertPosition p s) }
"Type" 		{ \p s -> TYPE (convertPosition p s) }
"Parameter"	{ \p s -> PARAMETER (convertPosition p s) }
"Definition" 	{ \p s -> DEFINITION (convertPosition p s) }
"Hypothesis" 	{ \p s -> HYPOTHESIS (convertPosition p s) }
"Bits" 		{ \p s -> BITS (convertPosition p s) }
"func"		{ \p s -> FUNC (convertPosition p s) }
"let" 		{ \p s -> LET (convertPosition p s) }
"in" 		{ \p s -> IN (convertPosition p s) }
"first"		{ \p s -> FIRST (convertPosition p s) }
"second" 	{ \p s -> SECOND (convertPosition p s) }
"B0"		{ \p s -> ZERO (convertPosition p s) }
"B1"		{ \p s -> ONE (convertPosition p s) }
"nil"		{ \p s -> NIL (convertPosition p s) }
"srec"		{ \p s -> SREC (convertPosition p s) }
"case" 		{ \p s -> CASE (convertPosition p s) }
"if"		{ \p s -> IF (convertPosition p s) }
"then"		{ \p s -> THEN (convertPosition p s) }
"else"		{ \p s -> ELSE (convertPosition p s) }
"bind"		{ \p s -> BIND (convertPosition p s) }
"return"	{ \p s -> RETURN (convertPosition p s) }
"rand"		{ \p s -> RAND (convertPosition p s) }
"lethypo"	{ \p s -> LETHYPO (convertPosition p s) }
\:		{ \p s -> COLON (convertPosition p s) }
\;		{ \p s -> SEMICOLON (convertPosition p s) }
\,		{ \p s -> COMMA (convertPosition p s) }
\$		{ \p s -> DOLLAR (convertPosition p s) }
\&		{ \p s -> AND (convertPosition p s) }
\*		{ \p s -> STAR (convertPosition p s) }
\=		{ \p s -> EQUAL (convertPosition p s) }
@ident 		{ \p s -> IDENT (convertPosition p s) s }
@identcap 	{ \p s -> IDENTCAP (convertPosition p s) s }
@bits		{ \p s -> BITSTRING (convertPosition p s) s }
"End"		{ \p s -> END (convertPosition p s) }

{
type FcPosn = ((Int, Int), (Int, Int)) 

fcPosnToString :: FcPosn -> String 
fcPosnToString ((line1, col1), (line2, col2)) = if line1 == line2 
     	      	     	     	      then "[ " ++ Prelude.show line1 ++ ": " ++ Prelude.show col1 ++ "--" ++ Prelude.show col2 ++ " ]"
				      else "[ " ++ Prelude.show line1 ++ ":" ++ Prelude.show col1 ++ " -- " ++ Prelude.show line2 ++ ":" ++ Prelude.show col2 ++ " ]"

data Token = ARROW FcPosn
     	   | LPAREN FcPosn
	   | RPAREN FcPosn
	   | LPAIR FcPosn
	   | RPAIR FcPosn
	   | LBOX FcPosn
	   | RBOX FcPosn
	   | IMPORT FcPosn
	   | TYPE FcPosn
	   | PARAMETER FcPosn
	   | DEFINITION FcPosn
	   | HYPOTHESIS FcPosn
	   | BITS FcPosn
	   | FUNC FcPosn
	   | LET FcPosn
	   | IN FcPosn
	   | FIRST FcPosn
	   | SECOND FcPosn
	   | ZERO FcPosn
	   | ONE FcPosn
	   | NIL FcPosn
	   | SREC FcPosn
	   | CASE FcPosn
	   | IF FcPosn
	   | THEN FcPosn
	   | ELSE FcPosn
	   | BIND FcPosn
	   | RETURN FcPosn
	   | RAND FcPosn
	   | LETHYPO FcPosn
	   | COLON FcPosn
	   | SEMICOLON FcPosn
	   | COMMA FcPosn
	   | DOLLAR FcPosn
	   | AND FcPosn
	   | STAR FcPosn
	   | EQUAL FcPosn
	   | IDENT  FcPosn String
	   | IDENTCAP FcPosn String
	   | BITSTRING FcPosn String
	   | LEXERROR FcPosn
	   | FCSTART
	   | FCEND
	   deriving  (Eq, Show)

convertPosition :: AlexPosn -> String -> FcPosn
convertPosition (AlexPn _ line col) s = ((line, col), (line, col + length s))

getTokenPosn :: Token -> FcPosn
getTokenPosn (ARROW p) = p
getTokenPosn (LPAREN p) = p
getTokenPosn (RPAREN p) = p
getTokenPosn (LPAIR p) = p
getTokenPosn (RPAIR p) = p
getTokenPosn (LBOX p) = p
getTokenPosn (RBOX p) = p
getTokenPosn (IMPORT p) = p
getTokenPosn (TYPE p) = p
getTokenPosn (PARAMETER p) = p
getTokenPosn (DEFINITION p) = p
getTokenPosn (HYPOTHESIS p) = p
getTokenPosn (BITS p) = p
getTokenPosn (FUNC p) = p
getTokenPosn (LET p) = p
getTokenPosn (IN p) = p
getTokenPosn (FIRST p) = p
getTokenPosn (SECOND p) = p
getTokenPosn (ZERO p) = p
getTokenPosn (ONE p) = p
getTokenPosn (NIL p) = p
getTokenPosn (SREC p) = p
getTokenPosn (CASE p) = p
getTokenPosn (IF p) = p
getTokenPosn (THEN p) = p
getTokenPosn (ELSE p) = p
getTokenPosn (BIND p) = p
getTokenPosn (RETURN p) = p
getTokenPosn (RAND p) = p
getTokenPosn (LETHYPO p) = p
getTokenPosn (COLON p) = p
getTokenPosn (SEMICOLON p) = p
getTokenPosn (COMMA p) = p
getTokenPosn (DOLLAR p) = p
getTokenPosn (AND p) = p
getTokenPosn (STAR p) = p
getTokenPosn (EQUAL p) = p
getTokenPosn (IDENT p _) = p 
getTokenPosn (IDENTCAP p _) = p 
getTokenPosn (BITSTRING p _) = p 
getTokenPosn (LEXERROR p) = p
getTokenPosn FCSTART = ((-1, -1), (0, 0))
getTokenPosn FCEND = ((-1, -1), (-1, -1))

fcLexing :: String -> [Token]
fcLexing s = (FCSTART : alexScanTokens s) ++ [FCEND]
}