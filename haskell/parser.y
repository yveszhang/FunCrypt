{
module Parser 
       ( printParseEntries ,
         fcParsing ,
       ) where 

import System.IO
import Control.Monad

import FCSyntax
import Lexer
}

%name fcParsing
%tokentype { Token }
%error { parseError }

%token
  "->"		{ ARROW $$ }
  '('		{ LPAREN $$ }
  ')'		{ RPAREN $$ }
  '<'		{ LPAIR $$ }
  '>'		{ RPAIR $$ }
  '['		{ LBOX $$ }
  ']'		{ RBOX $$ }
  "import"	{ IMPORT $$ }
  "type" 	{ TYPE $$ }
  "param" 	{ PARAMETER $$ }
  "def"		{ DEFINITION $$ }
  "hypo"	{ HYPOTHESIS $$ }
  "Bits"	{ BITS $$ }
  "func" 	{ FUNC $$ }
  "let" 	{ LET $$ }
  "in"		{ IN $$ }
  "fst"		{ FIRST $$ }
  "snd"		{ SECOND $$ }
  "0"		{ ZERO $$ }
  "1"		 { ONE $$ }
  "nil"		 { NIL $$ }
  "srec"	 { SREC $$ }
  "case"	 { CASE $$ }
  "if"		 { IF $$ }
  "then"	 { THEN $$ }
  "else"	 { ELSE $$ }
  "bind"	 { BIND $$ }
  "ret"		 { RETURN $$ }
  "rand"	 { RAND $$ }
  "lethypo"	 { LETHYPO $$ }
  ':'		 { COLON $$ }
  ';'		 { SEMICOLON $$ }
  ','		 { COMMA $$ }
  '$'		 { DOLLAR $$ }
  '&'		 { AND $$ }
  '*'		 { STAR $$ }
  '='		 { EQUAL $$ }
  "id"		 { IDENT p s } 
  "idcap"	 { IDENTCAP p s } 
  "bs"		 { BITSTRING p s } 
  "fcend"	 { FCEND }
  "fcstart" 	 { FCSTART }

%%

fcFile :: { [ParseEntry] }
       : "fcstart" { [] }
       | fcFile "type" "idcap" '=' fcType 
       	 	{ let pos = (fst $2, snd $ getTypePosn $5) in 
		  let IDENTCAP _ s = $3 in 
		  $1 ++ [ TypeDef pos (s, $5) ] 
		}
       | fcFile "param" "id" ':' fcAspectType
       	 	{ let (atPos, atAspect, atType) = $5 in 
		  let pos = (fst $2, snd $ getTypePosn atType) in 
		  let IDENT _ s = $3 in 
		  $1 ++ [ VarDef pos (s, atAspect, atType) ] 
		}
       | fcFile "def" "id" '=' fcProgram
       	 	{ let pos = (fst $2, snd $ getProgPosn $5) in 
		  let IDENT _ s = $3 in 
		  $1 ++ [ ProgDef pos (s, $5) ] 
		}
       | fcFile "hypo" "id" ':' fcProgram '=' '=' fcProgram 
       	 	{ let pos = (fst $2, snd $ getProgPosn $8) in 
		  let IDENT _ s = $3 in 
		  $1 ++ [ HypoDef pos (s, $5, $8) ] 
		}

fcType :: { TypePosn }
       : "Bits"
	 { TPbits $1 }		
       | "idcap"
	 { let IDENTCAP p s = $1 in TPvar p s }
       | fcType "->" fcType
       	 { let pos = (fst $ getTypePosn $1, snd $ getTypePosn $3) in TPfunc pos ($1, Arrow, $3) }
       | '<' fcType '>' "->" fcType
       	 { let newPosn = (fst $1, snd $ getTypePosn $5) in TPfunc newPosn ($2, Linear, $5) }
       | '[' fcType ']' "->" fcType
       	 { let newPosn = (fst $1, snd $ getTypePosn $5) in TPfunc newPosn ($2, Modal, $5) }
       | fcType '&' fcType
       	 { let newPosn = (fst $ getTypePosn $1, snd $ getTypePosn $3) in TPprod newPosn ($1, $3) }
       | fcType '*' fcType
       	 { let newPosn = (fst $ getTypePosn $1, snd $ getTypePosn $3) in TPtensor newPosn ($1, $3) }
       | '$' fcType
       	 { let newPosn = (fst $1, snd $ getTypePosn $2) in TPproba newPosn $2 }
       | '(' fcType ')'
       	 { $2 }

fcArgs :: { [ArgumentPosn] }
       : "id" 
       	 { let IDENT p s = $1 in [(p, (s, Modal, TPx))] }
       | '(' "id" ':' fcType ')' 
       	 { let IDENT p s = $2 in [((fst $1, snd $5), (s, Modal, $4))] }
       | fcArgs "id"
       	 { let IDENT p s = $2 in $1 ++ [(p, (s, Modal, TPx))] }
       | fcArgs '(' "id" ':' fcType ')' 
       	 { let IDENT p s = $3 in $1 ++ [((fst $2, snd $6), (s, Modal, $5))] }

fcAspectType :: { (FcPosn, FcAspect, TypePosn) }
	   : fcType
	     { (getTypePosn $1, Arrow, $1) }
	   | '<' fcType '>'
	     { let newPosn = (fst $1, snd $3) in (newPosn, Linear, $2) }
	   | '[' fcType ']'
	     { let newPosn = (fst $1, snd $3) in (newPosn, Modal, $2) }

fcFunction :: { ProgramPosn }
	   : "id"
	     { let IDENT p s = $1 in PPvar p s }
	   | "0"
	     { PPzero $1 }
	   | "1"
	     { PPone $1 }
	   | "srec"
	     { PPsrec $1 }
	   | "case"
	     { PPcase $1 }
	   | fcFunction fcProgram
	     { let newPosn = (fst $ getProgPosn $1, snd $ getProgPosn $2) in PPapp newPosn ($1, $2) }
	   | '(' fcProgram ')'
	     { let newPosn = (fst $1, snd $3) in $2 }

FcTuple :: { ProgramPosn }
	: fcProgram ',' fcProgram
	  { let newPosn = (fst $ getProgPosn $1, snd $ getProgPosn $3) in PPpair newPosn ($1, $3) }
	| FcTuple ',' fcProgram
	  { let newPosn = (fst $ getProgPosn $1, snd $ getProgPosn $3) in PPpair newPosn ($1, $3) }

fcProgram :: { ProgramPosn }
	  : fcFunction
	    { $1 }
	  | "nil"
	    { PPnil $1 }
	  | "if" fcProgram "then" fcProgram "else" fcProgram
	    { let newPosn = (fst $1, snd $ getProgPosn $6) in PPif newPosn ($2, $4, $6) }
	  | "func" fcArgs "->" fcProgram
	    { let newPosn = (fst $1, snd $ getProgPosn $4) in makeFunction newPosn $2 $4 }
	  | "let" "id" '=' fcProgram "in" fcProgram
	    { let newPosn = (fst $1, snd $ getProgPosn $6) in
	      let IDENT _ s = $2 in 
	      PPlet newPosn (s, $4, $6)
	    }
	  | '<' FcTuple '>'
	    { let pos = (fst $1, snd $3) in
	      let PPpair _ progPair = $2 in 
	      PPpair pos progPair 
	    }
	  | "fst" fcProgram
	    { let newPosn = (fst $1, snd $ getProgPosn $2) in PPfst newPosn $2 }
	  | "snd" fcProgram
	    { let newPosn = (fst $1, snd $ getProgPosn $2) in PPsnd newPosn $2 }
	  | fcProgram '*' fcProgram
	    { let newPosn = (fst $ getProgPosn $1, snd $ getProgPosn $3) in PPtensor newPosn ($1, $3) }
	  | "let" "id" '*' "id" '=' fcProgram "in" fcProgram
	    { let newPosn = (fst $1, snd $ getProgPosn $8) in 
	      let IDENT _ s1 = $2 in 
	      let IDENT _ s2 = $4 in 
	      PPtslet newPosn (s1, s2, $6, $8)
	    }	  
	  | "rand"
	    { PPrand $1 }
	  | "ret" fcProgram
	    { let pos = (fst $1, snd $ getProgPosn $2) in PPret pos $2 }
	  | "bind" "id" '=' fcProgram "in" fcProgram
	    { let pos = (fst $1, snd $ getProgPosn $6) in 
	      let IDENT _ s = $2 in 
	      PPbind pos (s, $4, $6)
	    }

{
data ProgramPosn = PPvar FcPosn String
               	 | PPnil FcPosn 
               	 | PPone FcPosn
               	 | PPzero FcPosn
               	 | PPcase FcPosn
               	 | PPsrec FcPosn
               	 | PPif FcPosn (ProgramPosn, ProgramPosn, ProgramPosn)
               	 | PPfunc FcPosn (ArgumentPosn, ProgramPosn)
               	 | PPapp FcPosn (ProgramPosn, ProgramPosn)
               	 | PPlet FcPosn (String, ProgramPosn, ProgramPosn)
               	 | PPpair FcPosn (ProgramPosn, ProgramPosn)
               	 | PPfst FcPosn ProgramPosn
               	 | PPsnd FcPosn ProgramPosn
               	 | PPtensor FcPosn (ProgramPosn, ProgramPosn)
               	 | PPtslet FcPosn (String, String, ProgramPosn, ProgramPosn)
               	 | PPrand FcPosn 
               	 | PPret FcPosn ProgramPosn
               	 | PPbind FcPosn (String, ProgramPosn, ProgramPosn)
               	 | PPhypo FcPosn (String, (ProgramPosn, ProgramPosn), ProgramPosn)
               	 | PPparen FcPosn ProgramPosn
               	 deriving (Eq, Show) 

data TypePosn = TPvar FcPosn String 
              | TPbits FcPosn 
              | TPprod FcPosn (TypePosn, TypePosn) 
              | TPtensor FcPosn (TypePosn, TypePosn)
              | TPfunc FcPosn (TypePosn, FcAspect, TypePosn) 
              | TPproba FcPosn TypePosn
              | TPerror FcPosn String
              | TPx	-- type to be infered xs
              deriving (Eq, Show) 

type ArgumentPosn = (FcPosn, (String, FcAspect, TypePosn))

data ParseEntry = TypeDef FcPosn (String, TypePosn)
		| VarDef FcPosn (String, FcAspect, TypePosn)
		| ProgDef FcPosn (String, ProgramPosn)
		| HypoDef FcPosn (String, ProgramPosn, ProgramPosn)
		| Warning String
		deriving (Eq, Show) 

printParseEntries :: [ParseEntry] -> IO ()
printParseEntries [] = return ()
printParseEntries (e : es) = putStrLn (show e) >>= (\_ -> printParseEntries es)

getTypePosn :: TypePosn -> FcPosn
getTypePosn (TPvar p _) = p
getTypePosn (TPbits p) = p
getTypePosn (TPprod p _) = p
getTypePosn (TPtensor p _) = p
getTypePosn (TPfunc p _) = p
getTypePosn (TPproba p _) = p
getTypePosn (TPerror p _) = p
getTypePosn (TPx) = ((-1, -1), (-1, -1))

getProgPosn :: ProgramPosn -> FcPosn
getProgPosn (PPvar p _) = p
getProgPosn (PPnil p) = p 
getProgPosn (PPone p) = p
getProgPosn (PPzero p) = p
getProgPosn (PPcase p) = p
getProgPosn (PPsrec p) = p
getProgPosn (PPif p _) = p
getProgPosn (PPfunc p _) = p
getProgPosn (PPapp p _) = p
getProgPosn (PPlet p _) = p
getProgPosn (PPpair p _) = p
getProgPosn (PPfst p _) = p
getProgPosn (PPsnd p _) = p
getProgPosn (PPtensor p _) = p
getProgPosn (PPtslet p _) = p
getProgPosn (PPrand p) = p 
getProgPosn (PPret p _) = p
getProgPosn (PPbind p _) = p
getProgPosn (PPhypo p _) = p
getProgPosn (PPparen p _) = p

makeFunction :: FcPosn -> [ArgumentPosn] -> ProgramPosn -> ProgramPosn
makeFunction posn [] prog = prog 
makeFunction posn (arg : args) prog = PPfunc posn (arg, makeFunction nextPosn args prog)
	     where nextPosn = if args == [] then posn else (fst (fst $ head args), snd posn)

parseError :: [Token] -> a
parseError [] = error "Parse error"
parseError (tok : toks) = error $ "Parse error" ++ position ++ ": " ++ info
  where position = if tok == FCEND then "" else " @" ++ (fcPosnToString . getTokenPosn) tok
        info = case tok of FCEND -> "Parsing reaches the end."
                           IDENT _ _ -> "If it's a type variable, it must start with a capital."
                           IDENTCAP _ _ -> "If it's not a type variable, it must start with a non-capital."
}
