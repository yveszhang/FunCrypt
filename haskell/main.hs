import System.IO
import System.Environment

import FCSyntax
import Lexer
import Parser

main = do args <- getArgs
          readFile (head args) >>= (printParseEntries . fcParsing .fcLexing)