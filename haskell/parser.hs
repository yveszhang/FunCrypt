{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Parser 
       ( printParseEntries ,
         fcParsing ,
       ) where 

import System.IO
import Control.Monad

import FCSyntax
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.6

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: ([ParseEntry]) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ([ParseEntry])
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (TypePosn) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (TypePosn)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ([ArgumentPosn]) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ([ArgumentPosn])
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ((FcPosn, FcAspect, TypePosn)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ((FcPosn, FcAspect, TypePosn))
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (ProgramPosn) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (ProgramPosn)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (ProgramPosn) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (ProgramPosn)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (ProgramPosn) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (ProgramPosn)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xaa\x00\xaa\x00\x00\x00\x0b\x00\xa9\x00\xa8\x00\xa7\x00\xa6\x00\xa5\x00\xa1\x00\x95\x00\x9e\x00\x05\x00\x02\x00\x26\x00\x26\x00\x26\x00\x71\x00\x26\x00\x26\x00\x08\x00\x94\x00\x26\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x91\x00\x26\x00\x00\x00\x00\x00\x92\x00\x30\x00\x00\x00\xff\xff\xff\xff\xff\xff\x00\x00\xff\xff\x00\x00\x30\x00\xff\xff\xff\xff\x48\x00\x46\x00\xff\xff\xff\xff\xff\xff\x30\x00\x43\x00\x2e\x00\x2a\x00\x26\x00\x92\x00\x90\x00\xf7\xff\x8d\x00\x8d\x00\x6e\x00\x0c\x00\x8c\x00\x00\x00\xfd\xff\xf8\xff\x4d\x00\x8b\x00\x8a\x00\x26\x00\x00\x00\x26\x00\x00\x00\x26\x00\x89\x00\x26\x00\x86\x00\x00\x00\x85\x00\x26\x00\x26\x00\x26\x00\x82\x00\x00\x00\xa4\x00\xa3\x00\x30\x00\x30\x00\x30\x00\xa2\x00\xa0\x00\xff\xff\xff\xff\x4a\x00\xfe\xff\x49\x00\x7d\x00\x81\x00\x4b\x00\xff\xff\x4b\x00\x4b\x00\x4b\x00\x22\x00\xff\xff\x26\x00\x26\x00\x26\x00\x26\x00\x30\x00\x30\x00\x4b\x00\x4b\x00\x4b\x00\x38\x00\x1c\x00\x00\x00\x00\x00\x26\x00\x4b\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x9f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x8e\x00\x88\x00\x87\x00\x84\x00\x00\x00\x83\x00\x51\x00\x56\x00\x00\x00\x80\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9c\x00\x9b\x00\x9a\x00\x00\x00\x99\x00\x00\x00\x00\x00\x98\x00\x97\x00\x00\x00\x00\x00\x96\x00\x93\x00\x8f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x00\x00\x00\x74\x00\x00\x00\x73\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x6c\x00\x5f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x67\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x5e\x00\x5b\x00\x5a\x00\x57\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xe7\xff\xdf\xff\xe6\xff\xe5\xff\x00\x00\x00\x00\x00\x00\xd6\xff\xe9\xff\xfb\xff\xec\xff\xfc\xff\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\xf8\xff\xfd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd5\xff\x00\x00\x00\x00\xd9\xff\xda\xff\x00\x00\x00\x00\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\xe3\xff\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xee\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\xf1\xff\xeb\xff\xea\xff\xf3\xff\xf4\xff\xf7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\x00\x00\xe1\xff\xe2\xff\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\xf5\xff\xd4\xff\xde\xff\xdc\xff\x00\x00\x00\x00\xef\xff\xed\xff\x00\x00\xd7\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x05\x00\x04\x00\x02\x00\x06\x00\x04\x00\x02\x00\x06\x00\x04\x00\x02\x00\x06\x00\x0d\x00\x01\x00\x02\x00\x0d\x00\x19\x00\x04\x00\x0d\x00\x06\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x1a\x00\x21\x00\x01\x00\x24\x00\x24\x00\x01\x00\x21\x00\x03\x00\x01\x00\x22\x00\x24\x00\x01\x00\x22\x00\x03\x00\x27\x00\x22\x00\x02\x00\x27\x00\x04\x00\x01\x00\x27\x00\x03\x00\x26\x00\x01\x00\x01\x00\x01\x00\x26\x00\x05\x00\x0e\x00\x0f\x00\x2b\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x23\x00\x24\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x23\x00\x24\x00\x01\x00\x10\x00\x01\x00\x07\x00\x05\x00\x26\x00\x23\x00\x24\x00\x07\x00\x03\x00\x23\x00\x24\x00\x23\x00\x24\x00\x04\x00\x05\x00\x06\x00\x02\x00\x10\x00\x10\x00\x04\x00\x24\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x23\x00\x24\x00\x01\x00\x23\x00\x24\x00\x23\x00\x24\x00\x24\x00\x24\x00\x24\x00\x04\x00\x24\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x04\x00\x04\x00\x06\x00\x06\x00\x01\x00\x01\x00\x03\x00\x24\x00\x25\x00\x01\x00\x24\x00\x25\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x1f\x00\x01\x00\x25\x00\x01\x00\x01\x00\x01\x00\x24\x00\xff\xff\x1f\x00\xff\xff\xff\xff\x26\x00\x26\x00\xff\xff\x24\x00\xff\xff\x25\x00\x24\x00\x26\x00\xff\xff\x1f\x00\x25\x00\x24\x00\x26\x00\xff\xff\xff\xff\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x1f\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x26\x00\x26\x00\x26\x00\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x26\x00\x4a\x00\x2d\x00\x26\x00\x2e\x00\x27\x00\x26\x00\x28\x00\x2d\x00\x40\x00\x2e\x00\x29\x00\x4d\x00\x4e\x00\x29\x00\x52\x00\x10\x00\x29\x00\x78\x00\x05\x00\x06\x00\x07\x00\x08\x00\x6d\x00\x49\x00\x74\x00\x38\x00\x38\x00\x31\x00\x4b\x00\x77\x00\x68\x00\x2a\x00\x38\x00\x31\x00\x2a\x00\x76\x00\x2b\x00\x2a\x00\x13\x00\x2b\x00\x14\x00\x31\x00\x2b\x00\x55\x00\x41\x00\x31\x00\x6e\x00\x31\x00\x4f\x00\x56\x00\x15\x00\x16\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x32\x00\x33\x00\x1f\x00\x20\x00\x21\x00\x31\x00\x32\x00\x33\x00\x31\x00\x78\x00\x31\x00\x57\x00\x5b\x00\x22\x00\x32\x00\x33\x00\x5c\x00\x48\x00\x32\x00\x33\x00\x32\x00\x33\x00\x10\x00\x41\x00\x42\x00\x3e\x00\x6c\x00\x6e\x00\x10\x00\x38\x00\x70\x00\x10\x00\x10\x00\x71\x00\x72\x00\x10\x00\x10\x00\x73\x00\x5e\x00\x32\x00\x33\x00\x6f\x00\x32\x00\x33\x00\x32\x00\x33\x00\x38\x00\x38\x00\x38\x00\x10\x00\x38\x00\x5f\x00\x10\x00\x10\x00\x60\x00\x63\x00\x10\x00\x10\x00\x65\x00\x66\x00\x10\x00\x10\x00\x67\x00\x53\x00\x10\x00\x10\x00\x38\x00\x3a\x00\x10\x00\x10\x00\x3b\x00\x3c\x00\x10\x00\x10\x00\x43\x00\x45\x00\x10\x00\x10\x00\x11\x00\x22\x00\x23\x00\x57\x00\x24\x00\x50\x00\x51\x00\x58\x00\x38\x00\x45\x00\x59\x00\x2e\x00\x2f\x00\x33\x00\x34\x00\x35\x00\x36\x00\x2b\x00\x03\x00\x6a\x00\x5d\x00\x6b\x00\x5e\x00\x5d\x00\x5e\x00\x38\x00\x00\x00\x65\x00\x00\x00\x00\x00\x62\x00\x63\x00\x00\x00\x38\x00\x00\x00\x47\x00\x38\x00\x4c\x00\x00\x00\x0e\x00\x53\x00\x38\x00\x3a\x00\x00\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 43) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43)
	]

happy_n_terms = 44 :: Int
happy_n_nonterms = 7 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  happyIn4
		 ([]
	)

happyReduce_2 = happyReduce 5# 0# happyReduction_2
happyReduction_2 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (TYPE happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut5 happy_x_5 of { happy_var_5 -> 
	happyIn4
		 (let pos = (fst happy_var_2, snd $ getTypePosn happy_var_5) in 
		  let IDENTCAP _ s = happy_var_3 in 
		  happy_var_1 ++ [ TypeDef pos (s, happy_var_5) ]
	) `HappyStk` happyRest}}}}

happyReduce_3 = happyReduce 5# 0# happyReduction_3
happyReduction_3 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (PARAMETER happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_5 of { happy_var_5 -> 
	happyIn4
		 (let (atPos, atAspect, atType) = happy_var_5 in 
		  let pos = (fst happy_var_2, snd $ getTypePosn atType) in 
		  let IDENT _ s = happy_var_3 in 
		  happy_var_1 ++ [ VarDef pos (s, atAspect, atType) ]
	) `HappyStk` happyRest}}}}

happyReduce_4 = happyReduce 5# 0# happyReduction_4
happyReduction_4 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (DEFINITION happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_5 of { happy_var_5 -> 
	happyIn4
		 (let pos = (fst happy_var_2, snd $ getProgPosn happy_var_5) in 
		  let IDENT _ s = happy_var_3 in 
		  happy_var_1 ++ [ ProgDef pos (s, happy_var_5) ]
	) `HappyStk` happyRest}}}}

happyReduce_5 = happyReduce 8# 0# happyReduction_5
happyReduction_5 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (HYPOTHESIS happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_5 of { happy_var_5 -> 
	case happyOut10 happy_x_8 of { happy_var_8 -> 
	happyIn4
		 (let pos = (fst happy_var_2, snd $ getProgPosn happy_var_8) in 
		  let IDENT _ s = happy_var_3 in 
		  happy_var_1 ++ [ HypoDef pos (s, happy_var_5, happy_var_8) ]
	) `HappyStk` happyRest}}}}}

happyReduce_6 = happySpecReduce_1  1# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { (BITS happy_var_1) -> 
	happyIn5
		 (TPbits happy_var_1
	)}

happyReduce_7 = happySpecReduce_1  1# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (let IDENTCAP p s = happy_var_1 in TPvar p s
	)}

happyReduce_8 = happySpecReduce_3  1# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (let pos = (fst $ getTypePosn happy_var_1, snd $ getTypePosn happy_var_3) in TPfunc pos (happy_var_1, Arrow, happy_var_3)
	)}}

happyReduce_9 = happyReduce 5# 1# happyReduction_9
happyReduction_9 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (LPAIR happy_var_1) -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_5 of { happy_var_5 -> 
	happyIn5
		 (let newPosn = (fst happy_var_1, snd $ getTypePosn happy_var_5) in TPfunc newPosn (happy_var_2, Linear, happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_10 = happyReduce 5# 1# happyReduction_10
happyReduction_10 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (LBOX happy_var_1) -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_5 of { happy_var_5 -> 
	happyIn5
		 (let newPosn = (fst happy_var_1, snd $ getTypePosn happy_var_5) in TPfunc newPosn (happy_var_2, Modal, happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_11 = happySpecReduce_3  1# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (let newPosn = (fst $ getTypePosn happy_var_1, snd $ getTypePosn happy_var_3) in TPprod newPosn (happy_var_1, happy_var_3)
	)}}

happyReduce_12 = happySpecReduce_3  1# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (let newPosn = (fst $ getTypePosn happy_var_1, snd $ getTypePosn happy_var_3) in TPtensor newPosn (happy_var_1, happy_var_3)
	)}}

happyReduce_13 = happySpecReduce_2  1# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (DOLLAR happy_var_1) -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (let newPosn = (fst happy_var_1, snd $ getTypePosn happy_var_2) in TPproba newPosn happy_var_2
	)}}

happyReduce_14 = happySpecReduce_3  1# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_2
	)}

happyReduce_15 = happySpecReduce_1  2# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (let IDENT p s = happy_var_1 in [(p, (s, Modal, TPx))]
	)}

happyReduce_16 = happyReduce 5# 2# happyReduction_16
happyReduction_16 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (LPAREN happy_var_1) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { (RPAREN happy_var_5) -> 
	happyIn6
		 (let IDENT p s = happy_var_2 in [((fst happy_var_1, snd happy_var_5), (s, Modal, happy_var_4))]
	) `HappyStk` happyRest}}}}

happyReduce_17 = happySpecReduce_2  2# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (let IDENT p s = happy_var_2 in happy_var_1 ++ [(p, (s, Modal, TPx))]
	)}}

happyReduce_18 = happyReduce 6# 2# happyReduction_18
happyReduction_18 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (LPAREN happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut5 happy_x_5 of { happy_var_5 -> 
	case happyOutTok happy_x_6 of { (RPAREN happy_var_6) -> 
	happyIn6
		 (let IDENT p s = happy_var_3 in happy_var_1 ++ [((fst happy_var_2, snd happy_var_6), (s, Modal, happy_var_5))]
	) `HappyStk` happyRest}}}}}

happyReduce_19 = happySpecReduce_1  3# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ((getTypePosn happy_var_1, Arrow, happy_var_1)
	)}

happyReduce_20 = happySpecReduce_3  3# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (LPAIR happy_var_1) -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (RPAIR happy_var_3) -> 
	happyIn7
		 (let newPosn = (fst happy_var_1, snd happy_var_3) in (newPosn, Linear, happy_var_2)
	)}}}

happyReduce_21 = happySpecReduce_3  3# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (LBOX happy_var_1) -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (RBOX happy_var_3) -> 
	happyIn7
		 (let newPosn = (fst happy_var_1, snd happy_var_3) in (newPosn, Modal, happy_var_2)
	)}}}

happyReduce_22 = happySpecReduce_1  4# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (let IDENT p s = happy_var_1 in PPvar p s
	)}

happyReduce_23 = happySpecReduce_1  4# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { (ZERO happy_var_1) -> 
	happyIn8
		 (PPzero happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  4# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { (ONE happy_var_1) -> 
	happyIn8
		 (PPone happy_var_1
	)}

happyReduce_25 = happySpecReduce_1  4# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { (SREC happy_var_1) -> 
	happyIn8
		 (PPsrec happy_var_1
	)}

happyReduce_26 = happySpecReduce_1  4# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { (CASE happy_var_1) -> 
	happyIn8
		 (PPcase happy_var_1
	)}

happyReduce_27 = happySpecReduce_2  4# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn8
		 (let newPosn = (fst $ getProgPosn happy_var_1, snd $ getProgPosn happy_var_2) in PPapp newPosn (happy_var_1, happy_var_2)
	)}}

happyReduce_28 = happySpecReduce_3  4# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (LPAREN happy_var_1) -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (RPAREN happy_var_3) -> 
	happyIn8
		 (let newPosn = (fst happy_var_1, snd happy_var_3) in happy_var_2
	)}}}

happyReduce_29 = happySpecReduce_3  5# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (let newPosn = (fst $ getProgPosn happy_var_1, snd $ getProgPosn happy_var_3) in PPpair newPosn (happy_var_1, happy_var_3)
	)}}

happyReduce_30 = happySpecReduce_3  5# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (let newPosn = (fst $ getProgPosn happy_var_1, snd $ getProgPosn happy_var_3) in PPpair newPosn (happy_var_1, happy_var_3)
	)}}

happyReduce_31 = happySpecReduce_1  6# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_32 = happySpecReduce_1  6# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOutTok happy_x_1 of { (NIL happy_var_1) -> 
	happyIn10
		 (PPnil happy_var_1
	)}

happyReduce_33 = happyReduce 6# 6# happyReduction_33
happyReduction_33 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (IF happy_var_1) -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	case happyOut10 happy_x_6 of { happy_var_6 -> 
	happyIn10
		 (let newPosn = (fst happy_var_1, snd $ getProgPosn happy_var_6) in PPif newPosn (happy_var_2, happy_var_4, happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_34 = happyReduce 4# 6# happyReduction_34
happyReduction_34 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (FUNC happy_var_1) -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	happyIn10
		 (let newPosn = (fst happy_var_1, snd $ getProgPosn happy_var_4) in makeFunction newPosn happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_35 = happyReduce 6# 6# happyReduction_35
happyReduction_35 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (LET happy_var_1) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	case happyOut10 happy_x_6 of { happy_var_6 -> 
	happyIn10
		 (let newPosn = (fst happy_var_1, snd $ getProgPosn happy_var_6) in
	      let IDENT _ s = happy_var_2 in 
	      PPlet newPosn (s, happy_var_4, happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_36 = happySpecReduce_3  6# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (LPAIR happy_var_1) -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { (RPAIR happy_var_3) -> 
	happyIn10
		 (let pos = (fst happy_var_1, snd happy_var_3) in
	      let PPpair _ progPair = happy_var_2 in 
	      PPpair pos progPair
	)}}}

happyReduce_37 = happySpecReduce_2  6# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (FIRST happy_var_1) -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (let newPosn = (fst happy_var_1, snd $ getProgPosn happy_var_2) in PPfst newPosn happy_var_2
	)}}

happyReduce_38 = happySpecReduce_2  6# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (SECOND happy_var_1) -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (let newPosn = (fst happy_var_1, snd $ getProgPosn happy_var_2) in PPsnd newPosn happy_var_2
	)}}

happyReduce_39 = happySpecReduce_3  6# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (let newPosn = (fst $ getProgPosn happy_var_1, snd $ getProgPosn happy_var_3) in PPtensor newPosn (happy_var_1, happy_var_3)
	)}}

happyReduce_40 = happyReduce 8# 6# happyReduction_40
happyReduction_40 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (LET happy_var_1) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	case happyOut10 happy_x_6 of { happy_var_6 -> 
	case happyOut10 happy_x_8 of { happy_var_8 -> 
	happyIn10
		 (let newPosn = (fst happy_var_1, snd $ getProgPosn happy_var_8) in 
	      let IDENT _ s1 = happy_var_2 in 
	      let IDENT _ s2 = happy_var_4 in 
	      PPtslet newPosn (s1, s2, happy_var_6, happy_var_8)
	) `HappyStk` happyRest}}}}}

happyReduce_41 = happySpecReduce_1  6# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOutTok happy_x_1 of { (RAND happy_var_1) -> 
	happyIn10
		 (PPrand happy_var_1
	)}

happyReduce_42 = happySpecReduce_2  6# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (RETURN happy_var_1) -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (let pos = (fst happy_var_1, snd $ getProgPosn happy_var_2) in PPret pos happy_var_2
	)}}

happyReduce_43 = happyReduce 6# 6# happyReduction_43
happyReduction_43 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (BIND happy_var_1) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	case happyOut10 happy_x_6 of { happy_var_6 -> 
	happyIn10
		 (let pos = (fst happy_var_1, snd $ getProgPosn happy_var_6) in 
	      let IDENT _ s = happy_var_2 in 
	      PPbind pos (s, happy_var_4, happy_var_6)
	) `HappyStk` happyRest}}}}

happyNewToken action sts stk [] =
	happyDoAction 43# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	ARROW happy_dollar_dollar -> cont 1#;
	LPAREN happy_dollar_dollar -> cont 2#;
	RPAREN happy_dollar_dollar -> cont 3#;
	LPAIR happy_dollar_dollar -> cont 4#;
	RPAIR happy_dollar_dollar -> cont 5#;
	LBOX happy_dollar_dollar -> cont 6#;
	RBOX happy_dollar_dollar -> cont 7#;
	IMPORT happy_dollar_dollar -> cont 8#;
	TYPE happy_dollar_dollar -> cont 9#;
	PARAMETER happy_dollar_dollar -> cont 10#;
	DEFINITION happy_dollar_dollar -> cont 11#;
	HYPOTHESIS happy_dollar_dollar -> cont 12#;
	BITS happy_dollar_dollar -> cont 13#;
	FUNC happy_dollar_dollar -> cont 14#;
	LET happy_dollar_dollar -> cont 15#;
	IN happy_dollar_dollar -> cont 16#;
	FIRST happy_dollar_dollar -> cont 17#;
	SECOND happy_dollar_dollar -> cont 18#;
	ZERO happy_dollar_dollar -> cont 19#;
	ONE happy_dollar_dollar -> cont 20#;
	NIL happy_dollar_dollar -> cont 21#;
	SREC happy_dollar_dollar -> cont 22#;
	CASE happy_dollar_dollar -> cont 23#;
	IF happy_dollar_dollar -> cont 24#;
	THEN happy_dollar_dollar -> cont 25#;
	ELSE happy_dollar_dollar -> cont 26#;
	BIND happy_dollar_dollar -> cont 27#;
	RETURN happy_dollar_dollar -> cont 28#;
	RAND happy_dollar_dollar -> cont 29#;
	LETHYPO happy_dollar_dollar -> cont 30#;
	COLON happy_dollar_dollar -> cont 31#;
	SEMICOLON happy_dollar_dollar -> cont 32#;
	COMMA happy_dollar_dollar -> cont 33#;
	DOLLAR happy_dollar_dollar -> cont 34#;
	AND happy_dollar_dollar -> cont 35#;
	STAR happy_dollar_dollar -> cont 36#;
	EQUAL happy_dollar_dollar -> cont 37#;
	IDENT p s -> cont 38#;
	IDENTCAP p s -> cont 39#;
	BITSTRING p s -> cont 40#;
	END happy_dollar_dollar -> cont 41#;
	FCSTART -> cont 42#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

fcParsing tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


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
parseError [] = error "Parse error: Parsing reaches the end."
parseError ts = error $ "Parse error " ++ position ++info
	   where position = fcPosnToString $ getTokenPosn (head ts) 
	   	 info = ""
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where (new_state) = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off Happy_GHC_Exts.+# nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off Happy_GHC_Exts.+# nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
