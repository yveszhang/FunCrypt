module FCSyntax 
       ( FcAspect (..) ,
         FcType (..) ,
         FcArgument (..) ,
         FcProgram (..) ,
       ) where 

data FcAspect = Modal | Arrow | Linear
              deriving (Eq, Show, Ord)

data FcType = Tvar String 
            | Tbits 
            | Tprod (FcType, FcType) 
            | Ttensor (FcType, FcType)
            | Tfunc (FcType, FcAspect, FcType) 
            | Tproba FcType
            | Terror String
            | Tunknown
            deriving (Eq, Show) 

data FcArgument = FcArgument {argName :: String , argAspect :: FcAspect , argType :: FcType}
                deriving (Eq, Show) 

data FcProgram = Pvar String
               | Pnil 
               | Pone 
               | Pzero 
               | Pcase 
               | Psrec 
               | Pif (FcProgram, FcProgram, FcProgram)
               | Pfunc (FcArgument, FcProgram)
               | Papp (FcProgram, FcProgram)
               | Plet (String, FcProgram, FcProgram)
               | Ppair (FcProgram, FcProgram)
               | Pfst FcProgram
               | Psnd FcProgram
               | Ptensor (FcProgram, FcProgram)
               | Ptslet (String, String, FcProgram, FcProgram)
               | Prand 
               | Pret FcProgram
               | Pbind (String, FcProgram, FcProgram)
               | Phypo (String, (FcProgram, FcProgram), FcProgram)
               | Pparen FcProgram
               deriving (Eq, Show) 