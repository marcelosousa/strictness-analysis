-------------------------------------------------------------------------------
-- Module: Language.Syntax
module Language.ATm.Syntax where

import Language.Tm.Syntax (Operation(..), Ident) 
import Analysis.Inferencing.ATm.HMTy

-- AST                     
data AProg = AProg ATm
                          
data ATm = ABot  {                                           ty :: TySch}
         | ABool {bool   :: Bool,                            ty :: TySch}
         | ANum  {int    :: Integer,                         ty :: TySch}
         | AVar  {aident :: Ident,                           ty :: TySch}
         | AFun  {afuni  :: AFunI,   atm :: ATm,             ty :: TySch}
         | AApp  {ltm :: ATm, rtm :: ATm,                    ty :: TySch}
         | AApp_ {ltm :: ATm, rtm :: ATm,                    ty :: TySch}  
         | ALet  {adecl  :: ADecl,   atm :: ATm,             ty :: TySch}
         | AOp   {aop :: Operation,  ltm :: ATm, rtm :: ATm, ty :: TySch}
         | AIf   {acon :: ATm, athen :: ATm, aelse :: ATm,   ty :: TySch}


data AFunI = AFunI {afid :: Ident, afity :: Ty}         

data ADecl = AName {adid :: Ident, adtm :: ATm}
           | AVal  {adid :: Ident, adtm :: ATm}        