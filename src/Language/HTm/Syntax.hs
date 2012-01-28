-------------------------------------------------------------------------------
-- Module: Language.Syntax
module Language.HTm.Syntax where

import Language.Tm.Syntax (Operation(..), Ident)
import Analysis.Inferencing.HTm.HHTy 
import Language.Tm.Printing

-- AST                     
type HProg = UTm

data UTm = UTm HTm TyAnn
  deriving Eq
                          
data HTm = UBot      
         | UBool Bool
         | UNum  Integer
         | UVar  Ident
         | UFun  Ident UTm
         | UNFun Ident Ident UTm
         | UApp  UTm UTm
         | UApp_ UTm UTm
         | ULet  UDecl UTm
         | UOp   Operation UTm UTm
         | UIf   UTm UTm UTm
         deriving Eq


data UDecl = UName {udid :: Ident, udtm :: UTm}
           | UVal  {udid :: Ident, udtm :: UTm}
           deriving Eq         