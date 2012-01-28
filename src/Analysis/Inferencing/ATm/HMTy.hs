-------------------------------------------------------------------------------
-- Module    :  Analysis.Inferencing.ATm.HMTy
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade
module Analysis.Inferencing.ATm.HMTy where
  
import Language.Tm.Syntax (Ident)

import Data.Map as Map 
---------------
-- Type System Datas
---------------                                                                

-------------------------------------------------------------------------------
-- Type Env
type TyEnv    = Map Var   TySch     

-------------------------------------------------------------------------------
-- Var and TyVars
type Var      = Ident
type TyVar    = Ident

-------------------------------------------------------------------------------
-- Types
data Ty = TInt
        | TBool
        | TyVar TyVar
        | TyArr Ty Ty
        
-------------------------------------------------------------------------------
-- Type Schemes
data TySch = Type Ty
           | FAll TyVar TySch
  
-------------------------------------------------------------------------------
-- Type Substitutions
data TySubst = Id
             | Subst  TyVar Ty
             | Comp   TySubst TySubst 

-------------------------------------------------------------------------------
-- "Pretty" Printing
instance Show TySubst where
  show Id               = "[id]"
  show (Subst a τ1)     = "[" ++ a ++ " -> " ++ show τ1 ++ "]"
  show (Comp θ1 θ2)     = show θ1 ++ "◦" ++ show θ2             
  
instance Show TySch where
  show (Type t)   = show t
  show (FAll v t) = "Ɐ"++v++".("++show t++")"

instance Show Ty where
  show TInt          = "Int"
  show TBool         = "Bool"  
  show (TyVar v)     = v
  show (TyArr t1 t2) = "(" ++ show t1 ++ "->" ++ show t2 ++ ")" --" ::~ " ++ show ann  ++ ")"  