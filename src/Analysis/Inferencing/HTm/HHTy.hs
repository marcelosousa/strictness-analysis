-------------------------------------------------------------------------------
-- Module    :  Analysis.Inferencing.HTm.HHTy
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade   

module Analysis.Inferencing.HTm.HHTy where

import Language.Tm.Syntax (Ident)

import Data.Map as Map                                                         
import Data.Set as Set

---------------
-- Type System Datas
---------------                                                                
-------------------------------------------------------------------------------
-- TyVar and TyAnnVar
type Var      = Ident
type TyVar    = Ident
type TyAnnVar = Ident

-------------------------------------------------------------------------------
-- Types
data Ty = TInt
        | TBool
        | TyVar TyVar
        | TyArr (Ty, TyAnn) (Ty, TyAnn)
  deriving (Ord,Eq)          

-------------------------------------------------------------------------------                    
-- Type Annotations       
data TyAnn = Live     
           | Dead     
           | TyAnnVar TyAnnVar
  deriving (Ord, Eq)
        
-------------------------------------------------------------------------------
-- Type Schemes
data TySch = Type Ty
           | FAll TyVar TySch
  
-------------------------------------------------------------------------------
-- Type Substitutions  
data TySubst = Id
             | TySubst    TyVar    Ty
             | TyAnnSubst TyAnn    TyAnn
             | Comp       TySubst  TySubst 

-------------------------------------------------------------------------------
-- Constraints 
data Constraint    = LB TyAnn TyAnn
  deriving (Ord,Eq)

type Constraints = Set Constraint 

-------------------------------------------------------------------------------
-- Environment

type SimpleType    = (Set TyAnn, Constraints, Ty, TyAnn)
type Environment   = Map Var SimpleType                          

-------------------------------------------------------------------------------
-- "Pretty" Printing          

instance Show Constraint where
  show (LB t1 t2) = "(" ++ show t1 ++ " ⊑ " ++ show t2 ++ ")"

instance Show TyAnn where
  show Live = "L"
  show Dead = "D"
  show (TyAnnVar x) = x
  
instance Show TySubst where
  show Id                  = "[id]"
  show (TyAnnSubst a1 a2)  = "[" ++ show a1 ++ " → " ++ show a2 ++ "]"
  show (TySubst a τ1)      = "[" ++ a ++ " → " ++ show τ1 ++ "]"
  show (Comp θ1 θ2)        = show θ1 ++ "◦" ++ show θ2             
  
instance Show TySch where
  show (Type t)   = show t
  show (FAll v t) = "Ɐ"++v++".("++show t++")"

instance Show Ty where
  show TInt          = "Int"
  show TBool         = "Bool"
  show (TyVar v)     = v
  show (TyArr t1 t2) = "(" ++ show t1 ++ "→" ++ show t2 ++ ")" --" ::~ " ++ show ann  ++ ")" 
    