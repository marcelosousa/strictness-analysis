{-# LANGUAGE TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Inferencing.HTm.HHSubs
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade   

module Analysis.Inferencing.HTm.HHSubs where

import Data.Set  as Set
import Data.Map  as Map 

import Analysis.Inferencing.HTm.HHTy
import Language.HTm.Syntax 

infix 9 ⊢

-- Todo: find a better character for this...
class Substitutable a where
  (⊢) :: TySubst -> a -> a
  
instance Substitutable Environment where
  (⊢) θ =  Map.map (θ ⊢)
  
instance Substitutable SimpleType where
  θ ⊢ (βs, c, τ, β) = let nsAnnVars = Set.map (θ ⊢) βs
                          nsTyConst = θ ⊢ c
                          τ'        = θ ⊢ τ 
                          nAnnVar   = θ ⊢ β
                      in  (nsAnnVars, nsTyConst, τ', nAnnVar)
  
instance Substitutable UTm where
  θ ⊢ (UTm htm ann) = UTm (θ ⊢ htm) $ θ ⊢ ann

instance Substitutable HTm where
  θ ⊢ (UFun x utm) = UFun x $ θ ⊢ utm
  θ ⊢ (UApp t1 t2) = UApp (θ ⊢ t1) (θ ⊢ t2)
  θ ⊢ (ULet (UName x t1) t2) =  ULet (UName x (θ ⊢ t1)) (θ ⊢ t2)
  θ ⊢ x = x  
  
instance Substitutable Constraint where
  θ ⊢ (LB c1 c2) = LB (θ ⊢ c1) (θ ⊢ c2)

instance Substitutable Constraints where
  (⊢) θ = Set.map (θ ⊢)

instance Substitutable TyAnn where
  θ@(TyAnnSubst (TyAnnVar x) β) ⊢ t@(TyAnnVar y) | x == y = β
  (Comp θ1 θ2)                  ⊢ β = θ1 ⊢ (θ2 ⊢ β)                               
  θ                             ⊢ β = β
      
instance Substitutable Ty where
  Id ⊢ τ                    = τ
  θ ⊢ TInt                  = TInt 
  θ ⊢ TBool                 = TBool
  (TySubst x t) ⊢ (TyVar y) | x == y    = t
                            | otherwise = TyVar y
  (TyAnnSubst _ _) ⊢ τ      = τ                                                       
  (Comp θ1 θ2) ⊢ (TyVar y)  = θ1 ⊢ (θ2 ⊢ TyVar y)
  θ ⊢ (TyArr (τ1,a1) (τ2,a2)) = TyArr  (θ ⊢ τ1 , θ ⊢ a1) 
                                       (θ ⊢ τ2 , θ ⊢ a2)        
                                                                    
