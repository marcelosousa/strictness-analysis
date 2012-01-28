-------------------------------------------------------------------------------
-- Module    :  Analysis.Conversion.ATm.Catas
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade

module Analysis.Conversion.ATm.Catas where

import Language.ATm.Printing
import Language.ATm.Syntax
import Language.Tm.Printing
import Language.Tm.Syntax  

import Analysis.Inferencing.ATm.HMTy
import Analysis.Inferencing.ATm.HMLib
import Analysis.Inferencing.ATm.HM

import Data.Map as Map 
import Data.Set as Set

-- Main Stuff           

env :: TyEnv
env = Map.empty

pFold :: Prog -> AProg
pFold (Prog prog) = let (τ, θ, c) = w prog env fstFreshVar
                        nenv      = applyθTyEnv θ env 
                        σ         = gen nenv τ 
                        atm       = tmFold prog σ
                    in AProg atm 

tmFold :: Tm -> TySch -> ATm                    
tmFold Bot           σ = ABot σ
tmFold (Num int)     σ = ANum int σ
tmFold (Bool b)      σ = ABool b σ
tmFold (Var ident)   σ = AVar ident σ
tmFold (Fun i tm)    σ = let τ = unPackTySch σ 
                         in  case τ of
                                  TyArr τ1 τ2 -> let atm = tmFold tm $ Type τ2
                                                     afi = AFunI i τ1 
                                                 in  AFun afi atm σ
                                  _           -> let atm = tmFold tm σ
                                                     afi = AFunI i τ
                                                 in  AFun afi atm σ
tmFold (App t1 t2)   σ = let τ = unPackTySch σ 
                         in  case τ of
                                  TyArr τ1 τ2 -> let at1 = tmFold t1 $ Type $ TyArr τ1 τ
                                                     at2 = tmFold t2 $ Type τ2 
                                                 in  AApp at1 at2 σ 
                                  τ'          -> let at1 = tmFold t1 $ Type $ TyArr τ' τ' -- this is wrong
                                                     at2 = tmFold t2 $ Type τ' 
                                                 in  AApp at1 at2 σ
tmFold (App_ t1 t2)   σ = let τ = unPackTySch σ 
                         in  case τ of
                                  TyArr τ1 τ2 -> let at1 = tmFold t1 $ Type $ TyArr τ1 τ
                                                     at2 = tmFold t2 $ Type τ2 
                                                 in  AApp_ at1 at2 σ 
                                  τ'          -> let at1 = tmFold t1 $ Type $ TyArr τ' τ' -- this is wrong
                                                     at2 = tmFold t2 $ Type τ' 
                                                 in  AApp_ at1 at2 σ
tmFold (Op op t1 t2) σ = let opn = show op
                             nt  = App (App (Var opn) t1) t2
                         in tmFold nt σ                     
tmFold (If con t e)  σ = let acond = tmFold con $ Type TBool
                             at    = tmFold t σ
                             ae    = tmFold e σ
                         in AIf acond at ae σ                             
tmFold (Let decl tm) σ = let adecl = declFold decl σ
                             atm   = tmFold tm σ
                         in ALet adecl atm σ 
                               
declFold :: Decl -> TySch -> ADecl
declFold (Name i tm) σ = let (τ,θ,_)  = w tm env fstFreshVar
                             nenv      = applyθTyEnv θ env 
                             σ'        = gen nenv τ
                             atm = tmFold tm σ'
                         in  AName i atm
declFold (Val  i tm) σ = let (τ,θ,_)  = w tm env fstFreshVar
                             nenv      = applyθTyEnv θ env 
                             σ'        = gen nenv τ
                             atm = tmFold tm σ'
                         in  AVal i atm
