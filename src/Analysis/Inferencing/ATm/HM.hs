-------------------------------------------------------------------------------
-- Module    :  Analysis.Inferencing.HM
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade
module Analysis.Inferencing.ATm.HM where

import Language.Tm.Printing
import Language.Tm.Syntax  

import Analysis.Inferencing.ATm.HMLib
import Analysis.Inferencing.ATm.HMTy

import Data.Map as Map
import Data.Set as Set

-- generalization                                        
gen :: TyEnv -> Ty -> TySch
gen env TInt = Type TInt
gen env ty   = let tvars = ftvTy ty `Set.difference` ftv env
                   ntysch = Type ty
               in Set.fold FAll ntysch tvars   

-- instantiation 
inst :: TySch -> Counter -> (Ty, Counter)
inst (Type ty)          c = (ty,c)
inst (FAll tyVar tySch) c = let (fTyVar, nc) = freshVar c
                                θ = Subst tyVar (TyVar fTyVar)
                                nTySch = applyθTySch θ tySch
                            in inst nTySch nc 

-- W algorithm 
w :: Tm -> TyEnv -> Counter -> (Ty, TySubst, Counter)
w Bot             _   c = let (fVar, nc) = freshVar c
                          in  (TyVar fVar, Id, nc)
w (Num _)         _   c = (TInt, Id, c)             
w (Bool b)        _   c = (TBool, Id, c)
w (Var i)         env c = let idTy = Map.lookup i env
                          in  case idTy of
                              Nothing    -> error $ "W error: " ++ i ++ " not in scope."
                              Just tysch -> let (ty,nc) = inst tysch c
                                            in (ty, Id, nc) 
w (Fun i tm)      env c = let (fVar, nc)    = freshVar c
                              σ1            = Type $ TyVar fVar
                              env'          = Map.insert i σ1 $ Map.delete i env                         
                              (τ2, θ1, nc') = w tm env' nc
                          in  (applyθTy θ1 (TyArr (TyVar fVar) τ2), θ1, nc')
w (App  t1 t2)    env c = let (fVar, nc)    = freshVar c
                              (τ1, θ1, nc') = w t1 env nc
                              (τ2, θ2, c')  = w t2 (applyθTyEnv θ1 env) nc'
                              α  = TyVar fVar 
                              θ3 = applyθTy θ2 τ1 `unify` TyArr τ2 α
                          in  (applyθTy θ3 α, Comp θ3 (Comp θ2 θ1), c')
w (App_ t1 t2)    env c = let (fVar, nc)    = freshVar c
                              (τ1, θ1, nc') = w t1 env nc
                              (τ2, θ2, c')  = w t2 (applyθTyEnv θ1 env) nc'
                              α  = TyVar fVar 
                              θ3 = applyθTy θ2 τ1 `unify` TyArr τ2 α
                          in  (applyθTy θ3 α, Comp θ3 (Comp θ2 θ1), c')
w (Op   op t1 t2) env c = let opn  = show op
                              env' = Map.insert opn (opTy op) env
                              napp = App (App (Var opn) t1) t2
                          in  w napp env' c 
w (Let  decl tm)  env c = let (τ1, θ1, nc') = wDecl decl env c
                              env' = applyθTyEnv θ1 env
                              τ1'  = gen env' τ1
                              nenv = Map.insert (did decl) τ1' env'
                              (τ, θ2, nc) = w tm nenv nc'
                          in  (τ, Comp θ2 θ1, nc)
w (If  cond t e)  env c = let (τ1, θ1, cc) = w cond env c
                              !θ1' = τ1 `unify` TBool
                              (τ2, θ2, nc'') = w t (applyθTyEnv θ1' env) cc
                              (τ3, θ3, c'')  = w e (applyθTyEnv θ1' env) nc''
                              !θ4 = τ2 `unify` τ3
                          in  (τ3, Comp θ4 (Comp θ3 (Comp θ2 θ1)), c'')
                                                                
wDecl :: Decl -> TyEnv -> Counter -> (Ty, TySubst, Counter)
wDecl decl = w (dtm decl)

                                           
-- Unification Algorithm for Types            
{--
τ1 = TyArr (TyVar "a") (TyArr TInt (TyVar "a") UndefAnnot) UndefAnnot
τ2 = TyArr (TyVar "TInt") (TyArr (TyVar "b" (TyVar "c") UndefAnnot) UndefAnnot

*Main> unify τ1 τ2
[c -> Int]◦[b -> Int]◦[a -> Int]
--}
unify :: Ty -> Ty -> TySubst 
unify TBool     TBool                                  = Id
unify TInt      TInt                                   = Id
unify (TyVar a) TInt                                   = Subst a TInt
unify TInt      (TyVar b)                              = Subst b TInt
unify (TyVar a) TBool                                  = Subst a TBool
unify TBool     (TyVar b)                              = Subst b TBool
unify (TyVar a) (TyVar b) | a == b                     = Id
unify (TyVar a) τ2        | a `Set.notMember` ftvTy τ2 = Subst a τ2
unify τ1 (TyVar b)        | b `Set.notMember` ftvTy τ1 = Subst b τ1
unify (TyArr τ11 τ12) (TyArr τ21 τ22)                  = let θ1 = τ11 `unify` τ21
                                                             θ2 = applyθTy θ1 τ12 `unify` applyθTy θ1 τ22
                                                         in Comp θ2 θ1
unify τ1 τ2                                            = error $ "Unify error: cannot unify " ++ show τ1 ++ " with " ++ show τ2

--unPackTyTyAnn :: TyTyAnn -> Ty
--unPackTyTyAnn (TyTyAnn τ _) = τ
