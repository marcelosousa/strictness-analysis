-------------------------------------------------------------------------------
-- Module    :  Analysis.Inferencing.ATm.HMLib
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade
module Analysis.Inferencing.ATm.HMLib where
                
import Data.Set as Set
import Data.Map as Map 
import Data.Char    
                                      
import Analysis.Inferencing.ATm.HMTy
import Language.Tm.Syntax
                                    
-- Fresh Variables based on an Int 
type Counter = Int    
         
fstFreshVar :: Counter
fstFreshVar = 97

freshVar :: Counter -> (Ident,Counter)
freshVar n = ('β' : show n, n+1)

-- Get the mono type on a type scheme                                  
getTy :: TySch -> Ty
getTy (Type ty)  = ty
getTy (FAll _ σ) = unPackTySch σ 

-- Get the Ty of a TySch                                                                                                                              
unPackTySch :: TySch -> Ty
unPackTySch (Type τ)   = τ
unPackTySch (FAll _ σ) = unPackTySch σ

-- Default type scheme for all the operations in the language
opTy :: Operation -> TySch
opTy op = Type $ TyArr TInt (TyArr TInt (resultOf op))

-- Type Substitutions   
-- Apply Type Substitutions to Type Environments              
applyθTyEnv :: TySubst -> TyEnv -> TyEnv
applyθTyEnv θ = Map.map (applyθTySch θ)

-- Apply Type Substitutions to Type Schemes
applyθTySch :: TySubst -> TySch -> TySch
applyθTySch    Id         σ            = σ
applyθTySch θ@(Subst x t) (Type ty)    = Type $ applyθTy θ ty
applyθTySch θ@(Subst x t) (FAll y σ) | x == y    = FAll y σ
                                     | otherwise = FAll y $ applyθTySch θ σ
applyθTySch   (Comp θ1 θ2) σ           = applyθTySch θ1 $ applyθTySch θ2 σ
                 
-- Apply Type Substitutions to Types              
applyθTy :: TySubst -> Ty -> Ty
applyθTy Id τ                    = τ
applyθTy θ TInt                  = TInt 
applyθTy θ TBool                 = TBool
applyθTy (Subst x t) (TyVar y) | x == y    = t
                               | otherwise = TyVar y                    
applyθTy (Comp θ1 θ2) (TyVar y)  = applyθTy θ1 $ applyθTy θ2 (TyVar y)                               
applyθTy θ (TyArr t1 t2)         = TyArr (applyθTy θ t1) (applyθTy θ t2)

-- Apply Type Substitutions to Type Annotated              
--applyθTyAnnot :: TySubst -> TyTyAnn -> TyTyAnn
--applyθTyAnnot θ (TyTyAnn τ ann) = TyTyAnn (applyθTy θ τ) ann
                                         
-- Free Type Variables
-- Free Type Variables on a TyEnv
ftv :: TyEnv -> Set TyVar
ftv = Map.fold (\tysch ftvs -> ftvSch tysch `Set.union` ftvs) Set.empty 

-- Free Type Variables on a TySch
ftvSch :: TySch -> Set TyVar
ftvSch (Type ty)       = ftvTy ty
ftvSch (FAll tvar sch) = ftvSch sch `Set.difference` Set.singleton tvar

-- Free Type Variables on a Ty
ftvTy :: Ty -> Set TyVar
ftvTy TInt            = Set.empty
ftvTy TBool           = Set.empty
ftvTy (TyVar s)       = Set.singleton s
ftvTy (TyArr t1 t2)   = ftvTy t1 `Set.union` ftvTy t2  

-- Free Type Variables on a TyAnn
--ftvTyTyAnn :: TyTyAnn -> Set TyVar
--ftvTyTyAnn (TyTyAnn τ _) = ftvTy τ

resultOf :: Operation -> Ty
resultOf Eq = TBool
resultOf Lt = TBool
resultOf Gt = TBool
resultOf _ = TInt


