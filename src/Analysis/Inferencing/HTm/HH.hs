-------------------------------------------------------------------------------
-- Module    :  Analysis.Inferencing.HTm.HH
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade   

module Analysis.Inferencing.HTm.HH where

import Language.Tm.Printing
import Language.Tm.Syntax 
 
import Language.HTm.Printing
import Language.HTm.Syntax  

import Analysis.Conversion.HTm.Catas
import Analysis.Inferencing.HTm.HHLib
import Analysis.Inferencing.HTm.HHSubs
import Analysis.Inferencing.HTm.HHTy

import Data.List  as List
import Data.Map   as Map 
import Data.Maybe as Maybe (fromJust)
import Data.Set   as Set

import Control.Monad.State

import qualified CCO.Printing as PP

pFold :: Prog -> (Constraints, UTm, UTm, UTm,  Prog)
pFold (Prog prog) = let β = TyAnnVar "a" 

                        (τ, θ, c, utm) = evalState (r prog β) (Map.empty,fstFreshVar)
                        c' = Set.insert (LB β β) $ s c (Set.insert β $ favTy τ)
                        utm' = θ ⊢ utm           -- ^ apply the substitution θ to the term
                        u' = transformUTmLUTm utm' c' -- ^ Transform the term to replace dead code by ⊥
                        
                        (τ2, θ2, c2, utm2) = evalState (r (unUTm u') β) (Map.empty,fstFreshVar)
                        c2' = Set.insert (LB β β) $ s c2 (Set.insert β $ favTy τ2)
                        utm2' = θ2 ⊢ utm2
                        u2' = transformUTmLUTm utm2' c2'
                    in  (c', utm' , u', u2' , Prog (unUTm u'))

                     
type Env = (Environment, Counter)
type Result = (Ty, TySubst, Constraints, UTm) 

newTyVar :: State Env Ty
newTyVar = do (env,c) <- get
              let (α,nc) = freshTyVar c 
              do put (env,nc)
                 return α

newTyAnnVar :: State Env TyAnn
newTyAnnVar = do (env,c) <- get
                 let (α,nc) = freshTyAnnVar c 
                 do put (env,nc)
                    return α

modifyEnv :: Environment -> State Env ()
modifyEnv env = do (env',c) <- get
                   put (env,c)
                   
updateEnv f   = do (env , c) <- get
                   put (f env, c)
  
-- | The reconstruction algorithm, expanded for the use with booleans, named
--   lambda's and strict applications.
-- 
r :: Tm -> TyAnn -> State Env Result
r  Bot     β = do α <- newTyVar 
                  return (α, Id, Set.empty, UTm UBot β)
r (Num i)  β = return (TInt,  Id, Set.empty, UTm (UNum i)  β)
r (Bool b) β = return (TBool, Id, Set.empty, UTm (UBool b) β)
r (Var x)  β = do (env,cnt) <- get
                  case (Map.lookup x env) of
                          Nothing -> error $ "Not in scope: " ++ x 
                          Just (βs, c, τ1, β0) -> do let (βs',nc) = Set.fold buildFreshTyAnn (Set.empty, cnt) βs
                                                         (θ',cs)  = createPairSubstConst βs βs'
                                                         τ        = θ' ⊢ τ1
                                                         lb       = LB β β0
                                                         ncs      = (θ' ⊢ c) ∪ Set.insert lb cs
                                                     do put (env,nc)
                                                        return (τ, Id, ncs, UTm (UVar x) β)
                                                         
r (Fun i tm) β = do α  <- newTyVar
                    β1 <- newTyAnnVar   
                    β2 <- newTyAnnVar
                    (env,cnt) <- get
                    let nsty = (Set.empty,Set.empty,α,β1)
                        nenv = (i ↦ nsty) env 
                    do put (nenv,cnt)
                       (τ2, θ1, c1, u1) <- r tm β2
                       let τ = TyArr (θ1 ⊢ α, β1) (τ2, β2)
                           nt = UTm (UFun i u1) β
                       return (τ, θ1, c1, nt)
                       
r (NFun f x tm) β = do α1  <- newTyVar
                       α2  <- newTyVar
                       fβ1 <- newTyAnnVar
                       fβ2 <- newTyAnnVar
                       updateEnv (x ↦ (Set.singleton fβ1        , Set.empty , α1                    , fβ1))
                       updateEnv (f ↦ (Set.fromList [fβ1 , fβ2] , Set.empty , (α1, fβ1) → (α2, fβ2) , fβ2))
                       (τ2, θ1, c1, u1) <- r tm fβ2
                       let θ2 = τ2 `unify` (θ1 ⊢ α2)
                           τ  = TyArr (θ2 ⊢ (θ1 ⊢ α1), θ2 ⊢ (θ1 ⊢ fβ1)) (τ2, fβ2)
                           nt = UTm (UNFun f x u1) β
                           c  = (θ2 ⊢ c1) ∪ (Set.singleton (LB (θ2 ⊢ (θ1 ⊢ fβ1)) fβ2))
                       return (τ, Comp θ1 θ2, c, nt)                       
                       
r (App t1 t2) β = do α   <- newTyVar
                     β'  <- newTyAnnVar
                     β1  <- newTyAnnVar
                     β2  <- newTyAnnVar
                     β2' <- newTyAnnVar     
                     (τ1,θ1,c1,u1) <- r t1 β1
                     (env,_) <- get
                     modifyEnv (θ1 ⊢ env)
                     (τ2,θ2,c2,u2) <- r t2 β2
                     let θ3     = (θ2 ⊢ τ1) `unify` TyArr (τ2,β2') (α,β')
                         auxθ3β = (⊢) θ3
                         c'     = (θ3 ⊢ (θ2 ⊢ c1)) ∪ (θ3 ⊢ c2)                         
                         c''    = Set.fromList [LB β (auxθ3β β') , LB (auxθ3β β') β1 , LB (auxθ3β β2') β2]
                         c      = c' ∪ c''
                         nt     = UTm (UApp u1 u2) β
                     return (θ3 ⊢ α, Comp θ3 (Comp θ2 θ1), c, nt)
                                                   
r (App_ t1 t2) β = do α   <- newTyVar
                      β'  <- newTyAnnVar
                      β1  <- newTyAnnVar
                      β2  <- newTyAnnVar
                      β2' <- newTyAnnVar        
                      (τ1,θ1,c1,u1) <- r t1 β1
                      (env,_) <- get
                      modifyEnv (θ1 ⊢ env)
                      (τ2,θ2,c2,u2) <- r t2 β2
                      let θ3     = (θ2 ⊢ τ1) `unify` TyArr (τ2,β2') (α,β')
                          auxθ3β = (⊢) θ3
                          c'     = (θ3 ⊢ (θ2 ⊢ c1)) ∪ (θ3 ⊢ c2)                         
                          c''    = Set.fromList [LB β1 β2 , LB β (auxθ3β β') , LB (auxθ3β β') β1 , LB (auxθ3β β2') β2] 
                          c      = c' ∪ c''
                          nt     = UTm (UApp_ u1 u2) β
                      return (θ3 ⊢ α, Comp θ3 (Comp θ2 θ1), c, nt)
                      
r (Let (Name x t1) t2) β = do β1  <- newTyAnnVar                              
                              (τ1,θ1,c1,u1) <- r t1 β1
                              (env,_) <- get
                              let aenv = θ1 ⊢ env
                                  βs   = favTy τ1 ∖ favEnv aenv
                                  βs'  = favEnv aenv ∪ favTy τ1 ∪ Set.singleton β1 
                                  c1'  = s c1 βs'
                                  sty  = (βs, selectCβ c1' βs, τ1, β1)
                                  nenv = (x ↦ sty) aenv
                              do modifyEnv nenv
                                 (τ2,θ2,c2,u2) <- r t2 β
                                 let utm  = UTm (ULet (UName x u1) u2) β
                                     c1'' = θ2 ⊢ c1
                                 return (τ2, Comp θ2 θ1, c1'' ∪ c2, utm)     
                                                                                                    
r (Op o t1 t2) β = do β1 <- newTyAnnVar
                      β2 <- newTyAnnVar
                      (τ1,θ1,c1,u1) <- r t1 β1
                      (env,_) <- get
                      modifyEnv (θ1 ⊢ env)
                      (τ2,θ2,c2,u2) <- r t2 β2
                      let θ3 = (θ2 ⊢ τ1) `unify` TInt
                          θ4 = (θ3 ⊢ τ2) `unify` TInt
                          c  = Set.fromList [ LB β (θ3 ⊢ (θ4 ⊢ β1))
                                            , LB β (θ4 ⊢ (θ3 ⊢ β2))
                                            ] 
                               ∪ applyθs [θ4, θ3,θ2] c1
                               ∪ applyθs [θ4, θ3] c2
                          nt = UTm (UOp o u1 u2) β
                      return (typeOf o, Comp θ4 (Comp θ3 (Comp θ2 θ1)), c, nt)
                      
r (If g t1 t2) β = do βg <- newTyAnnVar
                      β1 <- newTyAnnVar
                      β2 <- newTyAnnVar
                      (τg,θg,cg,ug) <- r g  βg
                      (τ1,θ1,c1,u1) <- r t1 β1 
                      (τ2,θ2,c2,u2) <- r t2 β2
                      let θ3 = applyθs [θ2 , θ1] τg `unify` TBool
                          θ4 = (θ3 ⊢ τ2)
                               `unify`
                                applyθs [θ3 , θ2] τ1
                          θ  = foldr1 Comp [θ4 , θ3, θ2, θ1, θg]
                          τ  = θ ⊢ τ1 -- or τ2 for that matter
                          nt = UTm (UIf ug u1 u2) β
                          c  = Set.fromList [LB β βg, LB β β1 , LB β β2]
                      return (τ, θ, c ∪ cg ∪ c1 ∪ c2, nt)  
                      
r t _ = error $ "No case for: " ++ show t

-- | Unification of two types including annotations.
unify :: Ty -> Ty -> TySubst
unify TInt      TInt                                  = Id
unify TBool     TBool                                 = Id
unify (TyVar α) (TyVar α')  | α == α'                 = Id  
unify (TyVar α) τ           | α ∉ ftvTy τ             = TySubst α τ 
unify τ         (TyVar α)   | α ∉ ftvTy τ             = TySubst α τ
unify (TyArr    (τ1,  β1)  (τ2,  β2 ))
      (TyArr    (τ1', β1') (τ2', β2')) = let θ1  = τ1 `unify` τ1'
                                             θ2  = TyAnnSubst (θ1 ⊢ β1) 
                                                              (θ1 ⊢ β1')
                                             θ3  = (θ2 ⊢ (θ1 ⊢ τ2)) `unify` (θ2 ⊢ (θ1 ⊢ τ2'))
                                             aux = applyθs [θ3 , θ2 , θ1]
                                             θ4  = TyAnnSubst (aux β2) (aux β2')
                                         in Comp θ4 $ Comp θ3 $ Comp θ2 θ1
unify τ         τ'    = error $ "unification fail:" ++ show τ ++ " `u` " ++ show τ'
 

-- | The solving worklist-algorithm for finding an retrieving the `relevant'
--   constraints.
s :: Constraints -> Set TyAnn -> Constraints
s constraints activeAnnVars =
  let {- Step 1: Initialisation -}
      (infl , lwrbnds , wl)   = forAll initF       (Map.empty , Map.empty , []) constraints
      infl'                   = forAll updateInfl  infl                         constraints
      lwrbnds'                = forAll updateLB    lwrbnds                      activeAnnVars
      
      {- Step 2: iteration -}
      (infl'', lwrbnds'', wl') = while (not . List.null . trd) 
                                       iterateF 
                                       (infl', lwrbnds' , wl)
  in  {- Step 3: producing the result -}
      Set.fromList
       [LB β' β | β  <- Set.toList $ favConstraints constraints 
                , β' <- Set.toList $ ("β", β) !? ("lwrbnds''", lwrbnds'') ]
  where {- Step 1: initialisation -}
        initF (infl , lwrbnds , wl) c@(LB β1 β2) = ( Map.insert  β2 Set.empty $ 
                                                     Map.insert  β1 Set.empty infl -- ^ Add empty constraints for both β1 and β2
                                                   , Map.insert  β2 Set.empty lwrbnds
                                                   , c : wl)
        updateInfl :: Map TyAnn Constraints -> Constraint -> Map TyAnn Constraints                                           
        updateInfl    infl             c@(LB β1 β2) = Map.update (Just . Set.insert c) β1 infl  
        updateLB      lwrbnds          β            = Map.insert β (Set.singleton β) lwrbnds
        {- Step 2: iteration -}
        iterateF (infl , lwrbnds, LB β1 β2 : wl) = let boundsβ1 = ("β1"  , β1) !? ("lwrbnds", lwrbnds)
                                                       boundsβ2 = ("β2.0", β2) !? ("lwrbnds", lwrbnds)
                                                   in if boundsβ1 ⊈ boundsβ2 then
                                                          let bounds = Map.insert β2 (boundsβ2 ∪ boundsβ1) lwrbnds
                                                              wl'    = Set.toList  (("β2.1", β2) !? ("infl",infl)) ++ wl
                                                          in  (infl , bounds , wl')
                                                        else
                                                          (infl , lwrbnds , wl)
        
-- | The transformation function to a term where ⊥ is replaced for every dead
--   variable
transformUTmLUTm :: UTm -> Constraints -> UTm
transformUTmLUTm (UTm u β) c = let βs = getLB β c
                          in  if Set.null βs
                              then UTm UBot Dead
                              else UTm (transformHTmLHTm u c) (compJoin βs)
                         
transformHTmLHTm :: HTm -> Constraints -> HTm
transformHTmLHTm h@(UFun i u)      c = UFun i $ transformUTmLUTm u c 
transformHTmLHTm h@(UApp u1 u2)    c = case transformUTmLUTm u1 c of
                                         f@(UTm _ Dead) -> UBot
                                         f -> UApp f $ transformUTmLUTm u2 c
transformHTmLHTm h@(UApp_ u1 u2)   c = case transformUTmLUTm u1 c of
                                         f@(UTm _ Dead) -> UBot
                                         f -> UApp_ f $ transformUTmLUTm u2 c
transformHTmLHTm h@(ULet udecl u2) c = ULet (transformUDeclLUDecl udecl c) (transformUTmLUTm u2 c) 
transformHTmLHTm h@(UOp  op u1 u2) c = UOp op (transformUTmLUTm u1 c) (transformUTmLUTm u2 c)
transformHTmLHTm h@(UIf  g  u1 u2) c = UIf (transformUTmLUTm g c) (transformUTmLUTm u1 c) (transformUTmLUTm u2 c)
transformHTmLHTm h                 _ = h

transformUDeclLUDecl :: UDecl -> Constraints -> UDecl
transformUDeclLUDecl (UName i u1) c = UName i $ transformUTmLUTm u1 c
transformUDeclLUDecl (UVal  i u1) c = UVal  i $ transformUTmLUTm u1 c
