{-# LANGUAGE TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Inferencing.HTm.HHLib
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade   

module Analysis.Inferencing.HTm.HHLib where
                       
import Data.List as List
import Data.Set  as Set
import Data.Map  as Map 
import Data.Char    
                                      
import Analysis.Inferencing.HTm.HHTy
import Analysis.Inferencing.HTm.HHSubs
import Language.Tm.Syntax 
import Language.HTm.Syntax 

(∪) :: Ord a => Set a -> Set a -> Set a                    
(∪) = Set.union                       

(∈) :: Ord a => a -> Set a -> Bool
(∈) = Set.member

(∉) :: Ord a => a -> Set a -> Bool
(∉) = Set.notMember   

infix 5 ⊈
(⊈) :: Ord a => Set a -> Set a -> Bool
(⊈) l r = not (isSubsetOf l r)

(∖) :: Ord a => Set a -> Set a -> Set a
(∖) = Set.difference

(→) = TyArr

(↦) :: Ord k => k -> v -> Map k v -> Map k v
(↦) = Map.insert
                                
forAll :: (a -> b -> a) -> a -> Set b -> a
forAll f e s = foldl' f e (Set.toList s)

while :: (a -> Bool) -> (a -> a) -> a -> a
while g = until (not . g)
  
trd (_ , _ , x) = x

infix 6 !?
(!?)    (kn, k)  (mn , m) = Map.findWithDefault Set.empty k m

-- | Fresh Variables based on an Int 
type Counter = Int    
         
fstFreshVar :: Counter
fstFreshVar = 1

freshVar :: Counter -> (Ident,Counter)
freshVar n = ('α' : show n, n+1)

freshAnnVar :: Counter -> (Ident,Counter)
freshAnnVar n = ('β' : show n, n+1)                                               

-- | Free Type Variables * Not Free Type Annotation Variables  
ftv :: Environment -> Set TyVar
ftv = Map.fold (\(_,_,ty,_) ftvs -> ftvTy ty ∪ ftvs) Set.empty 

-- | Free Type Variables on a TySch
ftvSch :: TySch -> Set TyVar
ftvSch (Type ty)       = ftvTy ty
ftvSch (FAll tvar sch) = ftvSch sch `Set.difference` Set.singleton tvar

-- | Free Type Variables on a Ty
ftvTy :: Ty -> Set TyVar
ftvTy TInt                  = Set.empty
ftvTy TBool                 = Set.empty
ftvTy (TyVar s)             = Set.singleton s
ftvTy (TyArr (τ1,_) (τ2,_)) = ftvTy τ1 ∪ ftvTy τ2  

-- | Free Annotation Variables on a Ty
favTy :: Ty -> Set TyAnn
favTy (TyArr (τ1,τann1) (τ2,τann2)) = favTy τ1 ∪ favTyAnn τann1 ∪ favTy τ2 ∪ favTyAnn τann2
favTy τ                             = Set.empty

-- | Free Annotation Variables on an Environment
favEnv :: Environment -> Set TyAnn
favEnv = Map.fold favSTy Set.empty

-- | Free Annotation Variables on a Simple Type
favSTy :: SimpleType -> Set TyAnn -> Set TyAnn
favSTy (βs,cs,τ,β) π = βs ∪ favConstraints cs ∪ favTy τ ∪ favTyAnn β ∪ π   

-- | Free Annotation Variables on a TyAnn
favTyAnn :: TyAnn -> Set TyAnn
favTyAnn Live           = Set.empty
favTyAnn Dead           = Set.empty
favTyAnn β@(TyAnnVar _) = Set.singleton β

-- | Free Annotation Variables on a TyAnn
favConstraints :: Constraints -> Set TyAnn
favConstraints cs = Set.fold Set.union Set.empty $ Set.map (\ (LB l r) -> favTyAnn l ∪ favTyAnn r) cs

-- | Get Active TyAnn Variables
atav :: Constraints -> Set TyAnn
atav = Set.fold (\(LB ta1 ta2) satv -> atavTyAnn ta1 ∪ satv) Set.empty 

atavTyAnn :: TyAnn -> Set TyAnn
atavTyAnn a@(TyAnnVar x) = Set.singleton a
atavTyAnn _              = Set.empty

-- | Select Lower Bounds given a set of TyAnn  
selectCβ :: Constraints -> Set TyAnn -> Constraints
selectCβ c βs = Set.filter (\(LB β _) -> β ∈ βs) c
          
-- | Auxiliar Functions for R
createPairSubstConst :: Set TyAnn -> Set TyAnn -> (TySubst, Constraints)
createPairSubstConst βs βs' = foldr (\(β,β') (θ,cs) -> 
        (Comp (TyAnnSubst β β') θ, Set.insert (LB β' β) cs)) (Id,Set.empty) $ zip (Set.toList βs) (Set.toList βs')

buildFreshTyAnn :: TyAnn -> (Set TyAnn, Counter) -> (Set TyAnn, Counter)
buildFreshTyAnn _ (setTyAnnVar, c) = let (x, nc) = freshAnnVar c
                                     in  (Set.insert (TyAnnVar x) setTyAnnVar, nc)

-- | Convenience function to apply several substitutions at once
applyθs []      ty = ty
applyθs (x:xs)  ty = x ⊢ applyθs xs ty 

-- | freshening functions :)
freshTyVar    = mapFst TyVar    . freshVar
freshTyAnnVar = mapFst TyAnnVar . freshAnnVar
  
mapFst f (a,b) = (f a , b)

typeOf :: Operation -> Ty
typeOf Eq = TBool
typeOf Lt = TBool
typeOf Gt = TBool
typeOf _  = TInt

getLB :: TyAnn -> Constraints -> Set TyAnn
getLB β c = let p (LB _  x) = β == x
                s (LB β' _) = β'
             in Set.map s $ Set.filter p c     
             
compJoin :: Set TyAnn -> TyAnn
compJoin = foldr1 join . Set.toList

join :: TyAnn -> TyAnn -> TyAnn
join Live _    = Live
join _    Live = Live
join Dead Dead = Dead
join x y = error $ "don't know: " ++ show x ++ " " ++ show y
             

