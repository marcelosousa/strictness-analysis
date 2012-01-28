-------------------------------------------------------------------------------
-- Module: Language.Syntax
module Language.ATm.Printing where

import Language.ATm.Syntax 
import Language.Tm.Printing

instance Show AProg where
  show (AProg tm) = "Ͱ " ++ show' tm
    where show' (AFun s tm ty)    = "(λ"++show s++" -> "++show tm++") :: "++show ty
          show' (ALet d tm ty)    = "let " ++ show d ++ " in " ++ show tm ++ " end :: "++show ty
          show' (ANum i ty)       = show i ++ " :: "++show ty
          show' (ABool b ty)      = show b ++ " ∷ "++show ty
          show' (AVar s ty)       = s  ++ " :: "++show ty 
          show' (ABot   ty)       = "⊥" ++ " :: "++ show ty
          show' (AOp op t1 t2 ty) = "(" ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ ") :: "++show ty
          show' (AApp  t1 t2 ty)   = "(" ++ show t1 ++ " " ++ show t2 ++ ") :: "++show ty                          
          show' (AApp_ t1 t2 ty)   = "(" ++ show t1 ++ " $! " ++ show t2 ++ ") :: "++show ty  
          show' (AIf c t1 t2 ty)   = "if " ++ show c ++ " then " ++ show t1 ++ " else " ++ show t2 ++ " :: "++show ty
                    
instance Show ATm where
  show (AFun s tm ty)    = "(λ"++show s++" -> "++show tm++")"  ++ "∷"++show ty
  show (ALet d tm ty)    = "(let " ++ show d ++ " in " ++ show tm  ++ " end) ∷ "++show ty
  show (ANum i ty)       = show i  ++ "∷"++show ty
  show (ABool b ty)      = show b  ++ "∷"++show ty
  show (AVar s ty)       = s       ++ "∷"++show ty 
  show (ABot   ty)       = "⊥"     ++ "∷"++show ty
  show (AOp op t1 t2 ty) = "(" ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ ")"  ++ "∷"++show ty
  show (AApp  t1 t2 ty)  = "(" ++ show t1 ++ " " ++ show t2 ++ ")"  ++ "∷"++show ty
  show (AApp_ t1 t2 ty)  = "(" ++ show t1 ++ " $! " ++ show t2 ++ ")" ++ "∷"++show ty
  show (AIf c t1 t2 ty)   = "(if" ++ show c ++ " then " ++ show t1 ++ " else " ++ show t2 ++ ") :: "++show ty


instance Show ADecl where
  show (AName name tm) = "name: "++ name ++ " = " ++ show tm
  show (AVal name tm) = "val: "++ name ++ " = " ++ show tm
                          
instance Show AFunI where
  show (AFunI i ty) = i ++ "::" ++ show ty