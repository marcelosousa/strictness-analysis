-------------------------------------------------------------------------------
-- Module: Language.HTm.Printing
module Language.HTm.Printing where

import Language.HTm.Syntax 
import Language.Tm.Printing 
import Language.Tm.Syntax

import CCO.Printing

instance Show UTm where
  show (UTm tm tyann)    = show tm ++ " ∷ " ++ show tyann  
          
instance Show HTm where
  show (UFun  s  tm)      = "(λ" ++ s               ++ " → " ++ show tm ++ ")"
  show (UNFun f s  tm)    = "(μ" ++ f ++ ".λ"  ++ s ++ " → " ++ show tm ++ ")"
  show (ULet  d  tm)      = "(let " ++ show d ++ " in " ++ show tm  ++ " end)"
  show (UNum  i    )      = show i
  show (UBool b    )      = show b
  show (UVar  s    )      = s  
  show (UBot       )      = "⊥"
  show (UOp   op t1 t2) = "(" ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ ")"
  show (UApp     t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (UApp_    t1 t2) = "(" ++ show t1 ++ " $! " ++ show t2 ++ ")"
  show (UIf   g  t1 t2) = "if " ++ show g ++ " then " ++ show t1 ++ " else " ++ show t2 ++ " endif"

instance Show UDecl where
  show (UName name tm) = "name: "++ name ++ " = " ++ show tm
  show (UVal  name tm) = "val: " ++ name ++ " = " ++ show tm

instance Printable UTm where
  pp (UTm tm _) = pp tm

instance Printable HTm where
  pp (UFun s tm) = text "fun" >#< text s >#< (text "=>" >-< pp tm)
  pp (UNFun f s tm) = text "nfun" >#< text f >#< text s >#< (text "=>" >-< pp tm)
  pp (ULet d tm) = text "let" >#< pp d >-< text "in" >#< pp tm >-< text "end"
  pp (UNum i)    = text $ show i
  pp (UBool b)   = text $ show b
  pp (UVar s) = text s
  pp (UBot)   = text "_|_"
  pp (UOp op t1 t2) = pparensO op t1 >#< text (show op) >#< pparensO op t2
  pp (UApp   t1 t2) = pparensA t1 >#< pparensA' t2
  pp (UApp_  t1 t2) = pparensA t1 >#< text "!" >#< pparensA' t2
  pp (UIf g  t1 t2) = text "if" >#< pp g >#< text "then" >-< indent 2 (pp t1) >-< text "else" >-< indent 2 (pp t2) >-< text "endif"

-- wether (a `op1` b) `op2` c is not equal to a `op1` b `op2` c
needsParens :: Operation -> Operation -> Bool
needsParens Sub Add = True
needsParens _ Add = False
needsParens Sub Sub = True
needsParens Add Sub = True
needsParens _ Sub = False
needsParens Mul Mul = False
needsParens o1 o2 = True

pparensO _ t@(UTm (UApp _ _) _) = parens $ pp t
pparensO _ t@(UTm (UApp_ _ _) _) = parens $ pp t
pparensO op1 t@(UTm (UOp op2 _ _) _) = (if needsParens op2 op1 then parens else id) $ pp t
pparensO _ t = pp t

pparensA t@(UTm (UOp _ _ _) _) = parens $ pp t
pparensA t = pp t

pparensA' t@(UTm (UOp _ _ _) _) = parens $ pp t
pparensA' t@(UTm (UApp _ _) _) = parens $ pp t
pparensA' t@(UTm (UApp_ _ _) _) = parens $ pp t
pparensA' t = pp t

instance Printable UDecl where
  pp (UName name tm) = text "name" >#< text name >#< text "=" >-< pp tm
  pp (UVal  name tm) = text "val" >#< text name >#< text "=" >-< pp tm