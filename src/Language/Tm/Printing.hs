-------------------------------------------------------------------------------
-- Module    :  Language.Tm.Printing

module Language.Tm.Printing where

import Language.Tm.Syntax 

instance Show Prog where
  show (Prog tm) = "Program: " ++ show tm
                                           
instance Show Tm where
  show (Fun s tm)     = "(λ"++s++" -> "++show tm++")" 
  show (NFun f s tm)  = "(μ"++f++".λ"++s++" -> "++show tm++")" 
  show (Let d tm)     = "let " ++ show d ++ " in " ++ show tm ++ " end"
  show (Num i)        = show i
  show (Bool b)       = show b 
  show (Var s)        = s  
  show (Bot)          = "⊥" 
  show (Op op t1 t2)  = "(" ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ ")" 
  show (App  t1 t2)   = "(" ++ show t1 ++ " " ++ show t2 ++ ")" 
  show (App_ t1 t2)   = "(" ++ show t1 ++ " $! " ++ show t2 ++ ")" 
  show (If g t1 t2 )  = "if " ++ show g ++ " then " ++ show t1 ++ " else " ++ show t2 ++ " endif"
  
instance Show Operation where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "`div`"
  show Mod = "`mod`"
  show Eq  = "=="
  show Lt  = "<"
  show Gt  = ">"

instance Show Decl where
  show (Name name tm) = "name: "++ name ++ " = " ++ show tm
  show (Val name tm) = "val: "++ name ++ " = " ++ show tm
                                  