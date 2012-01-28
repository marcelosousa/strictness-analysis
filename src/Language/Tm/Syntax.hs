-------------------------------------------------------------------------------
-- Module: Language.Syntax
module Language.Tm.Syntax where
 
type Ident = String

-------------------------------------------------------------------------------
  
data Operation = Add | Sub | Mul | Div | Mod | Eq | Lt | Gt
        deriving Eq

------------------------------------------------------------------------------- 
 
data Prog = Prog Tm
      deriving Eq

-------------------------------------------------------------------------------
data Tm = Fun Ident Tm
        | NFun Ident Ident Tm
        | Let Decl  Tm
        | Bool Bool 
        | Num Integer
        | Var Ident
        | Bot
        | Op  Operation Tm Tm
        | App  Tm Tm
        | App_ Tm Tm
        | If   Tm Tm Tm
        deriving Eq
     
-------------------------------------------------------------------------------    
data Decl = Name {did :: Ident, dtm :: Tm}
          | Val  {did :: Ident, dtm :: Tm}
          deriving Eq
