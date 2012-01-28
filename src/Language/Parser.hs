-------------------------------------------------------------------------------
-- Module    :  Parser
-- Copyright :  (c) 2008 Stefan Holdermans

module Language.Parser
  ( Token
  , TokenParser
  , Prog
  , parser
  ) where

import Language.Parsing
import Language.Scanner hiding (Token (..))
import Language.Scanner (Token)
import qualified Language.Scanner as T (Token (..))
import Language.Tm.Syntax

-------------------------------------------------------------------------------
-- parser

parser :: TokenParser Prog
parser =  Prog <$> tm

-------------------------------------------------------------------------------
-- terminals

type TokenParser = Parser Token

num :: TokenParser Integer
num =  token h (`Expected` "numeral") 
  where
    h (T.Num n) = Just n
    h _         = Nothing
    
bool :: TokenParser Bool
bool = token h (`Expected` "boolean")    
  where
    h (T.Bool b) = Just b
    h _          = Nothing

keyword :: String -> TokenParser String
keyword    key    =  token h (`Inserted` T.Keyword key)
  where
    h (T.Keyword key') | key == key' = Just key'
    h _                              = Nothing

ident :: TokenParser String
ident =  token h (`Expected` "identifier")
  where
    h (T.Ident str) = Just str
    h _             = Nothing

special :: Char -> TokenParser Char
special    c    =  token h (`Inserted` T.Special c)
  where
    h (T.Special c') | c == c' = Just c'
    h _                        = Nothing

operator :: String -> TokenParser String
operator    op     =  token h (`Inserted` T.Operator op)
  where
    h (T.Operator op') | op == op' = Just op'
    h _                            = Nothing

-------------------------------------------------------------------------------
-- declarations

decl :: TokenParser Decl
decl =  "declaration" <@> declName <|> declVal

declName :: TokenParser Decl
declName =  Name <$ keyword "name" <*> ident <* operator "=" <*> tm

declVal :: TokenParser Decl
declVal =  Val <$ keyword "val" <*> ident <* operator "=" <*> tm

-------------------------------------------------------------------------------
-- terms

tm :: TokenParser Tm
tm =  tmRelPrio

tmRelPrio :: TokenParser Tm
tmRelPrio =  chainl opRelPrio tmAddPrio
  where
    opRelPrio = choice [Op Eq <$ operator "==", Op Lt <$ operator "<", Op Gt <$ operator ">"]

tmAddPrio :: TokenParser Tm
tmAddPrio =  chainl opAddPrio tmMulPrio
  where
    opAddPrio = choice [Op Add <$ operator "+", Op Sub <$ operator "-"]

tmMulPrio :: TokenParser Tm
tmMulPrio =  chainl opMulPrio tmFunPrio
  where
    opMulPrio = choice
      [Op Mul <$ operator "*", Op Div <$ operator "/", Op Mod <$ operator "%"]

tmFunPrio :: TokenParser Tm
tmFunPrio =  "term" <@> choice [tmIf, tmFun, tmNFun, tmLet, tmAppPrio]

tmFun :: TokenParser Tm
tmFun =  Fun <$ keyword "fun" <*> ident <* operator "=>" <*> tm

tmNFun :: TokenParser Tm
tmNFun =  NFun <$ keyword "nfun" <*> ident <*> ident <* operator "=>" <*> tm

tmLet :: TokenParser Tm
tmLet =  Let <$ keyword "let" <*> decl <* keyword "in" <*> tm <* keyword "end"

tmAppPrio :: TokenParser Tm
tmAppPrio =  chainl opAppPrio tmAtom
  where
    opAppPrio = pure App <|> App_ <$ operator "!"

tmAtom :: TokenParser Tm
tmAtom =  "term" <@> choice [tmNum, tmBool, tmVar, tmBot, tmParens]

tmNum :: TokenParser Tm
tmNum =  Num <$> num

tmBool :: TokenParser Tm
tmBool = Bool <$> bool

tmVar :: TokenParser Tm
tmVar =  Var <$> ident

tmBot :: TokenParser Tm
tmBot =  Bot <$ operator "_|_"

tmParens :: TokenParser Tm
tmParens =  special '(' *> tm <* special ')'

---------------------------------------------------------------------------------
-- If-statement
tmIf :: TokenParser Tm
tmIf = If <$ keyword "if" <*> tm <* keyword "then" <*> tm <* keyword "else" <*> tm <* keyword "endif"