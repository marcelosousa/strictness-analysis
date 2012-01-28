-------------------------------------------------------------------------------
-- Module    :  Scanner
-- Copyright :  (c) 2008 Stefan Holdermans

module Language.Scanner
  ( Token (..)
  , scanner
  ) where

import Language.Scanning

-------------------------------------------------------------------------------
-- tokens

data Token
  = Num Integer
  | Bool Bool
  | Keyword String
  | Ident String
  | Special Char
  | Operator String

instance Show Token where
  show (Num n)        = "numeral " ++ show n
  show (Bool b)       = "boolean " ++ show b
  show (Keyword str)  = "keyword " ++ str
  show (Ident str)    = "identifier " ++ str
  show (Special c)    = "special character " ++ [c]
  show (Operator str) = "operator " ++ str

-------------------------------------------------------------------------------
-- scanner

scanner :: Scanner Token
scanner =  whitestuff |>
  num <|> bool <|> keyword <|> ident <|> special <|> operator

num :: Scanner Token
num =  Num . read <$> some digit

bool :: Scanner Token
bool = Bool . h <$> strings ["True" , "False"]
  where 
    h "True" = True
    h _      = False

keyword :: Scanner Token
keyword =  Keyword <$> strings ["if", "then", "else", "endif",  "end", "fun", "in", "let", "name", "val", "nfun"]

ident :: Scanner Token
ident =  (:) <$> (lower <|> upper) <*> tl <**> pure Ident
  where
    tl = many (lower <|> upper <|> digit <|> oneOf "!$%'-?@^_`~")

special :: Scanner Token
special =  Special <$> oneOf "()"

operator :: Scanner Token
operator =
  Operator <$> strings ["!", "%", "*", "-", "_|_", "+", "=", "=>", "/", "==", "<", ">"]

whitestuff :: Scanner ()
whitestuff =  whitespace <|> lineComment <|> blockComment

whitespace :: Scanner ()
whitespace =  () <$ some (oneOf [' ', '\t', '\n', '\v', '\f', '\r'])

lineComment :: Scanner ()
lineComment =  () <$ string "//" <* many (anyCharBut '\n')

blockComment :: Scanner ()
blockComment =  string "/*" *> tl
  where
    tl     = many (anyCharBut '*') *> (char '*' *> (tlstar <|> cls) <|> cls)
    tlstar = () <$ char '/' <|> char '*' *> tlstar <|> noneOf "/*" *> tl
    cls    = err "unclosed block comment"