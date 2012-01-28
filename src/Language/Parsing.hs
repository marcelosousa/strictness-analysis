{-# Language RankNTypes #-}

-------------------------------------------------------------------------------
-- Module    :  Parsing
-- Copyright :  (c) 2008 Stefan Holdermans

module Language.Parsing
  ( module Language.Position
  , Scan
  , Message (..)
  , Parser
  , succeed
  , fail_
  , label
  , token
  , position
  , module Language.Idiom
  , Parse
  , parse
  , (<@>)
  )
where

import Language.Idiom
import Language.Position
import Language.Scanning (Scan)

-------------------------------------------------------------------------------
-- error messages

data Message s = Deleted Pos s
               | Inserted Pos s
               | Expected Pos String
               | LexicalError Pos String
               | Fail Pos

instance (Show s) => Show (Message s) where
  show (Deleted pos s)        = show pos ++ ": unexpected "     ++ show s
  show (Inserted pos s)       = show pos ++ ": expected "       ++ show s
  show (Expected pos msg)     = show pos ++ ": expected "       ++ msg
  show (LexicalError pos msg) = show pos ++ ": lexical error: " ++ msg
  show (Fail pos)             = show pos ++ " <parse failure>"  

-------------------------------------------------------------------------------
-- parser type

type Result s a = (Either [Message s] a, [Int])
type P s a = forall b. (Scan s -> Result s b) -> Scan s -> Result s (a, b)

newtype Parser s a = Parser (P s a)

-------------------------------------------------------------------------------
-- primitive parsers

pSucceed         :: a -> P s a
pSucceed x k inp =  addResult x (k inp)

pFail                  :: P s a
pFail _ []             =  failure EOF
pFail _ ((pos, _) : _) =  failure pos

pLabel                          :: String -> P s a
pLabel lab k inp@[]             =  expected EOF lab (k inp)
pLabel lab k inp@((pos, _) : _) =  expected pos lab (k inp)

pEof   :: P s ()
pEof k =  p
  where
    p inp@[]                 = addStep 0 (addResult () (k inp))
    p ((pos, Left str) : ss) = lexicalError pos str (p ss)
    p ((pos, Right s) : ss)  = delete pos s (p ss)

pMatch     :: (s -> Maybe a) -> (Pos -> Message s) -> P s a
pMatch h r =  p
  where
    p k inp@[]                    = addStep 5 (addMsg (r EOF) (k inp))
    p k ((pos, Left str) : ss)    = lexicalError pos str (p k ss)
    p k inp@((pos, Right s) : ss) = case h s of
      Just x  -> addStep 0 (addResult x (k ss))
      Nothing ->
        addStep 5 (addMsg (r pos) (k inp)) `best` delete pos s (p k ss)

pPosition                      :: P s Pos
pPosition k inp@[]             =  addResult EOF (k inp)
pPosition k inp@((pos, _) : _) =  addResult pos (k inp)

-------------------------------------------------------------------------------
-- interface to primitive parsers

succeed   :: a -> Parser s a
succeed x =  Parser (pSucceed x)

fail_ :: Parser s a
fail_ =  Parser pFail

label     :: String -> Parser s a
label lab =  Parser (pLabel lab)

token     :: (s -> Maybe a) -> (Pos -> Message s) -> Parser s a
token h r =  Parser (pMatch h r)

position :: Parser s Pos
position =  Parser pPosition

-------------------------------------------------------------------------------
-- primitive parser combinators

cat       :: P s (a -> b) -> P s a -> P s b
p `cat` q =  \k inp -> let ~(v, ks) = p (q k) inp in (apply v, ks)
  where
    apply (Left ms)            = Left ms
    apply (Right (f, ~(x, y))) = Right (f x, y)

alt       :: P s a -> P s a -> P s a
p `alt` q =  \k inp -> p k inp `best` q k inp

-------------------------------------------------------------------------------
-- applicative interface

instance Functor (Parser s) where
  fmap = liftA

instance Applicative (Parser s) where
  pure x                = Parser (pSucceed x)
  Parser p <*> Parser q = Parser (p `cat` q)

instance Alternative (Parser s) where
  empty                 = Parser pFail
  Parser p <|> Parser q = Parser (p `alt` q)

-------------------------------------------------------------------------------
-- run a parser

type Parse s a = Either [Message s] a

parse                :: Parser s a -> Scan s -> Parse s a
parse (Parser p) inp =  either (Left . id) (Right . fst) v
  where
    ~(v, _)  = (pSucceed const `cat` p `cat` pEof) k inp
    k _      = (Right (), [])

-------------------------------------------------------------------------------
-- constructing the result

addStep            :: Int -> Result s a -> Result s a
addStep w ~(v, ws) =  (v, w : ws)

addResult            :: a -> Result s b -> Result s (a, b)
addResult x ~(v, ws) =  (addx v, ws)
  where
    addx (Left ms) = Left ms
    addx (Right y) = Right (x, y)

addMsg            :: Message s -> Result s a -> Result s b
addMsg m ~(v, ws) =  (addm v, ws)
  where
    addm (Left ms) = Left (m : ms)
    addm (Right _) = Left [m] 

delete       :: Pos -> s -> Result s a -> Result s a
delete pos s =  addStep 5 . addMsg (Deleted pos s)

expected         :: Pos -> String -> Result s b -> Result s (a, b)
expected pos msg =  addStep 5 . addMsg (Expected pos msg)

lexicalError         :: Pos -> String -> Result s a -> Result s a
lexicalError pos msg =  addMsg (LexicalError pos msg)

failure     :: Pos -> Result s a
failure pos =  (Left [Fail pos], [10000])

-------------------------------------------------------------------------------
-- merging steps

best :: Result s a -> Result s a -> Result s a
best l@(_, []) _                       =  l
best _ r@(_, [])                       =  r
best l@(vl, wl : wsl) r@(vr, wr : wsr)
  | wl < wr                            =  l
  | wl > wr                            =  r
  | otherwise                          =  addStep wl (best (vl, wsl) (vr, wsr))

-------------------------------------------------------------------------------
-- derived combinator

infixl 3 <@>

(<@>)     :: String -> Parser s a -> Parser s a
lab <@> p =  label lab <|> p
