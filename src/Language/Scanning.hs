-------------------------------------------------------------------------------
-- Module    :  Scanning
-- Copyright :  (c) 2008 Stefan Holdermans

module Language.Scanning
  ( Scanner
  , satisfy
  , ignore
  , err
  , module Language.Idiom
  , module Language.Position
  , Scan
  , scan
  , epsilon
  , char
  , anyChar
  , anyCharBut
  , oneOf
  , noneOf
  , (<..>)
  , letter
  , lower
  , upper
  , digit
  , octDigit
  , hexDigit
  , number
  , alphaNum
  , control
  , space
  , mark
  , punctuation
  , symbol
  , separator
  , printable
  , ascii
  , latin1
  , asciiUpper
  , asciiLower
  , string
  , strings
  , (|>)
  , (<|)
  ) where

import Language.Idiom
import Language.Position

import Control.Arrow (second)
import Data.Char
import Data.Traversable (for)

-------------------------------------------------------------------------------
-- state-machine functor

data Machine a = State (Maybe a) [(Char -> Bool, Machine (Char -> a))]

instance Functor Machine where
  fmap f (State mx delta) = State mx' delta'
    where
      mx'    = fmap f mx
      delta' = fmap (second (fmap (f .))) delta

-------------------------------------------------------------------------------
-- machine combinators

cat :: Machine (a -> b) -> Machine a -> Machine b
cat ~(State mf deltaf) r@(~(State mx deltax)) = State my (deltay ++ deltay')
  where
    my      = mf <*> mx
    deltay  = maybe [] ((`fmap` deltax) . second . fmap . (.)) mf
    deltay' = fmap (second ((`cat` r) . fmap flip)) deltaf

alt :: Machine a -> Machine a -> Machine a
alt ~(State mxl deltal) ~(State mxr deltar) =
  State (mxl <|> mxr) (deltal <|> deltar)

-------------------------------------------------------------------------------
-- action functor

data Action a
  = Return a
  | Ignore
  | Error String

instance Functor Action where
  fmap f (Return x)  = Return (f x)
  fmap _ Ignore      = Ignore
  fmap _ (Error str) = Error str

catAct                       :: Action (a -> b) -> Action a -> Action b
catAct (Return f) (Return x) =  Return (f x)
catAct (Error str) _         =  Error str
catAct _ (Error str)         =  Error str
catAct _ _                   =  Ignore

invTokErr         :: Pos -> String -> (Pos, Action a)
invTokErr pos inv =  (pos, Error ("invalid token " ++ inv))

-------------------------------------------------------------------------------
-- scanner functor

newtype Scanner a = S {unS :: Machine (Action a)}

instance Functor Scanner where
  fmap f = S . fmap (fmap f) . unS

-------------------------------------------------------------------------------
-- primitive scanner

satisfy   :: (Char -> Bool) -> Scanner Char
satisfy p =  S (State Nothing [(p, State (Just Return) [])])

err     :: String -> Scanner a
err str =  S (State (Just (Error str)) [])

-------------------------------------------------------------------------------
-- primitive scanner combinator

ignore :: Scanner a -> Scanner b
ignore =  S . cat (State (Just (catAct Ignore)) []) . unS

-------------------------------------------------------------------------------
-- applicative interface

instance Applicative Scanner where
  pure x    = S (State (Just (Return x)) [])
  sl <*> sr = S (fmap catAct (unS sl) `cat` unS sr)

instance Alternative Scanner where
  empty     = S (State Nothing [])
  sl <|> sr = S (unS sl `alt` unS sr)

-------------------------------------------------------------------------------
-- source positions

initialPos :: Pos
initialPos =  Pos 1 1

advPos                :: Char -> Pos -> Pos
advPos '\n' (Pos l _) =  Pos (l + 1) 1
advPos _ ( Pos l c)   =  Pos l (c + 1)
advPos _ _            =  error "Scanning.advPos: internal error"

-------------------------------------------------------------------------------
-- dynamics

advMachines   :: Char -> [Machine a] -> [Machine (Char -> a)]
advMachines c =  foldr adv []
  where
    adv ~(State _ delta) = ([next | ~(p, next) <- delta, p c] ++)

-- a prefix match: the associated action, the source position of the remaining
-- postfix, and the remaining postfix
type Match a = Maybe (a, Pos, String)

updMatch :: [Machine b] -> (b -> a) -> Pos -> String -> Match a -> Match a
updMatch [] _ _ _ m                         =  m
updMatch (State (Just x) _ : _) f pos inp _ =  Just (f x, pos, inp)
updMatch (_ : machines) f pos inp mx        =  updMatch machines f pos inp mx

-- longest prefix match
lpm         :: Machine a -> Pos -> String -> Match a
lpm machine =  go [machine] Nothing id
  where
    go :: [Machine b] -> Match a -> (b -> a) -> Pos -> String -> Match a
    go machines m f pos ""           = updMatch machines f pos "" m
    go [] m _ _ _                    = m
    go machines m f pos inp@(c : cs) =
      let machines' = advMachines c machines
          m'        = updMatch machines f pos inp m
          f'        = f . ($ c)
          pos'      = advPos c pos
      in  go machines' m' f' pos' cs

-- a scan: a list of either an error message or an action, associated with a
-- source position 
type Scan a = [(Pos, Either String a)]

(>:)                    :: (Pos, Action a) -> Scan a -> Scan a
(pos, Return x)  >: scn =  (pos, Right x) : scn
(pos, Error str) >: scn =  (pos, Left str) : scn
_                >: scn =  scn

scan   :: Scanner a -> String -> Scan a
scan s =  go initialPos "" initialPos
  where
    go invpos invrev pos inp = let inv = invTokErr invpos (reverse invrev) in
                               case lpm (unS s) pos inp of
      Nothing                -> case inp of 
        []     -> if null invrev then [] else inv >: []
        c : cs -> go invpos (c : invrev) (advPos c pos) cs
      Just (act, pos', inp') -> let scn = (pos, act) >: go pos' "" pos' inp'
                                in  if null invrev then scn else inv >: scn

-------------------------------------------------------------------------------
-- derived scanners

infixl 5 <..>

epsilon :: Scanner ()
epsilon =  pure ()

char   :: Char -> Scanner Char
char c =  satisfy (== c)

anyChar :: Scanner Char
anyChar =  satisfy (const True)

anyCharBut   :: Char -> Scanner Char
anyCharBut c =  satisfy (/= c)

oneOf    :: [Char] -> Scanner Char
oneOf cs =  satisfy (`elem` cs)

noneOf    :: [Char] -> Scanner Char
noneOf cs =  satisfy (`notElem` cs)

(<..>)     :: Char -> Char -> Scanner Char
cl <..> cr =  satisfy (\c -> c >= cl && c <= cr)

-- character classification

letter :: Scanner Char
letter =  satisfy isLetter

lower :: Scanner Char
lower =  satisfy isLower

upper :: Scanner Char
upper =  satisfy isUpper

digit :: Scanner Char
digit =  satisfy isDigit

octDigit :: Scanner Char
octDigit =  satisfy isOctDigit

hexDigit :: Scanner Char
hexDigit =  satisfy isHexDigit

number :: Scanner Char
number =  satisfy isNumber

alphaNum :: Scanner Char
alphaNum =  satisfy isAlphaNum

control :: Scanner Char
control =  satisfy isControl

space :: Scanner Char
space =  satisfy isSpace

mark :: Scanner Char
mark =  satisfy isMark

punctuation :: Scanner Char
punctuation =  satisfy isPunctuation

symbol :: Scanner Char
symbol =  satisfy isSymbol

separator :: Scanner Char
separator =  satisfy isSeparator

printable :: Scanner Char
printable =  satisfy isPrint

-- subranges

ascii :: Scanner Char
ascii =  satisfy isAscii

latin1 :: Scanner Char
latin1 =  satisfy isLatin1

asciiUpper :: Scanner Char
asciiUpper =  satisfy isAsciiUpper

asciiLower :: Scanner Char
asciiLower =  satisfy isAsciiLower

-- strings

string     :: String -> Scanner String
string str =  for str char

strings :: [String] -> Scanner String
strings =  foldr ((<|>) . string) empty

------------------------------------------------------------------------------
-- derived scanner combinators

infixl 2 |>, <|

(<|)   :: Scanner a -> Scanner b -> Scanner a
l <| r =  l <|> ignore r

(|>)   :: Scanner a -> Scanner b -> Scanner b
l |> r =  ignore l <|> r