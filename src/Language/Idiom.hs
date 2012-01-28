-------------------------------------------------------------------------------
-- Module    :  Idiom
-- Copyright :  (c) 2008 Stefan Holdermans

module Language.Idiom
  ( module Control.Applicative
  , (<$$>)
  , (<^>)
  , (<^)
  , (<^^>)
  , (<!>)
  , (<!)
  , (<!!>)
  , (<+>)
  , (<||>)
  , (<?>)
  , (<??>)
  , opt
  , (<<?>)
  , (<?>>)
  , packed
  , choice
  , foldrMany
  , foldrSome
  , foldlMany
  , foldlSome
  , foldrSepMany
  , foldrSepSome
  , foldlSepMany
  , foldlSepSome
  , sepMany
  , sepSome
  , chainr
  , chainl
  ) where

import Control.Applicative

-------------------------------------------------------------------------------
-- derived combinators

infixl 5 <?>, <??>, <<?>, <?>>
infixl 4 <$$>, <^>, <^, <^^>, <!>, <!, <!!>, <+>
infixl 3 <||>
infixl 2 `opt`

(<$$>)   :: (Applicative f) => f a -> (a -> b) -> f b
v <$$> f =  v <**> pure f

(<^>)   :: (Applicative f) => f (a -> b -> c) -> f b -> f (a -> c)
l <^> r =  flip <$> l <*> r

(<^)   :: (Applicative f) => f (a -> b -> c) -> f d -> f (b -> a -> c)
l <^ r =  flip <$> l <* r

(<^^>)   :: (Applicative f) => f b -> f (a -> b -> c) -> f (a -> c)
l <^^> r =  l <**> (flip <$> r)

(<!>)   :: (Applicative f) => (a -> b -> c) -> f b -> f (a -> c)
f <!> v =  flip f <$> v

(<!)   :: (Applicative f) => (a -> b -> c) -> f d -> f (b -> a -> c)
f <! v =  flip f <$ v

(<!!>)   :: (Applicative f) => f b -> (a -> b -> c) -> f (a -> c)
v <!!> f =  v <$$> flip f

(<+>)   :: (Applicative f) => f a -> f b -> f (a, b)
l <+> r =  (,) <$> l <*> r

(<||>)   :: (Alternative f) => f a -> f b -> f (Either a b)
l <||> r =  Left <$> l <|> Right <$> r

(<?>)   :: (Alternative f) => a -> f a -> f a
x <?> v =  v <|> pure x

(<??>), opt :: (Alternative f) => f a -> a -> f a
v <??> x    =  v <|> pure x
opt         =  (<??>)

(<<?>)   :: (Alternative f) => f (a -> a) -> f a -> f a
l <<?> r =  id <?> l <*> r  

(<?>>)   :: (Alternative f) => f a -> f (a -> a) -> f a
l <?>> r =  l <**> r <??> id

packed       :: (Applicative f) => f a -> f b -> f c -> f c
packed l r v =  l *> v <* r

choice :: (Alternative f) => [f a] -> f a
choice =  foldr (<|>) empty

foldrMany        :: (Alternative f) => (a -> b -> b) -> b -> f a -> f b
foldrMany op e v =  go
  where
    go = op <$> v <*> go `opt` e

foldrSome        :: (Alternative f) => (a -> b -> b) -> b -> f a -> f b
foldrSome op e v =  op <$> v <*> go
  where
    go = op <$> v <*> go `opt` e

foldlMany        :: (Alternative f) => (b -> a -> b) -> b -> f a -> f b
foldlMany op e v =  go <*> pure e
  where
    go = op <!> v <!!> (.) <*> go `opt` id

foldlSome        :: (Alternative f) => (b -> a -> b) -> b -> f a -> f b
foldlSome op e v =  op e <$> v <**> go
  where
    go = op <!> v <!!> (.) <*> go `opt` id

foldrSepMany :: (Alternative f) => (a -> b -> b) -> b -> f c -> f a -> f b
foldrSepMany op e u v = op <$> v <*> go `opt` e
  where
    go = op <$ u <*> v <*> go `opt` e

foldrSepSome :: (Alternative f) => (a -> b -> b) -> b -> f c -> f a -> f b
foldrSepSome op e u v = op <$> v <*> go
  where
    go = op <$ u <*> v <*> go `opt` e

foldlSepMany :: (Alternative f) => (b -> a -> b) -> b -> f c -> f a -> f b
foldlSepMany op e u v = op e <$> v <**> go `opt` e
  where
    go = op <! u <*> v <!!> (.) <*> go `opt` id

foldlSepSome :: (Alternative f) => (b -> a -> b) -> b -> f c -> f a -> f b
foldlSepSome op e u v = op e <$> v <**> go
  where
    go = op <! u <*> v <!!> (.) <*> go `opt` id

sepMany, sepSome :: (Alternative f) => f b -> f a -> f [a]
sepMany          =  foldrSepMany (:) []
sepSome          =  foldrSepSome (:) []

chainr     :: (Alternative f) => f (a -> a -> a) -> f a -> f a
chainr u v =  v <?>> go
  where
    go = u <^> v <?>> go

chainl     :: (Alternative f) => f (a -> a -> a) -> f a -> f a
chainl u v =  v <**> go
  where
    go = (.) <!> (u <^> v) <*> go `opt` id