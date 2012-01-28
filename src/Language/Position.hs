-------------------------------------------------------------------------------
-- Module    :  Position
-- Copyright :  (c) 2008 Stefan Holdermans

module Language.Position
  ( Pos (..)
  ) where

-------------------------------------------------------------------------------
-- source positions

data Pos = Pos {line :: !Int, column :: !Int} | EOF deriving (Eq, Ord)

instance Show Pos where
  show (Pos l c) = show l ++ ":" ++ show c
  show EOF       = "at end of input"