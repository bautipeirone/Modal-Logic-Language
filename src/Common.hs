{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

module Common
  ( Scheme (..)
  , Formula (..)
  , World
  , Atom
  , DefTable
  , atoms
) where


import Data.List (singleton, nub)

type World = String
type Atom  = String
type DefTable a = [(a, Formula a)]

{--------- Definicion de sintaxis concreta ---------}
data Scheme a = LBottom
              | LTop
              | LAtomic a
              | LIdent a
              | LSub     (Scheme a) (Scheme a) a
              | LAnd     (Scheme a) (Scheme a)
              | LOr      (Scheme a) (Scheme a)
              | LImply   (Scheme a) (Scheme a)
              | LIff     (Scheme a) (Scheme a)
              | LNot     (Scheme a)
              | LSquare  (Scheme a)
              | LDiamond (Scheme a)
              deriving Show
{---------------------------------------------------}

{--------- Definicion de sintaxis abstracta ---------}
data Formula a  = Bottom
                | Top
                | Atomic a
                | And     (Formula a) (Formula a)
                | Or      (Formula a) (Formula a)
                | Imply   (Formula a) (Formula a)
                | Iff     (Formula a) (Formula a)
                | Not     (Formula a)
                | Square  (Formula a)
                | Diamond (Formula a)
                deriving (Show, Foldable, Functor)

atoms :: Eq a => Formula a -> [a]
atoms = nub . foldMap singleton
{----------------------------------------------------}
