{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

module Common
  ( DefTable
  , Scheme
  , Lookup
  , LitFormula (..)
  , Formula (..)
  , Result
  , World
  , Atom
  , Trace (..)
  , ModelTrace (..)
  , Eval
  , atoms
  , undefVarError
  , liftFormula
  , subforms    -- Remove it
  , worldSteps  -- Remove it
) where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Error.Class (throwError)
import Data.List (singleton, nub)

type World = String
type Atom  = String

type Result a = Either String a

type DefTable a = [(a, Formula a)]

type Lookup m a = ReaderT (DefTable a) (Either String) (m a)
type Scheme a = Lookup LitFormula a


subforms :: [Trace] -> Either [Trace] [(World, Trace)]
subforms = Left

worldSteps :: [(World, Trace)] -> Either [Trace] [(World, Trace)]
worldSteps = Right

-- A Trace contains the result of evaluation of a formula and all of its subformulas
data Trace = Trace  { getTraceHead   :: Formula Atom
                    , evalTrace   :: Bool
                    , getSubtrace :: Either [Trace] [(World, Trace)] -- Left is used for subformulas
                                                                     -- while Right is for world changes
                    } deriving Show

-- A ModelTrace is like a Trace but for operations that are computed over all
-- the worlds of a model. This way, this type contains the result of the operation
-- and the trace of the formula for each world.
data ModelTrace = ModelTrace
                    { getFormula :: Formula Atom
                    , evalModel :: Bool
                    , getWorldTraces :: [(World, Trace)]
                    } deriving Show

-- Abstracts evaluation traces
type Eval = Either Trace ModelTrace

-- Only intended as an intermediate result of the parser. It is not used
-- for evaluation.
data LitFormula a = LBottom
                  | LTop
                  | LAtomic a
                  | LSub     (LitFormula a) (LitFormula a) a
                  | LAnd     (LitFormula a) (LitFormula a)
                  | LOr      (LitFormula a) (LitFormula a)
                  | LImply   (LitFormula a) (LitFormula a)
                  | LIff     (LitFormula a) (LitFormula a)
                  | LNot     (LitFormula a)
                  | LSquare  (LitFormula a)
                  | LDiamond (LitFormula a)
                  deriving Show

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

undefVarError :: Atom -> Scheme Atom
undefVarError s = throwError $ "Undefined identifier " ++ s

atoms :: Eq a => Formula a -> [a]
atoms = nub . foldMap singleton

liftFormula :: Formula Atom -> Scheme Atom
liftFormula Bottom        = return LBottom
liftFormula Top           = return LTop
liftFormula (Atomic x)    = return (LAtomic x)
liftFormula (Not f)       = liftM  LNot (liftFormula f)
liftFormula (Square f)    = liftM  LSquare (liftFormula f)
liftFormula (Diamond f)   = liftM  LDiamond (liftFormula f)
liftFormula (And f1 f2)   = liftM2 LAnd (liftFormula f1) (liftFormula f2)
liftFormula (Or  f1 f2)   = liftM2 LOr  (liftFormula f1) (liftFormula f2)
liftFormula (Imply f1 f2) = liftM2 LImply  (liftFormula f1) (liftFormula f2)
liftFormula (Iff  f1 f2)  = liftM2 LIff  (liftFormula f1) (liftFormula f2)
