module Common
  ( DefTable
  , Scheme
  , LitFormula (..)
  , Formula (..)
  , undefError
  , liftFormula
) where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Error.Class (throwError)

type DefTable a = [(a, Formula a)]
type Scheme a = StateT (DefTable a) (Either String) (LitFormula a)

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
                deriving Show

instance Functor Formula where
  fmap g Bottom        = Bottom
  fmap g Top           = Top
  fmap g (Atomic p)    = Atomic (g p)
  -- fmap g (Global v)    = Global (g v)
  fmap g (Not f)       = Not (fmap g f)
  fmap g (And   f1 f2) = And (fmap g f1) (fmap g f2)
  fmap g (Or    f1 f2) = Or  (fmap g f1) (fmap g f2)
  fmap g (Imply f1 f2) = Imply (fmap g f1) (fmap g f2)
  fmap g (Iff   f1 f2) = Iff (fmap g f1) (fmap g f2)
  fmap g (Square  f)   = Square (fmap g f)
  fmap g (Diamond f)   = Diamond (fmap g f)

undefError :: Show a => a -> Scheme a
undefError s = throwError $ "Undefined identifier " ++ show s

liftFormula :: Eq a => Formula a -> Scheme a
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
