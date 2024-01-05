-- Modulo con definiciones estandar para evaluacion de logica modal
module Modal
  ( Formula (..)
  , sub
) where

import Control.Monad (ap, liftM)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Frame

type World = String

data Model w a = Model
      { frame :: Graph w
      , tag   :: M.Map w (S.Set a)
      } deriving Show

worlds :: Model w a -> [w]
worlds = vertices . frame

transitions :: Model w a -> M.Map w [w]
transitions = edges . frame

data Formula a  = Bottom
                | Top
                | Atomic  a
                | Not     (Formula a)
                | And     (Formula a) (Formula a)
                | Or      (Formula a) (Formula a)
                | Imply   (Formula a) (Formula a)
                | Iff     (Formula a) (Formula a)
                | Square  (Formula a)
                | Diamond (Formula a)
                deriving Show

-- pattern Top :: Formula a
-- pattern Top = Not Bottom

-- pattern Imply :: Formula a -> Formula a -> Formula a
-- pattern Imply p q = Or (Not p) q

-- pattern Diamond :: Formula a -> Formula a
-- pattern Diamond p = Not (Square (Not p))

instance Functor Formula where
  fmap = liftM

instance Applicative Formula where
  pure = Atomic
  (<*>) = ap

instance Monad Formula where
  return = pure
  Bottom >>= g = Bottom
  Top    >>= g = Top
  (Atomic p) >>= g = g p
  (Not f) >>= g = Not (f >>= g)
  (And f1 f2) >>= g = And (f1 >>= g) (f2 >>= g)
  (Or f1 f2) >>= g = Or (f1 >>= g) (f2 >>= g)
  (Square f) >>= g = Square (f >>= g)
  (Imply f1 f2) >>= g = Imply (f1 >>= g) (f2 >>= g)
  (Iff f1 f2) >>= g = Iff (f1 >>= g) (f2 >>= g)
  -- (Diamond f) >>= g = Diamond (f >>= g)

sub :: Eq a => Formula a -> Formula a -> a -> Formula a
sub phi p x  = do y <- phi
                  if y == x then p
                  else return y

{-
validAtoms :: Model World a -> World -> [a]
validAtoms m w = fromMaybe S.empty (M.lookup w l)
          where l = tag m

nextStates :: Model World a -> World -> [World]
nextStates m w = fromMaybe [] (M.lookup w r)
          where r = transitions m

-- Notar que se puede omitir la semantica del operador Diamond
(||-) :: Eq a => Model World a -> Formula a -> World -> Bool
(||-) _ Bottom      x = False
(||-) m (Atomic p)  x = elem p $ validAtoms m x
(||-) m (Not f)     x = not $ (||-) m f x
(||-) m (And f1 f2) x = ((||-) m f1 x) && ((||-) m f2 x)
(||-) m (Or f1 f2)  x = ((||-) m f1 x) || ((||-) m f2 x)
(||-) m (Iff f1 f2) x = ((||-) m f1 x) == ((||-) m f2 x)
(||-) m (Square f)  x = all ((||-) m f) (nextStates m x)
-- (||-) m (Diamond f) x = any ((||-) m f) (nextStates m x)

satisfiableInModel :: Eq a => Model a -> Formula a -> Bool
satisfiableInModel m f = any ((||-) m f) w
                    where w = states m

validInModel :: Eq a => Model a -> Formula a -> Bool
validInModel m f = not $ satisfiableInModel m $ Not f
-}