-- Modulo con definiciones estandar para evaluacion de logica modal
module Modal
  ( Model (..)
  , TagMapping
  , Env
  , toFormula
  , buildFrame
  , buildTag
  , (||-)
  , validInModel
  , satisfiableInModel
) where

import Common
import Control.Monad (liftM, liftM2)
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import Frame

type World = String
type TagMapping w a = M.Map w (S.Set a)
{-
TODO implementar instancia de Show para TagMapping. Para hacer esto deberia
convertirlo a un newtype
-}

type Env a = (Model World a, DefTable a)

data Model w a = Model
      { frame :: Graph w
      , tag   :: TagMapping w a
      } deriving Show

toFormula :: Eq a => LitFormula a -> Formula a
toFormula = toFormula' . sub []
  where
    toFormula' :: LitFormula a -> Formula a
    toFormula' LBottom        = Bottom
    toFormula' LTop           = Top
    toFormula' (LAtomic x)    = Atomic x
    toFormula' (LAnd f1 f2)   = And (toFormula' f1) (toFormula' f2)
    toFormula' (LOr f1 f2)    = Or (toFormula' f1) (toFormula' f2)
    toFormula' (LImply f1 f2) = Imply (toFormula' f1) (toFormula' f2)
    toFormula' (LIff f1 f2)   = Iff (toFormula' f1) (toFormula' f2)
    toFormula' (LNot f)       = Not (toFormula' f)
    toFormula' (LSquare f)    = Square (toFormula' f)
    toFormula' (LDiamond f)   = Diamond (toFormula' f)
    toFormula' LSub{}         = error "Impossible"

    sub :: Eq a => [(a, LitFormula a)] -> LitFormula a -> LitFormula a
    sub _ LBottom = LBottom
    sub _ LTop    = LTop
    sub env p@(LAtomic y)  = fromMaybe p (lookup y env)
    sub env (LAnd p1 p2)   = LAnd (sub env p1) (sub env p2)
    sub env (LOr  p1 p2)   = LOr (sub env p1) (sub env p2)
    sub env (LImply p1 p2) = LImply (sub env p1) (sub env p2)
    sub env (LIff p1 p2)   = LIff (sub env p1) (sub env p2)
    sub env (LNot p)       = LNot (sub env p)
    sub env (LSquare p)    = LSquare (sub env p)
    sub env (LDiamond p)   = LDiamond (sub env p)
    sub env (LSub q r y)   = let r' = sub env r in sub ((y,r'):env) q

worlds :: Model w a -> [w]
worlds = vertices . frame

transitions :: Model w a -> M.Map w [w]
transitions = edges . frame

buildFrame :: Ord w => [(w, [w])] -> Graph w
buildFrame = graphFromEdges

buildTag :: (Ord w, Ord a) => [(w, [a])] -> TagMapping w a
buildTag = fmap S.fromList . M.fromListWith (++)

validAtoms :: Model World a -> World -> S.Set a
validAtoms m w = fromMaybe S.empty (M.lookup w l)
          where l = tag m

nextStates :: Model World a -> World -> [World]
nextStates m = neighbours (frame m)

model :: Reader (Env a) (Model World a)
model = asks fst

defs :: Reader (Env a) (DefTable a)
defs = asks snd

evalInWorlds :: Eq a => Formula a -> [World] -> Reader (Env a) [Bool]
evalInWorlds f = mapM (||- f)

(||-) :: Eq a => World -> Formula a -> Reader (Env a) Bool
_ ||- Bottom         = return False
_ ||- Top            = return True
w ||- (Atomic p)     = model >>= \m -> return $ elem p (validAtoms m w)
w ||- (Not f)        = liftM not (w ||- f)
w ||- (And f1 f2)    = liftM2 (&&) (w ||- f1) (w ||- f2)
w ||- (Or  f1 f2)    = liftM2 (||) (w ||- f1) (w ||- f2)
w ||- (Imply f1 f2)  = liftM2 (<=) (w ||- f1) (w ||- f2)
w ||- (Iff f1 f2)    = liftM2 (==) (w ||- f1) (w ||- f2)
w ||- (Square f)     = model >>= \m -> liftM and $ evalInWorlds f (nextStates m w)
w ||- (Diamond f)    = model >>= \m -> liftM or  $ evalInWorlds f (nextStates m w)

satisfiableInModel :: Eq a => Formula a -> Reader (Env a) Bool
satisfiableInModel f = do m <- model
                          bs <- evalInWorlds f (worlds m)
                          return (and bs)

validInModel :: Eq a => Formula a -> Reader (Env a) Bool
validInModel f = do b <- satisfiableInModel (Not f)
                    return (not b)
