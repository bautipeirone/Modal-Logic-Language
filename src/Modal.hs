-- Modulo con definiciones estandar para evaluacion de logica modal
module Modal
  ( Formula (..)
  , Model (..)
  , TagMapping
  , Graph
  , Env
  , DefTable
  , buildFrame
  , buildTag
  , (||-)
  , validInModel
  , satisfiableInModel
) where

import Control.Monad (ap, liftM, liftM2)
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import Frame

type World = String
type TagMapping w a = M.Map w (S.Set a)

type DefTable a = [(a, Formula a)]
type Env a = (Model World a, DefTable a)

data Exception = UndefVar | UnderWorld deriving (Show, Eq)

data Model w a = Model
      { frame :: Graph w
      , tag   :: TagMapping w a
      } deriving Show

worlds :: Model w a -> [w]
worlds = vertices . frame

transitions :: Model w a -> M.Map w [w]
transitions = edges . frame

buildFrame :: Ord w => [(w, [w])] -> Graph w
buildFrame = graphFromEdges

buildTag :: (Ord w, Ord a) => [(w, [a])] -> TagMapping w a
buildTag = fmap S.fromList . M.fromListWith (++)

data Formula a  = Bottom
                | Top
                | Sub (Formula a) (Formula a) a
                | Atomic  a
                | Global  a
                | Not     (Formula a)
                | And     (Formula a) (Formula a)
                | Or      (Formula a) (Formula a)
                | Imply   (Formula a) (Formula a)
                | Iff     (Formula a) (Formula a)
                | Square  (Formula a)
                | Diamond (Formula a)
                deriving Show

instance Functor Formula where
  fmap g Bottom        = Bottom
  fmap g Top           = Top
  fmap g (Atomic p)    = Atomic (g p)
  fmap g (Global v)    = Global (g v)
  fmap g (Not f)       = Not (fmap g f)
  fmap g (And   f1 f2) = And (fmap g f1) (fmap g f2)
  fmap g (Or    f1 f2) = Or  (fmap g f1) (fmap g f2)
  fmap g (Imply f1 f2) = Imply (fmap g f1) (fmap g f2)
  fmap g (Iff   f1 f2) = Iff (fmap g f1) (fmap g f2)
  fmap g (Square  f)   = Square (fmap g f)
  fmap g (Diamond f)   = Diamond (fmap g f)

-- sub :: Eq a => Formula a -> Formula a -> a -> Formula a
-- sub phi p x  = do y <- phi
--                   if y == x then p
--                   else return y

validAtoms :: Model World a -> World -> S.Set a
validAtoms m w = fromMaybe S.empty (M.lookup w l)
          where l = tag m

nextStates :: Model World a -> World -> [World]
nextStates m = neighbours (frame m)

model :: ReaderT (Env a) (Except String) (Model World a)
model = asks fst

defs :: ReaderT (Env a) (Except String) (DefTable a)
defs = asks snd

-- (||-) :: Eq a => Model World a -> Formula a -> World -> Bool
-- (||-) _ Bottom        _ = False
-- (||-) _ Top           _ = True
-- (||-) m (Atomic p)    w = elem p $ validAtoms m w
-- (||-) m (Not f)       w = not $ (||-) m f w
-- (||-) m (And f1 f2)   w = (||-) m f1 w && (||-) m f2 w
-- (||-) m (Or f1 f2)    w = (||-) m f1 w || (||-) m f2 w
-- (||-) m (Imply f1 f2) w = (||-) m f1 w <= (||-) m f2 w
-- (||-) m (Iff f1 f2)   w = (||-) m f1 w == (||-) m f2 w
-- (||-) m (Square f)    w = all ((||-) m f) (nextStates m w)
-- (||-) m (Diamond f)   w = any ((||-) m f) (nextStates m w)
(||-) :: Eq a => World -> Formula a -> ReaderT (Env a) (Except String) Bool
_ ||- Bottom         = return False
_ ||- Top            = return True
w ||- (Atomic p)     = do m <- model
                          return $ elem p (validAtoms m w)
w ||- (Global v)     = do ds <- defs
                          maybe (throwError "Variable no definida") (w ||-) (lookup v ds)
w ||- (Not f)        = liftM not (w ||- f)
w ||- (And f1 f2)    = liftM2 (&&) (w ||- f1) (w ||- f2)
w ||- (Or  f1 f2)    = liftM2 (||) (w ||- f1) (w ||- f2)
w ||- (Imply f1 f2)  = liftM2 (<=) (w ||- f1) (w ||- f2)
w ||- (Iff f1 f2)    = liftM2 (==) (w ||- f1) (w ||- f2)
w ||- (Square f)     = do m <- model
                          liftM and $ mapM (||- f) (nextStates m w)
w ||- (Diamond f)    = do m <- model
                          liftM or  $ mapM (||- f) (nextStates m w)
-- liftM and (sequence [readT True, readT False])
-- 

satisfiableInModel :: Eq a => Formula a -> Reader (Env a) Bool
satisfiableInModel = undefined
-- satisfiableInModel f = do (model, defs) <- ask
                          -- let w = worlds model
                          -- return $ any ((||-) m f) w

validInModel :: Eq a => Formula a -> Reader (Env a) Bool
-- validInModel m f = not $ satisfiableInModel m $ Not f
validInModel = undefined
