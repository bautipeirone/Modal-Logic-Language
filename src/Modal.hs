-- Modulo con definiciones estandar para evaluacion de logica modal
module Modal
  ( Model (..)
  , TagMapping
  , Trace (..)
  , ModelTrace (..)
  , EvalM
  , emptyModel
  , buildFrame
  , buildTag
  , (||-)
  , validInModel
  , satisfiableInModel
) where

import Common
import Prelude hiding (log)
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import Frame

type TagMapping w a = M.Map w (S.Set a)

-- A Trace contains the result of evaluation of a formula and all of its subformulas
data Trace = Trace
                { getTraceHead   :: Formula Atom
                , evalTrace   :: Bool
                , getSubtrace :: Either [Trace] [(World, Trace)] -- Left is used for subformulas
                                                                 -- while Right is for world changes
                } deriving Show

subforms :: [Trace] -> Either [Trace] [(World, Trace)]
subforms = Left

worldSteps :: [(World, Trace)] -> Either [Trace] [(World, Trace)]
worldSteps = Right

type EvalM a = Reader (Model World Atom) a

-- A ModelTrace is like a Trace but for operations that are computed over all
-- the worlds of a model. This way, this type contains the result of the operation
-- and the trace of the formula for each world.
data ModelTrace = ModelTrace
                    { getFormula :: Formula Atom
                    , evalModel :: Bool
                    , getWorldTraces :: [(World, Trace)]
                    } deriving Show


data Model w a = Model
      { frame :: Graph w
      , tag   :: TagMapping w a
      } deriving Show

emptyModel :: Model World Atom
emptyModel = Model {frame = emptyFrame, tag = M.empty}

worlds :: Model w a -> [w]
worlds = vertices . frame

buildFrame :: Ord w => [(w, [w])] -> Graph w
buildFrame = graphFromEdges

buildTag :: (Ord w, Ord a) => [(w, [a])] -> TagMapping w a
buildTag = fmap S.fromList . M.fromListWith (++)

validAtoms :: Model World Atom -> World -> S.Set Atom
validAtoms m w = fromMaybe S.empty (M.lookup w l)
          where l = tag m

nextStates :: Model World a -> World -> [World]
nextStates m = neighbours (frame m)

(||-) :: World -> Formula Atom -> EvalM Trace
_ ||- f@Bottom         = return $ Trace f False (subforms [])
_ ||- f@Top            = return $ Trace f True  (subforms [])
w ||- f@(Atomic p)     = do model <- ask
                            let b = p `elem` validAtoms model w
                            return $ Trace f b (subforms [])
w ||- f@(Not f1)       = do t <- w ||- f1
                            let b = not (evalTrace t)
                            return $ Trace f b (subforms [t])
w ||- f@(And f1 f2)    = do t1 <- w ||- f1
                            t2 <- w ||- f2
                            let b = evalTrace t1 && evalTrace t2
                            return $ Trace f b (subforms [t1,t2])
w ||- f@(Or f1 f2)     = do t1 <- w ||- f1
                            t2 <- w ||- f2
                            let b = evalTrace t1 || evalTrace t2
                            return $ Trace f b (subforms [t1,t2])
w ||- f@(Imply f1 f2)  = do t1 <- w ||- f1
                            t2 <- w ||- f2
                            let b = evalTrace t1 <= evalTrace t2
                            return $ Trace f b (subforms [t1,t2])
w ||- f@(Iff f1 f2)    = do t1 <- w ||- f1
                            t2 <- w ||- f2
                            let b = evalTrace t1 == evalTrace t2
                            return $ Trace f b (subforms [t1,t2])
w ||- f@(Square f1)    = do model <- ask
                            ts <- mapM (||- f1) (nextStates model w)
                            let b = all evalTrace ts
                            let subTraces = worldSteps (zip (nextStates model w) ts)
                            return $ Trace f b subTraces
w ||- f@(Diamond f1)   = do model <- ask
                            ts <- mapM (||- f1) (nextStates model w)
                            let b = any evalTrace ts
                            let subTraces = worldSteps (zip (nextStates model w) ts)
                            return $ Trace f b subTraces

satisfiableInModel :: Formula Atom -> EvalM ModelTrace
satisfiableInModel f = do m <- ask
                          ts <- mapM (||- f) (worlds m)
                          let b = any evalTrace ts
                          let wTraces = zip (worlds m) ts
                          return $ ModelTrace f b wTraces

validInModel :: Formula Atom -> EvalM ModelTrace
validInModel f = do m <- ask
                    ts <- mapM (||- f) (worlds m)
                    let b = all evalTrace ts
                    let wTraces = zip (worlds m) ts
                    return $ ModelTrace f b wTraces
