-- Modulo con definiciones estandar para evaluacion de logica modal
module Modal
  ( Model (..)
  , TagMapping
  , Trace (..)
  , ModelTrace (..)
  , EvalM
  , emptyModel
  , toFormula
  , buildFrame
  , buildTag
  , (||-)
  , validInModel
  , satisfiableInModel
) where

import Common
import Prelude hiding (log)
import Control.Monad (liftM, liftM2)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Bifunctor

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import Frame

type TagMapping w a = M.Map w (S.Set a)
{-
TODO implementar instancia de Show para TagMapping. Para hacer esto deberia
convertirlo a un newtype
-}


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

validAtoms :: Model World Atom -> World -> S.Set Atom
validAtoms m w = fromMaybe S.empty (M.lookup w l)
          where l = tag m

nextStates :: Model World a -> World -> [World]
nextStates m = neighbours (frame m)

evalInWorlds :: Formula Atom -> [World] -> EvalM [Trace]
evalInWorlds f = mapM (||- f)

-- log :: Formula Atom -> Bool -> EvalM ()
-- log f b = do   <- get
              -- tell $ indent n $ show f ++ ": " ++ show b ++ "\n"

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

{-
--propConstantEval :: Formula Atom -> Bool -> EvalM Trace
propConstantEval f b = return $ Trace f b (subforms [])

--propUnaryEval :: World -> Formula Atom -> (Bool -> Bool) -> (Bool, Either [Trace] [(World, Trace)])
propUnaryEval w f op = do t <- w ||- f
                          let b = op (evalTrace t)
                          return $ Trace _ b (subforms [t])

--propBinaryEval :: World -> Formula Atom -> Formula Atom -> (Bool -> Bool -> Bool) -> EvalM Trace
propBinaryEval w f1 f2 op = do t1 <- w ||- f1
                               f2 <- w ||- f2
                               let b = evalTrace t1 `op` evalTrace t2
                               return $ Trace f b 
-}
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
