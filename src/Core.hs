{-# LANGUAGE DeriveFunctor #-}

module Core
  ( Stmt (..)
  , SetStmt (..)
  , Op (..)
  , Eval (..)
  , runCmd
  , GStmt (..)
  , GOp (..)
  , SStmt
  , Stmt
  , SOp
  , Op
  , module Modal
  , module Axioms
  ) where

-- import PrettyPrinter
import Common
import State
import Frame
import Modal
import Axioms

import Control.Monad.Reader ( runReader )

-- class Trace a where
--   getTraceHead :: a w -> w
--   eval :: a -> Bool
--   getSubtraces :: Foldable t => a w -> t (a w)

-- Abstracts evaluation traces
data Eval = F Trace | M ModelTrace | A AxiomsTrace


{--------- Sentencias y operaciones del lenguaje ---------}
data SetStmt  = Frame (Graph World)
              | Tag (TagMapping World Atom)
                    deriving Show

data GStmt f l  = Def Atom f
                | Expr (GOp f l)
                | Set SetStmt
                deriving Functor

data GOp f l = Valid f
            | Satis f
            | Sequent World f
            | Assume l
            deriving Functor

type SStmt = GStmt (Scheme Atom)  SLogic
type Stmt  = GStmt (Formula Atom) Logic

type SOp = GOp (Scheme Atom)  SLogic
type Op  = GOp (Formula Atom) Logic

instance (Show f, Show l) => Show (GStmt f l) where
  show (Def s f)  = "def "  ++ show s ++ " = " ++ show f
  show (Expr op)  = "eval " ++ show op
  show (Set stmt) = "set "  ++ show stmt

instance (Show f, Show l) => Show (GOp f l) where
  show (Valid f) = "isValid " ++ show f
  show (Satis f) = "isSatisfiable " ++ show f
  show (Sequent w f) = show w ++ " ||- " ++ show f
  show (Assume l) = "assume " ++ show l
{---------------------------------------------------------}

updateDef :: Eq a => DefTable a -> a -> Formula a -> DefTable a
updateDef [] var f = [(var, f)]
updateDef (def@(var',_):defs) var f | var == var' = (var, f) : defs
                                    | otherwise   = def : updateDef defs var f

updateModel :: Model World Atom -> SetStmt -> Model World Atom
updateModel model (Frame frame') = model { frame = frame' }
updateModel model (Tag   tag'  ) = model { tag   = tag'   }

runCmd :: Stmt -> EvalRT (Maybe Eval)
runCmd (Def s f) = do defs <- getDefs
                      setDefs (updateDef defs s f)
                      return Nothing
runCmd (Set s)   = do model <- getModel
                      setModel (updateModel model s)
                      return Nothing
runCmd (Expr op) = do model <- getModel
                      return $ Just (eval op model)

eval :: Op -> Model World Atom -> Eval
eval op = runReader evalFun
  where
    evalFun = case op of
      Valid f     -> M <$> validInModel f
      Satis f     -> M <$> satisfiableInModel f
      Sequent w f -> F <$> w ||- f
      Assume l    -> A <$> modelSatisfiesLogic l
