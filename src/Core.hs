module Core
  ( Stmt (..)
  , SetStmt (..)
  , Op (..)
  , Eval (..)
  , runCmd
  , Env
  , module Modal
  , module Axioms
  ) where

-- import PrettyPrinter
import Common
import Frame
import Modal
import Axioms

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

data SetStmt w a  = Frame (Graph w)
                  | Tag (TagMapping w a)
                  deriving Show

data Stmt w a = Def a (Formula a)
              | Expr (Op w a)
              | Set (SetStmt w a)

data Op w a = Valid (Formula a)
            | Satis (Formula a)
            | Sequent w (Formula a)
            | Assume Logic


-- class Trace a where
--   getTraceHead :: a w -> w
--   eval :: a -> Bool
--   getSubtraces :: Foldable t => a w -> t (a w)

-- Abstracts evaluation traces
data Eval = F Trace | M ModelTrace | A AxiomsTrace

{------- Show Instances -------}
instance (Show w, Show a) => Show (Stmt w a) where
  show (Def s f)  = "def "  ++ show s ++ " = " ++ show f
  show (Expr op)  = "eval " ++ show op
  show (Set stmt) = "set "  ++ show stmt

instance (Show w, Show a) => Show (Op w a) where
  show (Valid f) = "isValid " ++ show f
  show (Satis f) = "isSatisfiable " ++ show f
  show (Sequent w f) = show w ++ " ||- " ++ show f
  show (Assume axs) = "assume " ++ show axs
{------------------------------}
type Env = (Model World Atom, DefTable Atom)

type M a = State Env a

getModel :: M (Model World Atom)
getModel = gets fst

getDefs :: M (DefTable Atom)
getDefs = gets snd

updateDef :: Eq a => DefTable a -> a -> Formula a -> DefTable a
updateDef [] var f = [(var, f)]
updateDef (def@(var',_):defs) var f | var == var' = (var, f) : defs
                                    | otherwise   = def : updateDef defs var f

updateModel :: Model w a -> SetStmt w a -> Model w a
updateModel model (Frame frame') = model { frame = frame' }
updateModel model (Tag   tag'  ) = model { tag   = tag'   }

runCmd :: Stmt World Atom -> M (Maybe Eval)
runCmd (Def s f) = do (model, vars) <- get
                      put (model, updateDef vars s f)
                      return Nothing
runCmd (Set s)   = do (model, vars) <- get
                      put (updateModel model s, vars)
                      return Nothing
runCmd (Expr op) = do model <- getModel
                      return $ Just (eval op model)

eval :: Op World Atom -> Model World Atom -> Eval
eval op = runReader evalFun
  where
    evalFun = case op of
      Valid f     -> M <$> validInModel f
      Satis f     -> M <$> satisfiableInModel f
      Sequent w f -> F <$> w ||- f
      Assume l    -> A <$> modelSatisfiesLogic l
