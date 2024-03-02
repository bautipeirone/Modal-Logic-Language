module Core ( Stmt (..)
            , SetStmt (..)
            , Result
            , updateDef
            , Op (..)
            ) where

import Modal

-- import PrettyPrinter
import Common
import Frame
import Control.Monad.State.Lazy
import Control.Monad.Except
-- import Data.Bifunctor

type Result a = Either String a

data SetStmt w a  = Frame (Graph w)
                  | Tag (TagMapping w a)

data Stmt w a = Def String (Formula a)
              | Expr (Op w a)
              | Set (SetStmt w a)

data Op w a = Valid (Formula a)
            | Satis (Formula a)
            | Sequent w (Formula a)
            deriving Show

type M a = StateT (Env String) (Except String) a

updateDef :: Eq a => DefTable a -> a -> Formula a -> DefTable a
updateDef [] var f = [(var, f)]
updateDef (def@(var',_):defs) var f | var == var' = (var, f) : defs
                                    | otherwise   = def : updateDef defs var f

updateModel :: Model w a -> SetStmt w a -> Model w a
updateModel model (Frame frame') = Model { frame = frame'     , tag = tag model}
updateModel model (Tag   tag'  ) = Model { frame = frame model, tag = tag'     }

runCmd :: Stmt String String -> M (Maybe Bool)
runCmd (Def s f) = do (model, vars) <- get
                      put (model, updateDef vars s f)
                      return Nothing
runCmd (Set s)   = do (model, vars) <- get
                      put (updateModel model s, vars)
                      return Nothing
runCmd (Expr op) = do env <- get
                      return Nothing
                      -- case eval op env of
                      --   Right result -> return $ Just result
                      --   Left  err    -> return $ Left err

eval :: Op String String -> Env a -> Except String Bool
eval = undefined
-- eval (Valid f) = runReader (validInModel f)
-- eval (Satis f) = runReader (satisfiableInModel f)
-- eval (Sequent w f) = runReader ()

-- instance Bifunctor SetStmt where
-- bimap f g ()
