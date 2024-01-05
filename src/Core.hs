module Core ( Stmt (..)
            , SetStmt (..)
            ) where

import Modal
-- import Data.Bifunctor

data SetStmt w a  = Worlds [w]
                  | Transition [(w, [w])]
                  | Tag [(w, [a])]
                  deriving Show

data Stmt w a = Def String (Formula a)
              | Expr (Formula a)
              | Set (SetStmt w a)
            deriving Show

-- instance Bifunctor SetStmt where
-- bimap f g ()
