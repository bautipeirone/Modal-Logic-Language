{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}

module State
  ( State
  , RT
  , EvalRT
  , Error
  , runRT
  , runtimeError
  , parseError
  , setInter
  , setLastFile
  , setVerbose
  , setEnv
  , getInter
  , getLastFile
  , getVerbose
  , getEnv
  , getModel
  , getDefs
  , setModel
  , setDefs
  , liftEval
) where

import Control.Monad.Trans
import Control.Monad.State  hiding (State, state)
import Control.Monad.Except
import Data.Bifunctor

import Common
import Modal ( Model, emptyModel )

data State = S
  { inter   :: Bool    -- True, si estamos en modo interactivo.
  , verbose :: Bool    -- True si el interprete esta en modo verbose
  , lfile   :: String  -- Ultimo archivo cargado (para hacer "reload")
  , env     :: Env     -- Entorno con variables globales y su valor
  }

type Env = (Model World Atom, [(Atom, Formula Atom)])

data Error = ParseError String | RuntimeError String

instance Show Error where
  show (ParseError s)   = "[PARSE ERROR]: "   ++ s ++ "."
  show (RuntimeError s) = "[RUNTIME ERROR]: " ++ s ++ "."

class (MonadState State m, MonadError Error  m, MonadIO m) => MonadRuntime m where

type RT     = StateT State (ExceptT Error IO)       -- Runtime del interprete
type EvalRT = StateT Env   (Except Error)           -- Runtime del evaluador

instance MonadRuntime RT

{------ Interfaz de clase MonadRuntime ------}
runtimeError :: MonadRuntime m => String -> m a
runtimeError = throwError . RuntimeError

parseError :: MonadRuntime m => String -> m a
parseError = throwError . ParseError

setInter :: MonadRuntime m => Bool -> m ()
setInter b = modify (\s -> s { inter = b })

setLastFile :: MonadRuntime m => String -> m ()
setLastFile f = modify (\s -> s { lfile = f })

setVerbose :: MonadRuntime m => Bool -> m ()
setVerbose b = modify (\s -> s { verbose = b })

setEnv :: MonadRuntime m => Env -> m () -- No deberia ir
setEnv e = modify (\s -> s { env = e })

getInter :: MonadRuntime m => m Bool
getInter = gets inter

getLastFile :: MonadRuntime m => m String
getLastFile = gets lfile

getVerbose :: MonadRuntime m => m Bool
getVerbose = gets verbose

getEnv :: MonadRuntime m => m Env
getEnv = gets env
{--------------------------------------------}

{------- Interfaz de clase MonadElab --------}
getModel :: EvalRT (Model World Atom)
getModel = gets fst

getDefs :: EvalRT (DefTable Atom)
getDefs = gets snd

setModel :: Model World Atom -> EvalRT ()
setModel model = modify (first $ const model)

setDefs :: DefTable Atom -> EvalRT ()
setDefs defs = modify (second $ const defs)
{--------------------------------------------}

liftEval :: EvalRT a -> RT a
liftEval eval = do  state <- get
                    let res = runExcept $ runStateT eval (env state)
                    case res of
                      Left e -> throwError e
                      Right (eval', env') -> do
                          setEnv env'
                          return eval'

-- liftElab :: ElabRT a -> RT a
-- liftElab elab = undefined

runRT :: RT a -> IO (Either Error a)
runRT rt = runExceptT $ evalStateT rt initialState
  where
    initialState = S True False "" (emptyModel, [])
