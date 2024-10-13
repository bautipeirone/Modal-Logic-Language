module Main (main) where


{- Core function to parse and convert a formula
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Reader
import Data.Either

parseF env s = toFormula (fromRight (error "undef var") $ litFormula s)
  where
    litFormula s = runReaderT (scheme s) env
    scheme s = fromRight (error "parse error") $ parseFormula s 1 "stdin"

-}

import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.IO
import           System.Environment
import           Control.Exception              ( catch
                                                , IOException
                                                )

import           Control.Monad.Except

import           Data.List
import           Data.Char

import           Common
import           Core
import           PrettyPrinter
import           Parser

import Control.Monad ( when, unless, foldM,  )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.State (runState)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
---------------------
--- Interpreter
---------------------

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True False "" initialEnv)

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

iname, iprompt :: String
iname = "Modal Logic Language"
iprompt = "MLL> "

data State = S
  { inter   :: Bool    -- True, si estamos en modo interactivo.
  , verbose :: Bool    -- True si el interprete esta en modo verbose
  , lfile   :: String  -- Ultimo archivo cargado (para hacer "reload")
  , env     :: Env     -- Entorno con variables globales y su valor
  }

initialEnv :: Env
initialEnv = (emptyModel, [])

--  read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter v lfile ve) =
  let rec st = do
        mx <- MC.catch
          (if inter then getInputLine iprompt else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        state' <- compileFiles (prelude : args) state
        when inter $ lift $ putStrLn
          (  "Intérprete de "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        --  enter loop
        rec state' { inter = True }

data Command = Compile CompileForm
             | Print String
             | Recompile
             | Browse
             | ToggleVerbose
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if ":" `isPrefixOf` x
  then do
    let (cmd, t') = break isSpace x
        t         = dropWhile isSpace t'
    --  find matching commands
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda."
          )
        return Noop
      [Cmd _ _ f _] -> do
        return (f t)
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ intercalate ", " [ head cs | Cmd cs _ _ _ <- matching ]
          ++ "."
          )
        return Noop
  else return (Compile (CompileInteractive x))

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S inter v lfile ve) cmd = case cmd of
  Quit   -> lift $ unless inter (putStrLn "!@#$^&*") >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Browse -> lift $ do
    putStr (unlines $ reverse (nub (map fst (snd ve))))
    return (Just state)
  Compile c -> do
    state' <- case c of
      CompileInteractive s -> compilePhrase state s
      CompileFile        f -> compileFile (state { lfile = f }) f
    return (Just state')
  Print s   -> printPhrase state s >> return (Just state)
  ToggleVerbose -> let v' = not v in printVerboseMode v' >> return (Just $ state { verbose = v' })
  Recompile -> if null lfile
    then lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    else handleCommand state (Compile (CompileFile lfile))

printVerboseMode :: Bool -> InputT IO ()
printVerboseMode v  = let s = if v then "activado" else "desactivado"
                      in lift $ putStrLn ("El modo verbose esta " ++ s) >> return ()

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  , Cmd [":print"] "<exp>" Print "Imprime un término y sus ASTs sin evaluarlo"
  , Cmd [":reload"]
        "<file>"
        (const Recompile)
        "Volver a cargar el último archivo"
  , Cmd [":verbose"] "" (const ToggleVerbose) "Activa y desactiva el modo verbose"
  , Cmd [":quit"]       "" (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<expr>                  evaluar la expresión\n"
    ++ "def <var> = <expr>      definir una variable\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let ct = intercalate
                   ", "
                   (map (++ if null a then "" else " " ++ a) c)
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { lfile = x, inter = False }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile state@(S inter verbose lfile env) f = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- lift $ Control.Exception.catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
      return ""
    )
  ss <- parseIO f' (parseFile) x
  lift $ case runReaderT ss (snd env) of
    Left s -> putStrLn s >> return state
    Right stmts -> maybe (return state) (foldM handleStmt state) stmts

compilePhrase :: State -> String -> InputT IO State
compilePhrase state x = do
  x' <- parseIO "<interactive>" parseStmt x
  case runReaderT x' (snd env) of
    Left s -> putStrLn s >> return state
    Right stmt -> maybe (return state) (handleStmt state) stmt

printPhrase :: State -> String -> InputT IO ()
printPhrase state x = do
  x' <- parseIO "<interactive>" parseStmt x
  case runReaderT x' (snd env) of
    Left s -> putStrLn s >> return ()
    Right stmt -> maybe (return ()) printStmt stmt
--  where
--   p :: Parser (Either String (Formula Atom))
--   p = do
--     sch <- parseFormula
--     return (runReaderT sch (fst (env state)))


printStmt :: Stmt World Atom -> InputT IO ()
printStmt stmt = lift $ print stmt

parseIO :: String -> Parser a -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p f 1 x of  -- TODO ¿Hace falta totParser?
  Left  e -> print e >> return Nothing
  Right r -> return (Just r)

handleStmt :: State -> Stmt World Atom -> InputT IO State
handleStmt state stmt = lift $ do
  (res,env') <- runState (runCmd stmt) (env state)
  let output = maybe "" (show $ ppEval (verbose state)) res
  when (inter state) $ putStrLn output
  return $ state { env = env' }

prelude :: String
prelude = "examples/Prelude.mll"

it :: String
it = "it"
