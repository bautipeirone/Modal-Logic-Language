module Main (main) where

import           System.Console.Haskeline
import           System.IO
import           System.Environment  hiding ( getEnv, setEnv )
import           Control.Exception              ( catch -- For IO exceptions
                                                , IOException
                                                )
import System.Exit ( exitWith, ExitCode(ExitFailure) )


import           Data.List  ( isPrefixOf, intercalate )
import           Data.Char
import           Core
import           PrettyPrinter
import           Parser
import           Elab
import           State

import Control.Monad ( when, (>=>) )
import Control.Monad.Trans
import qualified Control.Monad.Error.Class    as C

---------------------
--- Interpreter
---------------------

iname, iprompt :: String
iname = "Modal Logic Language"
iprompt = "MLL> "

main :: IO ()
main = runOrFail (runInputT defaultSettings main')

main' :: InputT RT ()
main' = do
  args <- liftIO getArgs
  repl args

runOrFail :: RT () -> IO ()
runOrFail m = do
  r <- runRT m
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right () -> return ()

repl :: [String] -> InputT RT ()
repl args = do
        lift $ setInter True
        _ <- lift $ catchErrors $ compileFiles (prelude:args)
        inter <- lift getInter
        when inter $ liftIO $ putStrLn
          (  "Intérprete de " ++ iname ++ ".\n"
          ++ "Escriba :? para recibir ayuda." )
        loop
  where loop = do
          input <- getInputLine iprompt
          case input of
              Nothing -> return ()
              Just "" -> loop
              Just x -> do
                      c    <- lift $ interpretCommand x
                      cont <- lift $ catchErrors $ handleCommand c
                      maybe loop (`when` loop) cont

catchErrors :: RT a -> RT (Maybe a)
catchErrors rt = C.catchError (Just <$> rt) (\e -> printError e >> return Nothing)
  where
    printError :: Error -> RT ()
    printError = liftIO . print

data Command = Compile CompileForm
             | Print String
             | Recompile
             | Browse
             | ToggleVerbose
             | ModalLibrary
             | PrintFrame
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

interpretCommand :: String -> RT Command
interpretCommand x = if ":" `isPrefixOf` x
  then do
    let (cmd, t') = break isSpace x
        t         = dropWhile isSpace t'
    --  find matching commands
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        liftIO $ putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
        return Noop
      [Cmd _ _ f _] -> do
        return (f t)
      _ -> do
        liftIO $ putStrLn
          (  "Comando ambigüo, podría ser "
          ++ intercalate ", " [ head cs | Cmd cs _ _ _ <- matching ]
          ++ "."
          )
        return Noop
  else return (Compile (CompileInteractive x))

handleCommand :: Command -> RT Bool
handleCommand cmd = do
  case cmd of
    Quit   -> return False
    Noop   -> return True
    Help   -> liftIO $ putStr (helpTxt commands) >> return True
    Browse -> printDefs >> return True
    Compile c -> do
      case c of
        CompileInteractive s -> compilePhrase s
        CompileFile        f -> compileFile f
      return True
    Print s   -> printPhrase s >> return True
    PrintFrame -> printModel >> return True
    ToggleVerbose -> do v <- getVerbose
                        printVerboseMode (not v)
                        setVerbose (not v)
                        return True
    ModalLibrary -> liftIO $ putStr modalLibrary >> return True
    Recompile -> do
        lfile <- getLastFile
        if null lfile
          then liftIO $ putStrLn "No hay un archivo cargado.\n" >> return True
          else handleCommand (Compile (CompileFile lfile))

printVerboseMode :: Bool -> RT ()
printVerboseMode v  = let s = if v then "activado" else "desactivado"
                      in liftIO $ putStrLn ("El modo verbose esta " ++ s)

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  , Cmd [":print"] "<exp>" Print "Imprime una formula luego de ser elaborada"
  , Cmd [":modal"] "" (const ModalLibrary)
        "Muestra la libreria de logicas y axiomas modales disponibles"
  , Cmd [":frame"] "" (const PrintFrame)
        "Imprime el frame activo en el momento"
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

modalLibrary :: String
modalLibrary = show ppModalHelp

printModel :: RT ()
printModel = do model <- fst <$> getEnv
                let out = show $ ppFrame model
                liftIO $ putStrLn out

printDefs :: RT ()
printDefs = do
    env <- getEnv
    case snd env of
      [] -> liftIO $ putStrLn "No hay definiciones cargadas por ahora"
      xs -> let nameWidth = maximum (map (length . fst) xs)
            in liftIO $ putStr (unlines $ map (showDef nameWidth) xs)
  where
    showDef nw (n,f) =  let padding = replicate (nw + 1 - (length n)) ' '
                        in n ++ padding ++ ": " ++ (show $ ppFormula f)

compileFiles :: [String] -> RT ()
compileFiles = mapM_ compileFile

compileFile :: String -> RT ()
compileFile f = do
  inter <- getInter
  setInter False
  setLastFile f
  liftIO $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- liftIO $ catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
      return ""
    )
  sstmts <- parseIO f' parseFile x
  mapM_ (elabStmt >=> handleStmt) sstmts
  setInter inter

compilePhrase :: String -> RT ()
compilePhrase x = do
  sstmt <- parseIO "<interactive>" parseStmt x
  stmt  <- elabStmt sstmt
  handleStmt stmt

printPhrase :: String -> RT ()
printPhrase x = do
  scheme <- parseIO "<interactive>" parseFormula x
  f <- elab scheme
  liftIO . print $ ppFormula f

parseIO :: String -> Parser a -> String -> RT a
parseIO f p x = case p x 1 f of
  Left  e -> parseError e
  Right r -> return r

handleStmt :: Stmt -> RT ()
handleStmt stmt = do
  verbose <- getVerbose
  inter <- getInter
  res <- liftEval $ runCmd stmt
  let output = maybe "" (\ev -> show (ppEval verbose ev) ++ "\n") res
  liftIO $ when inter $ putStr output

prelude :: String
prelude = "lib/Prelude.mll"

