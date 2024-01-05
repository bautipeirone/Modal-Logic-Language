module Main (main) where

import Core
import Parser

main :: IO ()
main = putStr "Resultado: " >> ((putStrLn . show) $ parseFormula "p an q" 1 "stdin")

