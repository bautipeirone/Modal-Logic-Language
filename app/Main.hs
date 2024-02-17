module Main (main) where

import Core
import Parser

main :: IO ()
main = putStr "Resultado: " >> print (parseFormula "p an q" 1 "stdin")

