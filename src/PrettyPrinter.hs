module PrettyPrinter
  ( pp
  ) where

-- import Core
import Common
import Prelude hiding ((<>))
-- import Text.PrettyPrint.ANSI.Leijen
-- import Text.PrettyPrint.HughesPJ
import Prettyprinter
import Data.Maybe (fromMaybe)

-- Translation of formula to LaTeX
toLatex :: Formula a -> String
toLatex = undefined

type Color = Doc () -> Doc ()
type Coloring a = [(a, Color)]

resetColor :: Color
resetColor = (<> pretty "\ESC[0m")

createColor :: Bool -> String -> Color
createColor bold hex doc = resetColor (pretty ansiColor <> doc)
      where ansiColor = concat ["\ESC[", boldCode, ";", hex, "m"]
            boldCode = if bold then "1" else "0"

blue, magenta, cyan, red, green, dred, dgreen :: Color
dred    = createColor False "31"
dgreen  = createColor False "32"
red     = createColor True "91"
green   = createColor True "92"
orange  = createColor False "93"
blue    = createColor False "94"
magenta = createColor False "95"
cyan    = createColor False "96"

defaultColors :: [Color]
defaultColors = cycle colors
  where colors = [orange, blue, magenta, cyan, dred, dgreen]


assignColors :: [a] -> Coloring a
assignColors = flip zip defaultColors

pp :: Formula String -> Doc ()
pp f = let col = assignColors (atoms f) in pp' col f 
  where
    pp' :: Coloring String -> Formula String -> Doc ()
    pp' col  Bottom        = red   $ pretty "⊥"
    pp' col  Top           = green $ pretty "⊤"
    pp' col  (Atomic x)    = let c = fromMaybe id (lookup x col) in c $ pretty x
    pp' col  (Not f)       = pretty "¬" <> pp' col f
    pp' col  (Square f)    = pretty "□" <> pp' col f
    pp' col  (Diamond f)   = pretty "◇" <> pp' col f
    pp' col  (And f1 f2)   = pp' col f1 <> (pretty " ∧ ") <> pp' col f2
    pp' col  (Or f1 f2)    = pp' col f1 <> (pretty " ∨ ") <> pp' col f2
    pp' col  (Imply f1 f2) = pp' col f1 <> (pretty " → ") <> pp' col f2
    pp' col  (Iff f1 f2)   = pp' col f1 <> (pretty " ↔ ") <> pp' col f2

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

-- Abstracts the abstract to concrete syntax
-- merge: function to merge representantions
-- fs (from string) transforms from string to representation
-- fa (from atom)   transforms from atom to representation
-- formulaToRepr :: (s -> s -> s) -> (String -> s) -> (a -> s) -> Formula a -> s
-- formulaToRepr merge fs fa Bottom = fs "F"
-- formulaToRepr merge fs fa Top    = fs "T"
-- formulaToRepr merge fs fa (Atomic x) = fa x
-- formulaToRepr merge fs fa (Not p) = (fs "not ") ++ (parensIf (isBinary p) (show p))
-- show (And p q) = (parensIf (cond p) (show p)) ++ " and " ++ (parensIf (cond q) (show q))
--   where
--     cond p = any ($ p) [isImply, isIff]
-- show (Or p q) = (parensIf (cond p) (show p)) ++ " or " ++ (parensIf (cond q) (show q))
--   where
--     cond p = any ($ p) [isImply, isAnd, isIff]
-- show (Imply p q) = (parensIf leftC (show p)) ++ " -> " ++ (parensIf rightC (show q))
--   where
--     leftC  = any ($ p) [isImply, isIff]
--     rightC = any ($ p) [isAnd, isOr, isIff]
-- show (Iff p q) = show p ++ " <-> " ++ show q
-- show (Square p)  = "[]" ++ (parensIf (isBinary p) (show p))
-- show (Diamond p) = "<>" ++ (parensIf (isBinary p) (show p))

-- La instancia devuelve la representancion lo mas simplificada posible
-- Es decir, remueve todos los parentesis redundantes que sean posibles
-- instance Show a => Show (Formula a) where

isTop :: Formula a -> Bool
isTop Top = True
isTop _   = False

isBottom :: Formula a -> Bool
isBottom Bottom = True
isBottom _      = False

isAtomic :: Formula a -> Bool
isAtomic Atomic{} = True
isAtomic _        = False

isNot :: Formula a -> Bool
isNot Not{} = True
isNot _     = False

isSquare :: Formula a -> Bool
isSquare Square{} = True
isSquare _        = False

isDiamond :: Formula a -> Bool
isDiamond Diamond{} = True
isDiamond _         = False

isAnd :: Formula a -> Bool
isAnd And{} = True
isAnd _     = False

isOr :: Formula a -> Bool
isOr Or{} = True
isOr _    = False

isImply :: Formula a -> Bool
isImply Imply{} = True
isImply _       = False

isIff :: Formula a -> Bool
isIff Iff{} = True
isIff _     = False

isConstant :: Formula a -> Bool
isConstant f = any ($ f) [isTop, isBottom, isAtomic]

isUnary :: Formula a -> Bool
isUnary f = any ($ f) [isNot, isSquare, isDiamond]

isBinary :: Formula a -> Bool
isBinary f = any ($ f) [isAnd, isOr, isImply, isIff]
