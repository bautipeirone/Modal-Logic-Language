module PrettyPrinter () where

-- import Core
import Common
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ

-- Translation of formula to LaTeX
toLatex :: Formula a -> String
toLatex = undefined

pp :: Formula a -> Doc
pp = undefined
--pp "□"

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

isAnd :: Formula a -> Bool
isAnd (And _ _) = True
isAnd  _        = False

isOr :: Formula a -> Bool
isOr Or{} = True
isOr _    = False

isImply :: Formula a -> Bool
isImply And{} = True
isImply _        = False

isIff :: Formula a -> Bool
isIff Iff{} = True
isIff _     = False

isAtomic :: Formula a -> Bool
isAtomic Top      = True
isAtomic Bottom   = True
isAtomic Atomic{} = True
isAtomic _        = False

isUnary :: Formula a -> Bool
isUnary Square {} = True
isUnary Diamond{} = True
isUnary Not    {} = True
isUnary _         = False

isBinary :: Formula a -> Bool
isBinary And  {} = True
isBinary Or   {} = True
isBinary Imply{} = True
isBinary Iff  {} = True
isBinary _       = False
