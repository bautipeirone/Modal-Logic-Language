module PrettyPrinter
  ( ppFormula
  , ppTrace
  , ppModelTrace
  , ppEval
  , ppModalHelp
  , ppFrame
  ) where

import Common
import Core (Eval (..))
import Axioms
import Modal
import Frame

import Prelude hiding ((<>))
import Prettyprinter
import Data.Maybe (fromMaybe)
import Data.Bifunctor
import Data.Map (assocs)
import Data.Set (elems)

type Color = Doc () -> Doc ()
type Coloring a = [(a, Color)]

{---------------- Utilidades -----------------}
padWordLeft :: Int -> String -> String
padWordLeft w s = let l = length s
                      pad = replicate (w - l) ' '
                  in  pad ++ s

resetColor :: Color
resetColor = (<> pretty "\ESC[0m")

createColor :: Bool -> String -> Color
createColor bold hex doc = resetColor (pretty ansiColor <> doc)
      where ansiColor = concat ["\ESC[", boldCode, ";", hex, "m"]
            boldCode = if bold then "1" else "0"

red, green, orange, blue, magenta, cyan :: Color
red     = createColor True "91"
green   = createColor True "92"
orange  = createColor True "93"
blue    = createColor True "94"
magenta = createColor True "95"
cyan    = createColor True "96"

defaultColors :: [Color]
defaultColors = cycle colors
  where colors = [orange, blue, magenta, cyan]

assignColors :: [a] -> Coloring a
assignColors = flip zip defaultColors
{-------------------------------------------- -}

ppFormula :: Formula Atom -> Doc ()
ppFormula f = let col = assignColors (atoms f) in pp col f

pp :: Coloring Atom -> Formula Atom -> Doc ()
pp = pp'
  where
    pp' _    Bottom         = red   $ pretty "⊥"
    pp' _    Top            = green $ pretty "⊤"
    pp' col  (Atomic x)     = let c = fromMaybe id (lookup x col) in c (pretty x)
    pp' col  (Not f1)       = ppUnary col (pretty "¬") f1
    pp' col  (Square f1)    = ppUnary col (pretty "□") f1
    pp' col  (Diamond f1)   = ppUnary col (pretty "◇") f1
    pp' col  (Or f1 f2)     = let predicate f = isBinary f && not (isOr f)
                              in ppBinary col predicate (pretty " ∨ ") f1 f2
    pp' col  (And f1 f2)    = let predicate f = isImply f || isIff f
                              in ppBinary col predicate (pretty " ∧ ") f1 f2
    pp' col  (Imply f1 f2)  = let p1 = parensIf (isImply f1 || isIff f1)
                                  p2 = parensIf (isIff f2)
                              in  p1 (pp' col f1) <> pretty " -> " <> p2 (pp' col f2)
    pp' col  (Iff f1 f2)    = ppBinary col isIff (pretty " <-> ") f1 f2
    ppUnary col sym f = let p = parensIf (isBinary f)
                        in sym <> p (pp' col f)
    ppBinary col predicate sym f1 f2 =  let p1 = parensIf (predicate f1)
                                            p2 = parensIf (predicate f2)
                                        in p1 (pp' col f1) <> sym <> p2 (pp' col f2)
-- Sea * un operador unario, y + un operador binario
-- (* (+ x y)) se escribe como * (x + y)
-- Un operador or pone parentesis si su argumento es un operador binario excepto para el or.
-- Indistinto para argumento izquierdo o derecho
-- Un operador and pone parentesis si su argumento es el operador -> o <->.
-- Indistinto para argumento izquierdo o derecho
-- El operador -> asocia a derecha, y por ende lleva parentesis en su argumento
-- izquierdo cuando este es un operador -> o <->. A su argumento derecho solo
-- le pone parentesis cuando es un <->.
-- El operador <-> nunca pone parentesis a sus argumentos ya que es el de menor
-- precedencia.
-- Referir a http://intrologic.stanford.edu/dictionary/operator_precedence.html

{------------ Trace Pretty Printing ------------}
ppEval :: Bool -> Eval -> Doc ()
ppEval False eval = ppBool $ case eval of
                          F t -> evalTrace  t
                          M t -> evalModel  t
                          A t -> evalAxioms t
ppEval True eval = case eval of
                  F t -> ppTrace       t
                  M t -> ppModelTrace  t
                  A t -> ppAxiomsTrace t


ppTrace :: Trace -> Doc ()
ppTrace tr = let col = assignColors (atoms (getTraceHead tr)) in ppTrace' col tr
  where
    ppTrace' :: Coloring Atom -> Trace -> Doc ()
    ppTrace' col t = let backtrace = case getSubtrace t of
                                       Left ts -> concatSubForms (ppTrace' col) ts
                                       Right wts -> concatWorldSteps (ppTrace' col) wts
                         result = ppFormulaEval col (getTraceHead t) (evalTrace t)
                     in backtrace <> result

ppModalHelp :: Doc ()
ppModalHelp = vsep [ pretty "Los axiomas son: "
                   , vsep (map ppAxiomMsg modalAxioms) <> line
                   , pretty "Las logicas disponibles son: "
                   , vsep (map ppLogicMsg modalLogics) <> line
                   ]
  where
    ppAxiomMsg :: Axiom -> Doc ()
    ppAxiomMsg ax = pretty (padWordLeft 4 (axiomName ax))
                 <> pretty " <=> "
                 <> ppFormula (axiomFormula ax)

    ppLogicMsg :: Logic -> Doc ()
    ppLogicMsg lg = pretty (padWordLeft 4 (logicName lg))
                 <> pretty "  =  "
                 <> pretty (map axiomName (logicAxioms lg))
                --  <> line
                 <> pretty "  |  "
                 <> pretty (logicDescription lg)

ppFrame :: Model World Atom -> Doc ()
ppFrame m = vsep [ pretty "Grafo de transiciones"
                 , ppGraph (frame m) <> line
                 , pretty "Etiquetado de estados"
                 , ppTag (tag m) <> line
                 ]
    where
      ppGraph g = vsep $ map (\(v,ns) -> pretty v <> pretty " -> " <> encloseSep lbrace rbrace comma (map pretty ns)) (assocs (edges g))
      ppTag t = vsep $ map (\(v,as) -> pretty v <> pretty " -> " <> encloseSep lbrace rbrace comma (map pretty (elems as))) (assocs t)

ppModelTrace :: ModelTrace -> Doc ()
ppModelTrace mt = let backtrace = concatWorldSteps ppTrace (getWorldTraces mt)
                      result = ppFormulaEval (assignColors $ atoms $ getFormula mt) (getFormula mt) (evalModel mt)
                  in backtrace <> result

ppAxiomsTrace :: AxiomsTrace -> Doc ()
ppAxiomsTrace at = vsep $ map (ppAxiomsCheck colWidth) (getAxioms at)
        where
          colWidth = maximum $ map (length . axiomName . fst) (getAxioms at)
          ppCheck :: Bool -> Doc ()
          ppCheck  True = green $ pretty "✓"
          ppCheck False = red   $ pretty "✗"
          ppAxiomsCheck :: Int -> (Axiom, Bool) -> Doc ()
          ppAxiomsCheck w (ax,b) = hsep [ pretty $ padWordLeft w (axiomName ax)
                                        , space
                                        , ppCheck b
                                        ]

ppFormulaEval :: Coloring Atom -> Formula Atom -> Bool -> Doc ()
ppFormulaEval col f b = hsep [ pp col f
                      , colon
                      , space
                      , ppBool b
                      ]

ppBool :: Bool -> Doc ()
ppBool True  = green $ pretty True
ppBool False = red   $ pretty False

concatSubForms :: (Trace -> Doc ()) -> [Trace] -> Doc ()
concatSubForms printer ts = let docs = fmap printer ts
                                backtrace = if null docs then emptyDoc
                                                         else indent 2 (vsep docs) <> line
                            in backtrace

lineSep:: Doc ()
lineSep = pretty "--------------------------"

encloseSubtrace :: (World, Doc ()) -> Doc ()
encloseSubtrace (w, doc) = vsep [ (pretty w) <> colon
                                , doc
                                , lineSep
                                ]

concatWorldSteps :: (Trace -> Doc ()) -> [(World, Trace)] -> Doc ()
concatWorldSteps printer wts = let docs = fmap (second ((indent 2) . printer)) wts
                                   docs' = fmap encloseSubtrace docs
                                   backtrace = if null docs then emptyDoc
                                                            else (vsep docs') <> line
                               in backtrace

{------------------ Utilities ------------------}
parensIf :: Bool -> Doc () -> Doc ()
parensIf True  = parens
parensIf False = id

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

isBinary :: Formula a -> Bool
isBinary f = any ($ f) [isAnd, isOr, isImply, isIff]
