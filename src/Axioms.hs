{-# LANGUAGE TupleSections #-}

module Axioms
  ( SLogic (..)
  , Logic (..)
  , AxiomsTrace (..)
  , Axiom (..)
  , identToLogic
  , listToLogic
  , modelSatisfiesLogic
  , modalAxioms
  , modalLogics
  )
where

import Common ( Formula (..), Atom, World )
import qualified Frame as F
import Modal ( Model (..), EvalM )
import Control.Monad.Reader (asks)

data Axiom = Axiom { axiomName :: String
                   , axiomFormula :: Formula Atom
                   , graphProperty :: F.GraphProperty World
                   }


data AxiomsTrace = AxiomsTrace
                    { getAxioms :: [(Axiom, Bool)]
                    , evalAxioms :: Bool
                    -- , getLogic :: Logic
                    }


data SLogic = LogicIdent String | AxiomsList [String]

data Logic = Logic { logicName :: String
                   , logicDescription :: String
                   , logicAxioms :: [Axiom]
                   }

axK, axT, axB, axD, ax4, ax5 :: Axiom
axK = Axiom "K"
            ( Imply ( Square (Imply p q) )
                    ( Imply (Square p) (Square q) )
            )
            (const True)
    where p = Atomic "p"
          q = Atomic "q"
axT = Axiom "T"
            ( Imply (Square p) p )
            F.isReflexive
    where p = Atomic "p"
axB = Axiom "B"
            ( Imply p (Square (Diamond p)) )
            F.isSymmetric
    where p = Atomic "p"
axD = Axiom "D"
            ( Imply (Square p) (Diamond p) )
            F.isSerial
    where p = Atomic "p"
ax4 = Axiom "4"
            ( Imply (Square p) (Square (Square p)) )
            F.isTransitive
    where p = Atomic "p"
ax5 = Axiom "5"
            ( Imply (Diamond p) (Square (Diamond p)) )
            F.isEuclidean
    where p = Atomic "p"

-- Check if these are well known. I invented these names
axE, axC :: Axiom
axE = Axiom "E"
            ( Iff (Square p) (Diamond p) )
            F.isFunctional
    where p = Atomic "p"
axC = Axiom "C"
            ( Or ( Square (Imply (And p (Square p)) q) )
                 ( Square (Imply (And q (Square q)) p) ) )
            F.isLinear
    where p = Atomic "p"
          q = Atomic "q"

modalAxioms :: [Axiom]
modalAxioms = [ axK, axT, axB, axD, ax4, ax5, axE, axC ]

-- Standard modal logics supported
modalLogics :: [Logic]
modalLogics = [ Logic "K"  "TODO"   [ axK ]
              , Logic "T"  "cambiar esto" [ axK , axT ]
              , Logic "S4" "agregar descripciones" [ axK , axT , ax4 ]
              , Logic "S5" "                 " [ axK , axT , ax5 ]
              , Logic "D"  ""                  [ axK , axD ]
              ]

identToLogic :: String -> Either String Logic
identToLogic s = maybe (Left $ "Unknown logic name: " ++ s) Right (lookup s namedLogics)
        where namedLogics = fmap (\l -> (logicName l, l)) modalLogics

listToLogic :: [String] -> Either String Logic
listToLogic ss = newUnnamedLogic <$> (mapM (`findAxiom` namedAxioms) ss)
        where
          newUnnamedLogic :: [Axiom] -> Logic
          newUnnamedLogic = Logic "Unnamed Logic" "Custom Logic instantiated via axioms list"
          namedAxioms = fmap (\ax -> (axiomName ax, ax)) modalAxioms
          findAxiom ax axs = maybe (Left $ "Unknown axiom name: " ++ ax)
                                    Right (lookup ax axs)


frameSatisfiesAxiom :: F.Graph World -> Axiom -> Bool
frameSatisfiesAxiom = flip graphProperty

modelSatisfiesLogic :: Logic -> EvalM AxiomsTrace
modelSatisfiesLogic l = do m <- asks frame
                           let bs = map (frameSatisfiesAxiom m) axs
                               ns = zip axs bs
                               b  = and bs
                           return AxiomsTrace {getAxioms=ns, evalAxioms=b}
                      where
                        axs = logicAxioms l


