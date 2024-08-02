module Axioms () where

import Common ( Formula, Atom )
import Frame ( GraphProperty )

data Axiom = Axiom { axiomName :: String
                   , axiomFormula :: Formula Atom
                   , graphProperty :: GraphProperty World
                   }

instance Show Axiom where
  show ax = let name = axiomName ax
                formula = axiomFormula ax
                in ax ++ " <=> " ++ show formula

type Logic = [Axiom]

axK, axT, axB, axD, ax4, ax5 :: Axiom
axK = Axiom { "K"
            , Imply ( Square (Imply p q)
                      Imply (Square p) (Square q) )
            , const True
            }
    where p = Atomic "p"
          q = Atomic "q"
axT = Axiom { "T"
            , Imply (Square p) p
            , isReflexive
            }
    where p = Atomic "p"
axB = Axiom { "B"
            , Imply p (Square (Diamond p))
            , isSymmetric
            }
    where p = Atomic "p"
axD = Axiom { "D"
            , Imply (Square p) (Diamond p)
            , isSerial
            }
    where p = Atomic "p"
ax4 = Axiom { "4"
            , Imply (Square p) (Square (Square p))
            , isTransitive
            }
    where p = Atomic "p"
ax5 = Axiom { "5"
            , Imply (Diamond p) (Square (Diamond p))
            , isEuclidean
            }
    where p = Atomic "p"

-- Check if these are well known. I invented these names
axE, axC :: Axiom
axE = Axiom { "E"
            , Iff (Square p) (Diamond p)
            , isFunctional
            }
    where p = Atomic "p"
axC = Axiom { "C"
            , Or ( Square (Imply (And p (Square p)) q)
                   Square (Imply (And q (Square q)) p) )
            , isLinear
    where p = Atomic "p"
          q = Atomic "q"

-- Standard modal logics
k, t, s4, s5, d :: Logic
k   = [ axK ]
t   = k ++ [ axT ]
s4  = t ++ [ ax4 ]
s5  = t ++ [ ax5 ]
d   = k ++ [ axD ]

frameSatisfiesAxiom :: Graph World -> Axiom -> Bool
frameSatisfiesAxiom f ax = (graphProperty ax) f

modelSatisfiesLogic :: Model -> Logic -> Bool
modelSatisfiesLogic m axs = all (frameSatisfiesAxiom (frame m)) axs

