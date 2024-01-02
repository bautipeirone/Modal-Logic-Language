-- Logica Modal
use modal -- Importa modulo con definiciones

set world W = [x1, x2, x3, x4] -- Especifica conjunto de mundos a usar

set relation R as              -- Indica relacion de transicion entre mundos
  x1 -> x2
  x1 -> x3
  x1 -> x4
  x2 -> x2
  x3 -> x1
  x4 -> x1, x2
end

set tag L as                  -- Indica
  x1 -> {}
  x2 -> {}
  x3 -> {p}
  x4 -> {p,q}
end

set model M = (W, R, L)

def phi = p and q
def psi = not p
