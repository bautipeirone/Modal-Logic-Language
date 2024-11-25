-- Esquemas de los axiomas modales standard
def AK = [](p1 -> p2) -> ([]p1 -> []p2)
def AT = []p1 -> p1
def AB = p1 -> []<>p1
def AD = []p1 -> <>p1
def A4 = []p1 -> [][]p1
def A5 = <>p1 -> []<>p1
def AE = []p1 <-> <>p1
def AC = [](p1 && []p1 -> p2) || [](p2 && []p2 -> p1)

-- Esquemas de las logicas como conjunciones de sus axiomas
def    L4 = AK
def    LT = AK and AT
def   LS4 = LT and A4
def LKT45 = LS4 and A5
def   LS5 = LKT45 and AB
def    LD = AK and AD

-- Esquemas usuales en deduccion natural
def TND = p1 or ~p1
def  MT = ~p2 -> ~p1

-- Modelo basico, este modelo satisface todas las logicas
set frame = {
  x -> {x}
}

set tag = {
  x -> {}
}