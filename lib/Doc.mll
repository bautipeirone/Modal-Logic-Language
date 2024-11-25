-- Logica Modal

set frame = {              -- Indica relacion de transicion entre mundos
  x1 -> {x2,x3,x4},
  x1 -> {x5},              -- Hacer esto agrega nuevas aristas, no sobreescribe
  x2 -> {x2},
  x3 -> {x1},
  x4 -> {x1, x2, x5},
  x6 -> {}
} -- Los vertices se derivan de forma automatica, x1 ... x6

set tag = {                  -- Indica asosiacion de valores de verdad de
  x1 -> {},                  -- formulas atomicas segun el mundo
  x2 -> {},
  x3 -> {p},
  x4 -> {p,q}
}

def Phi = []p <-> <>q  -- Formulas y/o esquemas. Las formulas pueden ser evaluadas
                       -- o usadas a su vez como esquemas de formulas, es decir,
                       -- abstraer una estructura sintactica y luego ser utilizada
                       -- mediante reemplazos
-- Hay dos comentarios al respecto de esto
-- El primero es que el espacio de nombres de definiciones y de atomos es disjunto,
-- las definiciones deben comenzar con mayuscula y los atomos con minuscula. Esto es
-- para evitar ambiguedades a la hora de evaluarlas.
-- Lo segundo a notar es que para utilizar estas definiciones como esquemas, se
-- debe conocer el nombre de los atomos para llevar a cabo las sustituciones con exito.
-- Por esto, es conveniente usar una convencion, y la que adopto en el preludio es
-- la de nombrarlos p1, p2, ..., en el orden en que aparecen.


isValid Phi[r and s/p]     -- Reemplazo sintactico, equivalente a "r and s <-> q"

-- r and q  -- Dos tipos de escritura de proposiciones posible
-- r &&  q  -- Estilo clasico (&&, ||, ~) o estilo literal (and, or, not)
-- r and q && t -- Esto es posible, pero no recomendado

assume Logic-T  -- Verifica si la logica modal T vale sobre el modelo definido
assume {Axiom-K, Axiom-T} -- Tambien puede escribirse como un conjunto de axiomas
-- Las logicas y axiomas disponibles son built-ins del lenguaje y no se pueden
-- agregar nuevos ya que la verificacion de axiomas requiere verificar propiedades
-- especiales sobre el grafo del modelo.
