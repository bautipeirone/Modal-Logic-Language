# MLL - DSL ALP
## Decisiones de diseño
* Uso de distintos espacios de nombres para identificadores y atomos.
  Los identificadores comienzan con mayuscula y los atomos con minuscula. La
  justificacion detras de esto es poder evitar sentencias confusas como `def p = r`, luego
  de la cual `p` queda registrado como una definicion global; y un operador `undef`
  no me parecia conveniente en el lenguaje. Por lo tanto, para salir de ambiguedades,
  las formulas globales (o lo que llamé esquemas) comienzan con mayuscula. Esto
  tambien me permite detectar errores de variables no definidas. Previo a este cambio,
  un identificador se buscaba como esquema, y si no existia es que se lo consideraba
  como atomo, por lo tanto nunca se podia afirmar que un identificador no estaba
  definido, sino que simplemente se pensaba como una proposicion atomica mas.
* El metodo para definir grafos en un principio contaba con una sentencia que
  definia sus vertices. Encontre que esto era impractico, y es por eso que
  el algoritmo de construccion del grafo deduce de forma automatica los vertices
  que participan en el grafo.
* Con el fin de hacer un lenguaje mas practico y usable pensé en soportar
  varias notaciones para los operadores logicos.