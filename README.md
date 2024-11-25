# Modal Logic Language
Este lenguaje de dominio específico está pensado para llevar a cabo la
evaluación semántica de formulas de la lógica modal, la cual se ubica entre
la lógica de orden proposicional (orden 0) y lógica de predicados (orden 1).

## Uso
Para compilar, solo hace falta
```
stack build
```

Para correr el REPL, una vez compilado el programa
```
stack exec MLL-exe
```

Para cargar un archivo con definiciones, o algún modelo, se lo realiza desde
el REPL con el comando `:load`. Si se desea ver cual es el modelo cargado,
se lo puede ver con `:frame`. Estos y otros comandos de utilidad se pueden
consultar haciendo `:help`.

## Archivos y preludio
Por defecto, la ejecución interactiva del programa lleva a la carga de un archivo
de preludio con definiciones standard.
También se provee de otro archivo que sirve a modo de documentación del lenguaje,
mostrando de forma práctica la sintaxis concreta de este.

## Referencias
Todas las referencias y bibliografía utilizada en el desarrollo del lenguaje
se puede consultar en el informe adjunto.