Proyecto: Tres en Raya - ncurses (C++)

Descripción:
- Juego de TicTacToe (Tres en Raya) usando ncurses.
- Soporta 0, 1 y 2 jugadores.
- Permite mostrar varios tableros independientes (1-9).
- Controles: teclado y ratón.

Requisitos:
- Linux, g++, make, ncurses library (libncurses-dev)

Compilar:
$ cd code/tictactoe_ncurses
$ make

Ejecutar:
$ ./bin/tictactoe_ncurses

Controles:
- Flechas: mover selección dentro del tablero
- Tab: cambiar entre tableros
- Enter: colocar ficha
- R: reiniciar tablero seleccionado
- Q: salir
- Ratón: clic en celda para seleccionar/colocar (botón izquierdo)

Notas:
- Modo 0: todos los tableros se completan automáticamente con jugadas aleatorias.
- Modo 1: usuario controla X y O manualmente (se respeta alternancia por tablero).
- Modo 2: usuario controla O; X se coloca automáticamente tras cada turno humano si procede.

Estructura de archivos:
- include/: archivos .h
- src/: implementación .cpp
- Makefile
- bin/: ejecutable generado

Ejemplos de uso:
- Cambia número de tableros en Ajustes a 4 y luego Jugar.
- En modo 2, juega como O; la X aparecerá automáticamente si es el turno de X.

Soporte:
- Manejo básico de redimensionado (SIGWINCH, redibuja al cambiar ventana).
- Manejo de errores: el programa intenta restaurar el estado de la terminal en caso de excepción.
