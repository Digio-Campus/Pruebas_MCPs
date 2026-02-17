TicTacToe - ncurses
===================

Descripción
- Juego Tres en Raya (TicTacToe) multiplataforma en terminal usando ncurses.
- Soporta múltiples tableros independientes (1,2,4,6,9), modos 0/1/2, y redimensionado.

Compilación y ejecución

  cd code/tictactoe-ncurses
  make
  make run

Tests

  make test

Controles
- Flechas: mover cursor en tablero
- Tab: cambiar tablero seleccionado
- Enter: colocar ficha (se respeta el turno dentro de cada tablero)
- R: reiniciar tablero actual
- Q: volver al menú principal

Modos
- 0: ambos jugadores automáticos (llenado aleatorio)
- 1: un jugador controla X y O (manual, alternancia estricta por tablero)
- 2: jugador controla O; X se genera automáticamente tras cada turno

Estructura
- include/: cabeceras de `Board`, `UI`, `Menu`, `Settings`, `Game`
- src/: implementación C++
- tests/: test unitarios para `Board`
- Makefile: targets `all`, `run`, `test`, `clean`
