TicTacToe ncurses — Multi-tablero

Compilación y ejecución:

  cd code/tictactoe-ncurses
  make
  make run

Controles (modo juego):
  - Flechas / WASD: mover cursor dentro del tablero seleccionado
  - ENTER: colocar ficha
  - TAB: cambiar tablero seleccionado
  - R: reiniciar tablero seleccionado
  - Q / ESC: volver al menú / salir
  - KEY_RESIZE: redimensiona y ajusta layout

Ajustes:
  - NumPlayers: 0 (autómático), 1 (un jugador controla X y O), 2 (jugador controla O, X es automática)
  - NumBoards: 1..9 (tableros independientes)

Tests:
  make test

Requisitos:
  - g++
  - ncurses

Descripción:
  Proyecto modular en C++17 (compatible con C++11) que implementa Tres en Raya con ncurses.
