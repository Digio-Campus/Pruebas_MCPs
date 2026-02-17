Tres en Raya (ncurses)
======================

Proyecto C++ que implementa el juego "Tres en Raya" (Tic-Tac-Toe) usando `ncurses`.

Características principales:
- Interfaz completa en terminal con `ncurses` (teclado + ratón).
- Menú principal: `Jugar`, `Ajustes`, `Ayuda`, `Salir`.
- Modos de juego configurables: 0 (auto aleatorio), 1 (un jugador controla X y O), 2 (jugador controla O; X generado automáticamente).
- Soporta múltiples tableros independientes y redimensionado de ventana.
- Estadísticas por tablero, reinicio individual y total.

Compilación
-----------
Instalar dependencias (Debian/Ubuntu):

    sudo apt-get install build-essential libncurses5-dev libncursesw5-dev

Compilar:

    make -C code/tictactoe_ncurses

Ejecutar:

    ./code/tictactoe_ncurses/bin/tictactoe_ncurses

Controles (resumen)
-------------------
- En menús: flechas / ratón / Enter
- En el juego:
  - Flechas: mover cursor dentro del tablero seleccionado
  - Enter / Espacio: colocar marca (según turno y modo)
  - Tab / ← → : cambiar tablero seleccionado
  - r: reiniciar tablero actual
  - R: reiniciar todos los tableros
  - h: mostrar ayuda
  - q: volver al menú / salir

Estructura del proyecto
----------------------
- `include/` - archivos .h
- `src/` - implementación .cpp
- `Makefile` - compilación

