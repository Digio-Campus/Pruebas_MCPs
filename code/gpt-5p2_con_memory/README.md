# TicTacToe-ncurses (Tres en Raya multi-tablero)

Proyecto en C++17 que implementa **Tres en Raya** con interfaz en terminal usando **ncurses**, con **múltiples tableros independientes** mostrados simultáneamente.

## Requisitos
- Linux
- `g++`
- `make`
- ncurses (paquete típico: `libncurses-dev` o `ncurses-devel`)

## Compilación
```bash
cd code/TicTacToe-ncurses
make
```

## Ejecución
```bash
./bin/tictactoe
```

## Controles (resumen)
### Menús
- Flechas ↑/↓: mover selección
- Enter: aceptar
- Esc o q: volver

### Partida
- Flechas ↑/↓/←/→: mover cursor dentro del tablero actual
- Enter / Espacio: colocar ficha (si el modo lo permite)
- Tab / Shift+Tab: cambiar de tablero
- r: reiniciar tablero actual (mantiene estadísticas)
- R: reiniciar todos los tableros (mantiene estadísticas)
- p: pausar/reanudar auto-juego (solo modo 0 jugadores)
- q: volver al menú principal

## Modos (Ajustes → Jugadores)
- **0 jugadores**: todos los tableros se rellenan automáticamente con jugadas **aleatorias**.
- **1 jugador**: el usuario juega **X y O manualmente** en todos los tableros, respetando el turno alterno **por tablero**.
- **2 jugadores**: el usuario juega **O**; **X** se coloca automáticamente tras cada turno de O (jugada aleatoria).

## Notas
- Cada tablero tiene su **propio turno**, **victorias**, **empates** y **reinicio**.
- Soporta **redimensionado** de la ventana (KEY_RESIZE).
