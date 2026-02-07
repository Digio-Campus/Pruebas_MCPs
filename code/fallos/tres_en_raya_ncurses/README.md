# Tres en Raya (ncurses) — Multi‑tablero

Proyecto en C++ que implementa el juego **Tres en Raya** con interfaz en terminal usando **ncurses**, con **múltiples tableros independientes** en pantalla, menú principal y ajustes.

## Requisitos

- Linux
- `g++` (C++17)
- `ncurses` (dev). En Debian/Ubuntu: `sudo apt-get install libncurses5-dev` (o `libncurses-dev`).

## Compilación

Desde esta carpeta:

```bash
make
```

## Ejecución

```bash
make run
# o
./bin/tres_en_raya_ncurses
```

## Controles (resumen)

- Menús: **↑/↓** para navegar, **Enter** para seleccionar, **Esc** para volver.
- En partida:
  - Mover cursor de casilla: **↑/↓/←/→**
  - Colocar ficha: **Enter** o **Espacio**
  - Cambiar de tablero: **Tab** (siguiente), **Shift+Tab** (anterior), o **[ / ]**
  - Reiniciar tablero actual: **r**
  - Reiniciar todos: **R**
  - Volver al menú: **q**

## Modos de jugadores

- **0 jugadores**: todos los tableros se juegan solos con movimientos aleatorios (X y O) y se reinician automáticamente al terminar.
- **1 jugador**: el usuario juega **X y O manualmente** en cada tablero, manteniendo alternancia estricta **X→O→X→O** independiente por tablero.
- **2 jugadores**: el usuario juega **O** y **X** se genera automáticamente. En cada tablero nuevo, **X comienza automáticamente**.

## Notas

- Cada tablero es completamente independiente: estado, turno, victorias y empates.
- El programa maneja **redimensionado** de ventana (evento `KEY_RESIZE`), recalculando el layout.
- Si la terminal es demasiado pequeña para mostrar todos los tableros configurados, se mostrará un aviso.
