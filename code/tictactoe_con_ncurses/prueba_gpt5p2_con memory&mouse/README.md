# Tres en Raya (ncurses) — múltiples tableros independientes

Proyecto completo en **C++17** con interfaz en terminal usando **ncurses**.

## Requisitos

- Linux
- `g++`, `make`
- ncurses (en Debian/Ubuntu: `sudo apt-get install libncurses5-dev`)

## Compilación y ejecución

```bash
cd code/tictactoe-ncurses
make
./bin/tictactoe
```

O directamente:

```bash
make run
```

## Menú principal

- **Jugar**: inicia la partida mostrando todos los tableros configurados.
- **Ajustes**: configura modo (0/1/2 jugadores) y número de tableros.
- **Ayuda**: controles, reglas y modos.
- **Salir**: termina el programa.

## Modos de jugadores

- **0 jugadores**: todos los tableros se rellenan automáticamente con jugadas **aleatorias** (X y O) hasta terminar cada uno.
- **1 jugador**: el usuario juega **manual** tanto X como O en cada tablero, respetando la alternancia **X→O** por tablero.
- **2 jugadores**: el usuario controla **O**; **X** se genera automáticamente (aleatorio) cuando sea el turno de X en cada tablero.

## Controles (en partida)

- Flechas: mover el cursor dentro del tablero seleccionado.
- `Enter`: colocar ficha (según el turno del tablero).
- `Tab` o `]` / `[`: cambiar de tablero.
- `r`: reiniciar **solo** el tablero seleccionado (estadísticas se mantienen).
- `a`: reiniciar **todos** los tableros (estadísticas se mantienen).
- `q` o `Esc`: volver al menú.
- Redimensionado: el juego se adapta; si hay más tableros de los que caben, se limita al máximo visible.

## Reglas

- Cada tablero empieza con turno **X**.
- Tras una X, siempre juega O; tras una O, siempre juega X.
- Se detectan automáticamente victorias (3 en línea) y empates.
- Cada tablero mantiene estadísticas independientes: victorias de X, victorias de O y empates.
