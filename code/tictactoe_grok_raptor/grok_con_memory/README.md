# TicTacToe NCurses

Un juego completo de Tres en Raya (TicTacToe) implementado en C++ utilizando la librería ncurses para la interfaz en terminal. Soporta múltiples tableros independientes, controles de teclado y ratón, y diferentes modos de juego.

## Características

- **Múltiples tableros**: Hasta 9 tableros simultáneos, cada uno completamente independiente
- **Modos de juego**:
  - 0 jugadores: Todos los tableros se llenan automáticamente con jugadas aleatorias
  - 1 jugador: Control manual de X y O en todos los tableros
  - 2 jugadores: Jugador controla O, X se genera automáticamente
- **Controles**: Teclado (flechas, Enter, Tab, R, Q) y ratón (clic para seleccionar)
- **Interfaz adaptativa**: Se ajusta al tamaño de la terminal
- **Estadísticas**: Seguimiento de victorias por tablero

## Requisitos

- Compilador g++
- Librería ncurses
- Sistema Linux

## Compilación

```bash
cd code/tictactoe-ncurses
make
```

## Ejecución

```bash
make run
# o directamente
./bin/tictactoe
```

## Controles

### Menú Principal
- Flechas arriba/abajo: Navegar opciones
- Enter: Seleccionar

### Menú de Ajustes
- Flechas arriba/abajo: Seleccionar opción
- Flechas izquierda/derecha: Cambiar valor
- Enter: Volver al menú principal

### Juego
- Flechas: Mover cursor
- Enter: Colocar ficha
- Tab: Cambiar tablero
- R: Reiniciar tablero actual
- Q: Salir al menú
- Ratón: Clic en celda para seleccionar y colocar

## Modos de Juego

### Modo 0 (0 jugadores)
- Todos los tableros juegan automáticamente
- Se muestran las jugadas con pausas visuales
- Al finalizar, muestra estadísticas

### Modo 1 (1 jugador)
- Control manual de ambas fichas (X y O)
- Alternancia estricta X → O → X → O por tablero
- Jugador decide todas las jugadas

### Modo 2 (2 jugadores)
- Jugador controla O
- X se genera automáticamente tras cada turno del jugador
- Ideal para jugar contra la máquina

## Estructura del Proyecto

```
code/tictactoe-ncurses/
├── include/
│   ├── Board.h
│   ├── UI.h
│   ├── Game.h
│   ├── Menu.h
│   └── Settings.h
├── src/
│   ├── Board.cpp
│   ├── UI.cpp
│   ├── Game.cpp
│   ├── Menu.cpp
│   ├── Settings.cpp
│   └── main.cpp
├── Makefile
└── README.md
```

## Ejemplos de Uso

1. **Juego automático con 4 tableros**:
   - Ir a Ajustes
   - Cambiar Jugadores a 0
   - Cambiar Tableros a 4
   - Jugar

2. **Juego manual con 2 tableros**:
   - Ajustes: Jugadores 1, Tableros 2
   - Usar Tab para cambiar entre tableros
   - Colocar X y O alternadamente

3. **Juego vs máquina con 1 tablero**:
   - Ajustes: Jugadores 2, Tableros 1
   - Colocar O, la máquina responde con X

## Manejo de Errores

- Validación de configuración en Settings
- Verificación de movimientos válidos en Board
- Adaptación a tamaño de terminal en UI

## Notas Técnicas

- Implementa hit testing para soporte de ratón
- Layout dinámico de tableros
- Gestión de estado independiente por tablero
- Colores ncurses para mejor UX