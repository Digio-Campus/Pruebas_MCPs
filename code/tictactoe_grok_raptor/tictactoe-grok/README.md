# TicTacToe NCurses

Un juego completo de Tres en Raya (Tic-Tac-Toe) implementado en C++ con interfaz de terminal usando la librería ncurses. Soporta múltiples tableros independientes, diferentes modos de juego y navegación por teclado.

## Características

- **Múltiples tableros**: Hasta 9 tableros simultáneos, cada uno completamente independiente
- **Modos de juego**:
  - 0 jugadores: Relleno automático aleatorio
  - 1 jugador: Control manual de X y O alternando
  - 2 jugadores: Jugador controla O, X generado automáticamente
- **Interfaz ncurses**: Colores, navegación por teclado, adaptación al tamaño de terminal
- **Estadísticas**: Conteo de victorias, derrotas y empates por tablero
- **Reinicio**: Individual por tablero o global

## Requisitos

- Compilador C++17 (g++)
- Librería ncurses
- Linux

## Compilación

```bash
cd code/tictactoe-ncurses
make
```

Esto compilará el ejecutable en `bin/tictactoe`.

## Ejecución

```bash
make run
# o directamente:
./bin/tictactoe
```

## Controles

### Menú Principal
- ↑/↓: Navegar entre opciones
- Enter: Seleccionar opción
- q: Salir

### Menú de Ajustes
- ↑/↓: Seleccionar opción
- ←/→: Cambiar valor
- Enter: Volver al menú principal

### Modo Juego
- ↑/↓/←/→: Mover cursor en el tablero seleccionado
- Enter: Colocar ficha
- Tab: Cambiar al siguiente tablero
- 1-9: Seleccionar tablero directamente
- r: Reiniciar tablero actual
- R: Reiniciar todos los tableros
- q: Volver al menú principal

## Ejemplos de Uso

1. **Juego automático**:
   - Configurar 0 jugadores, 4 tableros
   - Ver cómo se llenan automáticamente

2. **Juego manual**:
   - Configurar 1 jugador, 1 tablero
   - Colocar X y O alternando

3. **Juego vs AI**:
   - Configurar 2 jugadores, 1 tablero
   - Colocar O, ver cómo AI coloca X

## Tests

Ejecutar tests unitarios:

```bash
make test
```

## Estructura del Proyecto

```
code/tictactoe-ncurses/
├── include/          # Headers
│   ├── Board.h
│   ├── UI.h
│   ├── Game.h
│   ├── Menu.h
│   └── Settings.h
├── src/              # Implementaciones
│   ├── Board.cpp
│   ├── UI.cpp
│   ├── Game.cpp
│   ├── Menu.cpp
│   ├── Settings.cpp
│   └── main.cpp
├── tests/            # Tests
│   └── test_board.cpp
├── bin/              # Ejecutable (generado)
├── obj/              # Objetos (generado)
└── Makefile
```

## Limpieza

```bash
make clean
```

Elimina archivos compilados.