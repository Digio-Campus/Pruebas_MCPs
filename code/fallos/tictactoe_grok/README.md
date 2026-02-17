# TicTacToe Keyboard

Juego de Tres en Raya implementado en C++ con interfaz ncurses para terminal.

## Características

- Múltiples tableros simultáneos (1-9), cada uno independiente
- Tres modos de juego:
  - 0 jugadores: Modo automático
  - 1 jugador: Control manual de X y O
  - 2 jugadores: Jugador controla O, X automático
- Navegación por teclado (flechas, Enter)
- Detección automática de victorias y empates
- Reinicio individual de tableros
- Estadísticas por tablero
- Adaptación dinámica al tamaño de terminal

## Compilación

Requiere g++ y ncurses.

```bash
cd code/tictactoe_keyboard
make
```

## Ejecución

```bash
make run
# o
./bin/tictactoe
```

## Controles

### Menú Principal
- Flechas arriba/abajo: Navegar
- Enter: Seleccionar

### Menú Ajustes
- Flechas arriba/abajo: Seleccionar opción
- Flechas izquierda/derecha: Cambiar valor
- Enter: Volver

### Juego
- Flechas: Mover cursor
- Enter: Colocar ficha
- Tab: Cambiar tablero
- R: Reiniciar tablero seleccionado
- Q: Salir al menú

## Modos de Juego

### Modo 0 (Automático)
Los tableros se llenan automáticamente con jugadas aleatorias.

### Modo 1 (Manual)
El jugador controla tanto X como O, alternando turnos estrictamente.

### Modo 2 (Vs IA)
El jugador controla O, la X se genera automáticamente.

## Estructura del Proyecto

- `include/`: Archivos de cabecera (.h)
- `src/`: Archivos fuente (.cpp)
- `bin/`: Ejecutable
- `obj/`: Archivos objeto

## Dependencias

- g++ (compilador C++)
- ncurses (librería para interfaz terminal)
- make (herramienta de compilación)

En Ubuntu/Debian: `sudo apt install build-essential libncurses5-dev libncursesw5-dev`