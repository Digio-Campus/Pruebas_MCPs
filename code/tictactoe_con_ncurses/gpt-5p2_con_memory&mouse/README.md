# Tres en Raya (ncurses) - Multi-tablero

Proyecto en C++ que implementa el juego **Tres en Raya** con interfaz en terminal usando **ncurses**, soportando **varios tableros simultáneos e independientes** (cada uno con su turno, victoria/empate y estadísticas).

## Requisitos
- Linux
- g++
- ncurses (paquete de desarrollo, p. ej. `libncurses-dev`)
- make

## Compilación
Desde la carpeta del proyecto:

```bash
cd code/tictactoe-ncurses
make
```

## Ejecución

```bash
make run
# o
./bin/tictactoe
```

## Menú principal (ratón + teclado)
- **Jugar**: inicia la partida.
- **Ajustes**: configura modo (0/1/2 jugadores) y número de tableros (1-9).
- **Ayuda**: muestra controles y reglas.
- **Salir**: cierra el programa.

## Ajustes
- **Jugadores**:
  - **0**: todos los tableros se completan automáticamente con jugadas aleatorias (X y O). Al terminar un tablero, se reinicia solo tras una breve pausa.
  - **1**: control manual de **X y O** (sin IA). Se respeta estrictamente la alternancia **X → O → X → O** en cada tablero.
  - **2**: el jugador controla **O** y la **X** se genera automáticamente cuando toque jugar X (en cada tablero de forma independiente).
- **Tableros**: cantidad de tableros simultáneos (1–9). El layout se adapta al tamaño de la terminal.

## Controles en partida
- Flechas: mover el cursor por la celda seleccionada.
- Enter / Espacio: colocar ficha.
- Tab: cambiar de tablero.
- R: reiniciar el tablero activo (mantiene estadísticas).
- Q: volver al menú.

### Ratón
- Menú/Ajustes: clic para seleccionar.
- Partida: clic en un tablero para activarlo; clic en una celda para jugar (si el modo lo permite).

## Ejemplos de uso
- **Modo auto**: Ajustes → Jugadores = 0, Tableros = 6 → Jugar (verás 6 tableros auto-jugándose en paralelo).
- **Modo manual**: Ajustes → Jugadores = 1, Tableros = 3 → Jugar (manejas X y O en cada tablero, alternando turnos por tablero).
- **Modo 2 jugadores**: Ajustes → Jugadores = 2, Tableros = 4 → Jugar (colocas O; X aparece automáticamente cuando corresponda).

## Notas / Troubleshooting
- Para que el ratón funcione, tu terminal debe soportar eventos de ratón en ncurses.
- Si la terminal es pequeña, reduce el número de tableros o agranda la ventana.
