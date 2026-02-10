# Tic-Tac-Toe ncurses

Un juego de Tres en Raya completo implementado en C++ usando la librería ncurses para interfaz de terminal.

## Características

- **Múltiples tableros independientes**: Juega en varios tableros simultáneamente
- **Tres modos de juego**:
  - 0 Jugadores: Los tableros se rellenan automáticamente con movimientos aleatorios
  - 1 Jugador: Controlas X y O manualmente, alternando turnos en cada tablero
  - 2 Jugadores: Controlas O y la IA juega con X
- **Interfaz completa**: Menú principal, configuración, ayuda y pantalla de juego
- **Soporte de entrada**: Teclado + ratón
- **Adaptable a terminal**: La interfaz se ajusta al tamaño de la ventana
- **Independencia de tableros**: Cada tablero tiene su propio estado, turnos y victorias

## Requisitos

- GCC/G++ con soporte C++17
- ncurses development libraries
- Linux/Unix

### Instalación de dependencias

**Debian/Ubuntu:**
```bash
sudo apt-get install libncurses-dev build-essential
```

**Fedora:**
```bash
sudo dnf install ncurses-devel gcc-c++
```

**macOS:**
```bash
brew install ncurses
```

## Compilación

```bash
cd code/tictactoe-ncurses
make clean
make
```

Para compilar sin archivos objeto previos:
```bash
make rebuild
```

## Ejecución

```bash
make run
```

O directamente:
```bash
./tictactoe
```

## Controles

### Menú Principal
- **1**: Jugar
- **2**: Configuración
- **3**: Ayuda
- **4**: Salir
- **Ratón**: Hacer clic en opciones

### Durante el juego
- **Flechas arriba/abajo**: Mover entre filas
- **Flechas izquierda/derecha**: Mover entre columnas
- **AvPág/RePág**: Cambiar entre tableros
- **ESPACIO o ENTER**: Colocar X u O en la casilla seleccionada
- **R**: Reiniciar el tablero actual
- **Q**: Salir
- **Ratón**: Hacer clic en cualquier casilla para jugar

### Menú de Configuración
- **1/2/3**: Seleccionar modo de juego
- **1-9**: Número de tableros simultáneos
- **B**: Volver atrás
- **ENTER**: Confirmar

## Modos de Juego

### 0 Jugadores (Automático)
Los tableros se llenan completamente con movimientos aleatorios. Útil para ver cómo termina un juego automáticamente.

### 1 Jugador (Manual)
Controlas tanto X como O. Los turnos alternan automáticamente:
- Primero juegas X
- Luego juegas O
- Luego vuelves a jugar X
- Y así sucesivamente

Cada tablero mantiene su propia secuencia de turnos independientemente.

### 2 Jugadores (Humano vs IA)
- **Tú**: Juegas con O
- **IA**: Juega con X automáticamente después de tu movimiento

## Estructura del Proyecto

```
tictactoe-ncurses/
├── main.cpp              # Punto de entrada
├── Board.h/Board.cpp     # Lógica del tablero individual
├── Game.h/Game.cpp       # Control del juego y múltiples tableros
├── UI.h/UI.cpp           # Renderizado con ncurses
├── Input.h/Input.cpp     # Manejo de entrada (teclado/ratón)
├── Settings.h/Settings.cpp # Configuración global
├── Makefile              # Compilación
└── README.md             # Este archivo
```

## Detalles técnicos

### Independencia de tableros
- Cada tablero tiene su propio estado (EMPTY, X, O)
- Cada tablero mantiene su propio turno (X u O)
- La detección de victoria es independiente por tablero
- Los empates se manejan cuando el tablero se llena

### Alternancia de turnos
El juego respeta estrictamente:
```
X → O → X → O → X → O ...
```
en cada tablero de forma independiente.

### Detección de victorias
Se detecta automáticamente:
- Tres en fila (horizontal)
- Tres en columna (vertical)
- Tres en diagonal

## Compilación avanzada

Para compilar con símbolos de debug:
```bash
make CXXFLAGS="-std=c++17 -Wall -Wextra -g"
```

Para limpiar todos los archivos generados:
```bash
make clean
```

## Ejemplos de uso

1. **Juego automático con 4 tableros:**
   - Ejecuta: `./tictactoe`
   - Selecciona: 1 (Play)
   - En configuración: Selecciona 1 (0 Jugadores) y 4 tableros
   - Observa cómo se rellenan automáticamente

2. **Juego manual con 2 tableros:**
   - Ejecuta: `./tictactoe`
   - Selecciona: 1 (Play)
   - En configuración: Selecciona 2 (1 Jugador) y 2 tableros
   - Alterna entre X y O en ambos tableros

3. **Juego contra IA con 1 tablero:**
   - Ejecuta: `./tictactoe`
   - Selecciona: 1 (Play)
   - En configuración: Selecciona 3 (2 Jugadores) y 1 tablero
   - Juega O mientras IA juega X

## Manejo de errores

El programa maneja:
- Redimensionamiento de terminal en tiempo real
- Entrada de ratón inválida
- Movimientos inválidos
- Fin del juego por victoria o empate

## Notas de implementación

- La detección de ratón utiliza el protocolo ncurses estándar
- Los movimientos de IA son aleatorios por simplicidad
- El layout se adapta automáticamente al tamaño de la terminal
- Máximo 9 tableros simultáneos para mantener usabilidad

## Licencia

Este proyecto es código de ejemplo educativo.

## Autor

Desarrollado como proyecto educativo en C++17 con ncurses.
