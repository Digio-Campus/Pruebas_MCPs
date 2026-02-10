# Documentación Técnica - Tic-Tac-Toe ncurses

## Arquitectura General

El proyecto está organizado en módulos independientes:

```
┌─────────┐
│  main() │
└────┬────┘
     │
     ▼
┌────────────┐
│   Game     │ (Control principal)
└────┬───┬──┘
     │   │
     ▼   ▼
┌──────┐ ┌────────┐ ┌───────┐ ┌──────────┐
│Board │ │   UI   │ │ Input │ │Settings  │
└──────┘ └────────┘ └───────┘ └──────────┘
```

## Clases Principales

### Board.h/Board.cpp

Encapsula la lógica de un tablero individual 3x3.

**Estados**:
```cpp
enum class CellState { EMPTY, X, O };
enum class GameState { IN_PROGRESS, X_WINS, O_WINS, DRAW };
```

**Responsabilidades**:
- Almacenar estado del tablero
- Validar movimientos
- Detectar victorias y empates
- Mantener turno actual
- Proporcionar información de celdas vacías

**Métodos clave**:
- `makeMove(row, col, player)` - Realiza un movimiento
- `checkWin(player)` - Verifica si un jugador ganó
- `getCurrentTurn()` - Retorna X u O
- `getEmptyCells()` - Lista celdas disponibles

### Game.h/Game.cpp

Orquesta múltiples tableros y el flujo del juego.

**Responsabilidades**:
- Gestionar múltiples instancias de Board
- Manejar el loop principal
- Procesar entrada del usuario
- Coordinar turnos entre tableros
- Generar movimientos de IA

**Estado**:
```cpp
std::vector<Board> boards;  // Todos los tableros activos
int currentBoardIdx;         // Tablero seleccionado
GameMode gameMode;           // 0, 1, o 2 jugadores
```

**Métodos principales**:
- `run()` - Loop principal del juego
- `playGame()` - Pantalla de juego activo
- `makeAIMove(boardIdx)` - IA juega
- `makeRandomMove(boardIdx)` - Movimiento aleatorio

### UI.h/UI.cpp

Maneja todo el renderizado con ncurses.

**Responsabilidades**:
- Dibujar tableros con bordes ACS
- Renderizar menús
- Mostrar estado del juego
- Calcular posiciones de tableros
- Detectar clics de ratón

**Características**:
- Colores (COLOR_PAIR 1-5)
- Caracteres de línea (ACS_VLINE, ACS_HLINE, etc.)
- Diseño adaptable a terminal

**Layout de tablero**:
```
┌─────────┐
│ X│ │O   │  7 filas
├─┼─┼─┤   │  11 columnas
│ │X│    │
├─┼─┼─┤   │
│O│ │X   │  Células: 3x3
└─────────┘
[TURN: O]
```

### Input.h/Input.cpp

Abstracción para entrada de teclado y ratón.

**Responsabilidades**:
- Leer caracteres del teclado
- Procesar eventos de ratón
- Activar/desactivar entrada de ratón

**Evento de ratón**:
```cpp
struct MouseEvent {
    int x, y;        // Posición en pantalla
    int button;      // Qué botón
    bool pressed;    // ¿Presionado?
};
```

**Métodos**:
- `getKeypress()` - Retorna siguiente tecla
- `getMouseEvent(event)` - Llena estructura MouseEvent
- `enableMouse()` - Activa detección de ratón

### Settings.h/Settings.cpp

Almacena y gestiona configuración global.

**Datos**:
```cpp
GameMode gameMode;    // ZERO, ONE, o TWO_PLAYERS
int numBoards;        // 1-9
int boardsPerRow;     // Calculado según terminal
```

**Métodos**:
- `setGameMode(mode)` - Cambia modo
- `setNumBoards(num)` - Cambia cantidad de tableros
- `calculateLayout(width, height)` - Adapta al tamaño

## Flujo del Juego

### Inicialización
```
main() 
  → Game()
    → UI::init() 
      → initscr(), start_color(), mouseinterval()
    → Input::enableMouse()
  → Game::run()
```

### Loop principal
```
while (running) {
    mainMenu()      ← Espera selección
    playGame()      ← Juega si selecciona "Play"
    helpMenu()      ← Muestra ayuda si selecciona "Help"
}
```

### Loop de juego
```
while (gameActive) {
    drawAllBoards()
    ch = getKeypress() o getMouseEvent()
    
    if (validMove) {
        board.makeMove()
        if (2 players) IA juega
    }
    
    updateGame()  ← Verifica si todos terminaron
}
```

## Manejo de Turnos Independientes

Cada tablero mantiene su propio turno:

```cpp
// Board::getCurrentTurn() retorna X u O
// Board::makeMove() alterna automáticamente:
currentTurn = (currentTurn == CellState::X) ? CellState::O : CellState::X;
```

**Ejemplo con 3 tableros**:
```
Tablero 0: TURNO X (ha jugado: X, O, X)
Tablero 1: TURNO O (ha jugado: X, O, X, O)  ← Más jugadas
Tablero 2: TURNO X (ha jugado: X)           ← Menos jugadas
```

## Detección de Victorias

Se verifica después de cada movimiento:

```cpp
// Filas
for (int i = 0; i < 3; i++) {
    if (grid[i][0] == player && grid[i][1] == player && grid[i][2] == player)
        return true;
}

// Columnas
for (int j = 0; j < 3; j++) {
    if (grid[0][j] == player && grid[1][j] == player && grid[2][j] == player)
        return true;
}

// Diagonales
// [0,0] [1,1] [2,2]
// [0,2] [1,1] [2,0]
```

## Modos de Juego

### Modo 0 Jugadores (Automático)
```cpp
for (int i = 0; i < numBoards; i++) {
    while (!boards[i].isGameOver()) {
        makeRandomMove(i);
    }
}
```

### Modo 1 Jugador (Manual ambos lados)
```cpp
// Usuario presiona SPACE
board.makeMove(row, col, board.getCurrentTurn());
// El turno cambia automáticamente en makeMove()
// Siguiente vez que presione SPACE, será el otro jugador
```

### Modo 2 Jugadores (Humano vs IA)
```cpp
if (settings.getGameMode() == GameMode::TWO_PLAYERS 
    && !board.isGameOver()) {
    makeAIMove(boardIdx);  // Ejecuta automáticamente
}
```

## Detección de Ratón

```cpp
MEVENT mevent;
int ch = getch();
if (ch == KEY_MOUSE && getmouse(&mevent) == OK) {
    event.x = mevent.x;
    event.y = mevent.y;
    event.button = mevent.bstate & BUTTON1_PRESSED;
}
```

**Hit testing en celda**:
```cpp
bool UI::cellClicked(int mouseX, int mouseY, int boardIdx, 
                     int& row, int& col) {
    // 1. Verificar si está dentro de los límites del tablero
    // 2. Calcular posición relativa
    // 3. Mapear a fila/columna de 3x3
    // 4. Retornar posición si es válida
}
```

## Layout Adaptable

```cpp
void Settings::calculateLayout(int termWidth, int termHeight) {
    // Cada tablero: ~11 chars ancho, ~7 alto
    boardsPerRow = std::max(1, termWidth / 11);
    int maxRows = std::max(1, termHeight / 7);
    int maxBoards = boardsPerRow * maxRows;
    numBoards = std::min(numBoards, maxBoards - 1);
}
```

**Ejemplo**:
- Terminal 120x30
- Celdas por fila: 120 / 11 = 10 (pero máx 9)
- Filas de celdas: 30 / 7 = 4
- Máximo: 9 x 4 = 36 tableros (limitado a 9)

## Manejo de Colores

```cpp
start_color();
init_pair(1, COLOR_BLACK, COLOR_WHITE);  // Normal
init_pair(2, COLOR_WHITE, COLOR_BLACK);  // Seleccionado
init_pair(3, COLOR_RED, COLOR_BLACK);    // X
init_pair(4, COLOR_BLUE, COLOR_BLACK);   // O
init_pair(5, COLOR_GREEN, COLOR_BLACK);  // Éxito
```

## Compilación y Optimización

```makefile
CXXFLAGS = -std=c++17 -Wall -Wextra -O2

# -std=c++17: Usa características de C++17
# -Wall -Wextra: Warnings completos
# -O2: Optimización nivel 2
```

## Consideraciones de Rendimiento

- **Redibujado**: Solo `drawAllBoards()` por frame
- **Entrada**: `nodelay()` evita bloqueos
- **Almacenamiento**: Arrays fijos 3x3 por tablero
- **IA**: Búsqueda lineal de celdas vacías (simple pero eficiente)

## Extensiones Futuras

1. **IA mejorada**: Minimax, alpha-beta
2. **Guardado de partidas**: Serializar state
3. **Estadísticas**: Rastrear ganancias/empates
4. **Temas**: Sistema de colores personalizable
5. **Redes**: Multiplayer en red
6. **Sonido**: Efectos de ncurses

---

Documentación generada para versión 1.0
