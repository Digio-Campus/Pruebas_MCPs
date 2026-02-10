# Estructura del Código - Documentación Técnica

## 📋 Índice

1. [Organización General](#organización-general)
2. [Clase Board](#clase-board)
3. [Clase Game](#clase-game)
4. [Clase UI](#clase-ui)
5. [main.cpp](#maincpp)
6. [Convenciones y Patrones](#convenciones-y-patrones)

---

## Organización General

### Estructura de Carpetas
```
tictactoe/
├── Headers (.h)
│   ├── board.h       (55 líneas)
│   ├── game.h        (40 líneas)
│   └── ui.h          (45 líneas)
├── Implementación (.cpp)
│   ├── board.cpp     (135 líneas)
│   ├── game.cpp      (95 líneas)
│   ├── ui.cpp        (280 líneas)
│   └── main.cpp      (90 líneas)
└── Compilación
    └── Makefile
```

### Dependencias de Compilación
```
main.cpp
├── depends on: game.h, ui.h
├── includes: iostream, ncurses.h
└── compiles to: main.o

board.cpp (board.h)
├── includes: board.h, cstdio, cstdlib, ctime, vector
└── compiles to: board.o

game.cpp (game.h)
├── includes: game.h, board.h, cstdlib, ctime, algorithm
└── compiles to: game.o

ui.cpp (ui.h)
├── includes: ui.h, game.h, ncurses.h, cstring, cstdio, algorithm
└── compiles to: ui.o

Final Link: main.o board.o game.o ui.o + -lncurses → tictactoe
```

---

## Clase Board

### Archivos
- **board.h**: Definición de interfaz
- **board.cpp**: Implementación

### Propósito
Gestiona un único tablero 3x3 de Tic Tac Toe.

### Estructura de Datos Principales

```cpp
class Board {
  private:
    CellState grid[3][3];    // Tablero 3x3
    CellState currentTurn;   // Turno actual (X o O)
    CellState winner;        // Ganador (EMPTY si no hay)
    bool isFull;            // ¿Tablero lleno?
    int moveCount;          // Movimientos realizados
    int xWins;              // Victorias de X
    int oWins;              // Victorias de O
    int draws;              // Empates
}
```

### Enum CellState

```cpp
enum CellState {
    EMPTY = 0,
    PLAYER_X = 1,
    PLAYER_O = 2
};
```

### Métodos Principales

#### Constructor/Destructor
```cpp
Board();           // Inicializa tablero vacío
~Board();          // Limpieza
```

#### Métodos de Juego
```cpp
bool makeMove(int row, int col);    // Hacer movimiento
void reset();                        // Limpiar tablero
CellState checkWinner();             // Verificar ganador
bool isGameOver();                   // ¿Juego terminó?
bool isCellEmpty(int row, int col);  // ¿Casilla vacía?
```

#### Getters
```cpp
CellState getCell(int row, int col) const;
CellState getCurrentTurn() const;
CellState getWinner() const;
bool getIsFull() const;
int getMoveCount() const;
int getXWins() const;
int getOWins() const;
int getDraws() const;
```

#### Setters
```cpp
void setCurrentTurn(CellState turn);
void incrementXWins();
void incrementOWins();
void incrementDraws();
```

### Algoritmo: checkWinner()

```cpp
CellState Board::checkWinner() {
    // Verificar 3 filas
    for (int i = 0; i < 3; i++) {
        if (grid[i][0] == grid[i][1] && grid[i][1] == grid[i][2])
            if (grid[i][0] != EMPTY) return grid[i][0];
    }
    
    // Verificar 3 columnas
    for (int j = 0; j < 3; j++) {
        if (grid[0][j] == grid[1][j] && grid[1][j] == grid[2][j])
            if (grid[0][j] != EMPTY) return grid[0][j];
    }
    
    // Verificar diagonal principal
    if (grid[0][0] == grid[1][1] && grid[1][1] == grid[2][2])
        if (grid[0][0] != EMPTY) return grid[0][0];
    
    // Verificar diagonal secundaria
    if (grid[0][2] == grid[1][1] && grid[1][1] == grid[2][0])
        if (grid[0][2] != EMPTY) return grid[0][2];
    
    return EMPTY;  // Sin ganador
}
```

### Flujo de makeMove()

```
makeMove(row, col)
    ├─ Validar límites (0-2)
    ├─ Verificar casilla vacía
    ├─ Colocar marca del turno actual
    ├─ Incrementar contador de movimientos
    ├─ Verificar ganador
    │   └─ Si hay ganador: actualizar estadísticas
    ├─ Verificar empate (9 movimientos)
    │   └─ Si empate: incrementar draws
    └─ Cambiar turno (X ↔ O) si no terminó
```

---

## Clase Game

### Archivos
- **game.h**: Definición de interfaz
- **game.cpp**: Implementación

### Propósito
Coordina múltiples tableros y gestiona lógica de juego.

### Enum GameMode

```cpp
enum GameMode {
    MODE_AUTO = 0,    // 0 Jugadores - Automático
    MODE_MANUAL = 1,  // 1 Jugador - Manual
    MODE_AI = 2       // 2 Jugadores - Con IA
};
```

### Estructura de Datos

```cpp
class Game {
  private:
    std::vector<Board*> boards;  // Vector de tableros
    int numBoards;               // Número de tableros
    GameMode gameMode;           // Modo actual
    bool isRunning;              // ¿Juego en ejecución?
}
```

### Métodos Principales

#### Inicialización
```cpp
void initGame(int numBoards, GameMode mode);
void reset();
```

#### Gestión de Tableros
```cpp
Board* getBoard(int index);      // Obtener tablero por índice
int getNumBoards() const;        // Número de tableros
```

#### Movimientos
```cpp
bool makeMove(int boardIndex, int row, int col);
bool makeAIMove(int boardIndex);
int getAIMove(int boardIndex);   // Estrategia IA
```

#### Utilidades
```cpp
void resetBoard(int boardIndex);
GameMode getGameMode() const;
bool getIsRunning() const;
void setIsRunning(bool running);
```

### Algoritmo: getAIMove()

```cpp
int Game::getAIMove(int boardIndex) {
    Board* board = boards[boardIndex];
    std::vector<std::pair<int, int>> available = 
        board->getAvailableMoves();
    
    if (available.empty()) return -1;
    
    // Estrategia actual: movimiento aleatorio
    // (Mejorables: minimax, bloqueo, ganar)
    
    int randomIndex = rand() % available.size();
    return available[randomIndex].first * 3 + 
           available[randomIndex].second;
}
```

### Flujo de makeMove()

```
makeMove(boardIndex, row, col)
    ├─ Validar índice de tablero
    ├─ Obtener tablero
    ├─ Verificar si juego ya terminó
    ├─ Hacer movimiento en tablero
    └─ Si modo IA y turno de X:
        └─ makeAIMove automáticamente
```

---

## Clase UI

### Archivos
- **ui.h**: Definición de interfaz
- **ui.cpp**: Implementación

### Propósito
Gestiona toda la interfaz con ncurses y entrada/salida.

### Estructura de Datos

```cpp
struct UIState {
    int screenWidth;         // Ancho de terminal
    int screenHeight;        // Alto de terminal
    int selectedBoard;       // Tablero activo
    int cursorRow;          // Fila cursor (0-2)
    int cursorCol;          // Columna cursor (0-2)
    bool resize;            // ¿Ventana redimensionada?
};

class UI {
  private:
    WINDOW* mainWindow;      // Ventana ncurses
    std::vector<WINDOW*> boardWindows;  // Ventanas tableros
    UIState state;           // Estado actual
    Game* game;              // Referencia a Game
}
```

### Métodos de Inicialización

```cpp
bool initNCurses();      // Inicializar ncurses
void cleanupNCurses();   // Limpiar ncurses
```

### Menús

```cpp
int showMainMenu();                        // 1-4
void showSettingsMenu(int& numPlayers, 
                     int& numBoards);      // Configuración
void showHelpMenu();                       // Ayuda
```

### Renderizado

```cpp
void render();                             // Renderizar todo
void renderAllBoards();                    // Todos los tableros
void renderBoard(int index, int x, 
                 int y, int w, int h);    // Un tablero
void renderStatusBar();                    // Barra inferior
void clearScreen();                        // Limpiar pantalla
void displayMessage(const std::string&);   // Mensaje
```

### Manejo de Entrada

```cpp
int handleInput(int ch);       // Procesar tecla
void handleBoardNavigation();  // Navegar tableros
void handleCursorMovement();   // Mover cursor
```

### Detección de Evento

```cpp
bool checkWindowResize();      // ¿Se redimensionó?
void updateWindowSize();       // Actualizar tamaño
```

### Visualización de Tablero

```
Board 1
┌─────────┐
│ X O . │  <- cursor invertido
│ . X . │
│ . O . │
└─────────┘
X:1 O:1 E:0
```

---

## main.cpp

### Estructura General

```cpp
int main() {
    try {
        Game game;                    // Crear juego
        UI ui(&game);                 // Crear interfaz
        
        // Inicializar ncurses
        if (!ui.initNCurses()) {
            return 1;  // Error
        }
        
        // Bucle de menú principal
        while (true) {
            int choice = ui.showMainMenu();
            
            switch (choice) {
                case 1: runGameLoop(game, ui);  break;
                case 2: // Ajustes
                case 3: ui.showHelpMenu();      break;
                case 4: return 0;               break;
            }
        }
        
        ui.cleanupNCurses();
    } 
    catch (const std::exception& e) {
        endwin();  // Limpiar ncurses
        return 1;
    }
    
    return 0;
}
```

### Función: runGameLoop()

```cpp
int runGameLoop(Game& game, UI& ui) {
    // Obtener ajustes
    int numPlayers, numBoards;
    ui.showSettingsMenu(numPlayers, numBoards);
    
    // Inicializar juego
    GameMode mode = (GameMode)numPlayers;
    game.initGame(numBoards, mode);
    
    // Bucle principal
    while (game.getIsRunning()) {
        ui.updateWindowSize();
        ui.render();
        
        int ch = getch();  // Obtener entrada
        
        // Procesar según modo
        if (mode == MODE_AUTO) {
            // Hacer movimientos automáticos
        } else if (mode == MODE_MANUAL) {
            // Permitir entrada de usuario
        } else if (mode == MODE_AI) {
            // IA responde automáticamente
        }
    }
    
    return 0;
}
```

---

## Convenciones y Patrones

### Nombres

#### Variables
```cpp
int moveCount;           // camelCase para variables locales
bool isFull;            // Prefijo "is" para booleanos
int xWins;              // Nombres descriptivos
```

#### Funciones/Métodos
```cpp
void makeMove();        // Verbo + sustantivo
bool isGameOver();      // Prefijo "is" para bool
CellState checkWinner(); // Verbo descriptivo
```

#### Constantes
```cpp
enum CellState { EMPTY, PLAYER_X, PLAYER_O };  // MAYÚSCULAS
```

### Patrones de Diseño

#### Encapsulación
```cpp
private:
    CellState grid[3][3];  // Datos privados
public:
    bool makeMove(...);    // Interfaz pública
```

#### Validación
```cpp
bool Board::makeMove(int row, int col) {
    if (row < 0 || row > 2) return false;  // Validar
    if (grid[row][col] != EMPTY) return false;
    // Procesar
}
```

#### Inicialización
```cpp
Board::Board() {
    reset();              // Usar reset() en constructor
    xWins = 0;
    oWins = 0;
}
```

### Estilos de Codificación

#### Indentación
```cpp
if (condition) {           // 4 espacios
    statement();
    if (nested) {
        inner_statement();
    }
}
```

#### Espaciado
```cpp
int x = 5;                // Espacios alrededor de =
if (x > 0) {              // Espacio después de if
    for (int i = 0; i < 3; i++) {  // Espacios en for
```

#### Comentarios
```cpp
// Comentarios de una línea para explicaciones breves

/*
   Comentarios multilínea para explicaciones
   más complejas
*/

// Comentarios en encabezados de funciones
```

### Gestión de Memoria

```cpp
// Asignación
Board* board = new Board();

// Uso
board->makeMove(0, 0);

// Limpieza
delete board;
```

### Manejo de Errores

```cpp
bool success = game.makeMove(...);
if (!success) {
    // Manejar error
    return false;
}
```

---

## Flujo de Ejecución Completo

### 1. Inicio
```
main()
├─ Crear Game y UI
├─ initNCurses()
└─ Mostrar menú principal
```

### 2. Configuración
```
showSettingsMenu()
├─ Seleccionar modo (0, 1, 2 jugadores)
└─ Seleccionar número de tableros (1, 2, 4, 6, 9)
```

### 3. Juego
```
runGameLoop()
├─ initGame(numBoards, mode)
├─ while (isRunning):
│   ├─ render()          -> mostrar tableros
│   ├─ getch()           -> obtener entrada
│   └─ handleInput()     -> procesar entrada
│       ├─ Si TAB: cambiar tablero
│       ├─ Si ENTER: makeMove()
│       ├─ Si R: resetBoard()
│       └─ Si ESC: salir
└─ Volver a menú
```

### 4. Ciclo de Movimiento
```
makeMove(boardIndex, row, col)
├─ Validar
├─ Colocar marca
├─ checkWinner()
│   └─ Si ganador: incrementar stats
├─ Cambiar turno
└─ Si modo IA: makeAIMove()
```

---

## Puntos de Extensión

### Para Mejorar IA
```cpp
// En game.cpp, reemplazar getAIMove():
int Game::getAIMove(int boardIndex) {
    // Implementar minimax
    // Implementar evaluación de posiciones
    // Implementar bloqueo de movimientos ganadores
}
```

### Para Agregar Persistencia
```cpp
// Agregar métodos en Game:
bool saveGame(const std::string& filename);
bool loadGame(const std::string& filename);
```

### Para Agregar Temas
```cpp
// Agregar en UI:
void setColorTheme(const std::string& theme);
void applyTheme();
```

---

## Conclusión

El código está estructurado de manera clara y modular:
- **Board**: Lógica de un tablero individual
- **Game**: Coordinación de múltiples tableros
- **UI**: Interfaz con el usuario
- **main**: Integración de componentes

Cada clase tiene una responsabilidad única y bien definida, facilitando mantenimiento y extensión.
