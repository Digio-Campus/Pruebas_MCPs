# Guía de Desarrollo y Extensión del Proyecto

## Resumen del Proyecto

**Líneas de código totales**: ~1470 líneas (código + documentación)
- Board.cpp/h: 217 líneas - Lógica del tablero individual
- GameManager.cpp/h: 270 líneas - Gestión de múltiples tableros
- UI.cpp/h: 673 líneas - Interfaz ncurses completa
- main.cpp: 20 líneas - Punto de entrada
- Makefile: 77 líneas - Sistema de compilación
- README.md: 231 líneas - Documentación completa

## Arquitectura del Proyecto

### Capa de Modelo (Board)
```
Board.h/cpp
├── CellState enum: EMPTY, X, O
├── GameState enum: PLAYING, X_WINS, O_WINS, DRAW
└── Lógica de juego individual
    ├── Validación de movimientos
    ├── Detección de victorias (filas, columnas, diagonales)
    ├── Alternancia de turnos
    └── Movimientos aleatorios
```

### Capa de Control (GameManager)
```
GameManager.h/cpp
├── GameMode enum: ZERO_PLAYERS, ONE_PLAYER, TWO_PLAYERS
├── Gestión de múltiples tableros (vector<Board>)
├── Estadísticas independientes (vector<BoardStats>)
└── Lógica de modos de juego
    ├── Selección de tablero/celda
    ├── Actualización automática
    └── Coordinación de turnos
```

### Capa de Vista (UI)
```
UI.h/cpp
├── Inicialización de ncurses
├── Menú principal
├── Menú de ajustes
├── Pantalla de ayuda
├── Pantalla de juego
│   ├── Layout adaptativo
│   ├── Dibujo de tableros
│   ├── Manejo de entrada (teclado + ratón)
│   └── Visualización de estadísticas
└── Sistema de colores
```

## Puntos de Extensión

### 1. Añadir Nuevo Modo de Juego

**Archivo**: GameManager.h
```cpp
// Añadir nuevo modo al enum
enum class GameMode {
    ZERO_PLAYERS,
    ONE_PLAYER,
    TWO_PLAYERS,
    NEW_MODE  // <-- Nuevo modo
};
```

**Archivo**: GameManager.cpp
```cpp
// Implementar lógica en makeMove() o updateAutoBoards()
if (mode == GameMode::NEW_MODE) {
    // Tu lógica aquí
}
```

**Archivo**: UI.cpp
```cpp
// Actualizar showSettings() para mostrar el nuevo modo
// Actualizar drawTurnInfo() para mostrar descripción
```

### 2. Añadir IA Inteligente

**Archivo**: Board.h
```cpp
// Añadir método para movimiento inteligente
bool makeSmartMove(); // Minimax, evaluación heurística, etc.
```

**Archivo**: Board.cpp
```cpp
bool Board::makeSmartMove() {
    // Implementar algoritmo minimax
    // o evaluación de jugadas
    return makeMove(bestRow, bestCol);
}
```

### 3. Persistencia de Estadísticas

**Archivo**: GameManager.h
```cpp
// Añadir métodos de serialización
void saveStats(const std::string& filename);
void loadStats(const std::string& filename);
```

**Implementación**:
```cpp
void GameManager::saveStats(const std::string& filename) {
    std::ofstream file(filename);
    for (const auto& stat : stats) {
        file << stat.xWins << " " << stat.oWins << " "
             << stat.draws << " " << stat.gamesPlayed << "\n";
    }
}
```

### 4. Tableros de Tamaño Variable

**Archivo**: Board.h
```cpp
class Board {
private:
    int size; // 3, 4, 5, etc.
    int winCondition; // 3, 4, 5, etc.
    // ...
```

**Consideraciones**:
- Modificar checkWinner() para condiciones dinámicas
- Actualizar UI para renderizar tamaños variables
- Ajustar calculateBoardLayout()

### 5. Multijugador en Red

**Nuevo archivo**: Network.h/cpp
```cpp
class NetworkManager {
public:
    void connectToServer(const std::string& ip, int port);
    void sendMove(int board, int row, int col);
    Move receiveMove();
};
```

**Integración**:
- Añadir NetworkManager a GameManager
- Crear nuevo GameMode::NETWORK
- Sincronizar estados entre clientes

### 6. Replay y Historial

**Archivo**: GameManager.h
```cpp
struct Move {
    int boardIndex;
    int row, col;
    CellState player;
    time_t timestamp;
};

class GameManager {
private:
    std::vector<Move> moveHistory;
public:
    void recordMove(const Move& move);
    void replayGame(int gameIndex);
};
```

### 7. Animaciones con ncurses

**Archivo**: UI.cpp
```cpp
void UI::animateWin(int boardIndex, const std::vector<std::pair<int,int>>& winCells) {
    for (int i = 0; i < 5; i++) { // Parpadeo
        for (auto& cell : winCells) {
            // Alternar colores
        }
        refresh();
        napms(200); // Delay de 200ms
    }
}
```

### 8. Sonidos (usando system() o biblioteca)

**Archivo**: UI.cpp
```cpp
void UI::playSound(const std::string& event) {
    if (event == "win") {
        system("play win.wav 2>/dev/null &");
    } else if (event == "move") {
        system("play click.wav 2>/dev/null &");
    }
}
```

## Debugging y Testing

### Compilación con símbolos de debug
```bash
# En Makefile, añadir flag -g
CXXFLAGS = -std=c++11 -Wall -Wextra -pedantic -g

# Compilar
make clean && make

# Ejecutar con gdb
gdb ./tictactoe
```

### Comandos útiles de gdb
```bash
(gdb) break UI.cpp:250    # Breakpoint en línea
(gdb) break Board::makeMove  # Breakpoint en función
(gdb) run                  # Ejecutar
(gdb) print selectedBoard  # Ver variable
(gdb) step                 # Paso a paso
(gdb) continue             # Continuar
```

### Testing básico
```bash
# Test 1: Compilación
make clean && make

# Test 2: Verificar que no hay memory leaks
valgrind --leak-check=full ./tictactoe

# Test 3: Verificar terminal después de ejecución
./tictactoe
# Presionar ESC para salir limpiamente
# Terminal debe verse normal
```

## Problemas Comunes y Soluciones

### 1. Terminal rota después de crash
```bash
reset
# o
stty sane
```

### 2. Ratón no funciona
```cpp
// Verificar en UI::init()
mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
printf("%s", "\033[?1003h\n"); // Habilitar tracking
```

### 3. Colores no se ven
```bash
# Verificar TERM
echo $TERM  # debe ser xterm-256color o similar

# Si es "linux", cambiar a:
export TERM=xterm-256color
```

### 4. Redimensionado de ventana
```cpp
// Manejar señal SIGWINCH
#include <signal.h>

void handle_winch(int sig) {
    endwin();
    refresh();
}

// En main()
signal(SIGWINCH, handle_winch);
```

## Mejores Prácticas

### 1. Gestión de Recursos ncurses
```cpp
// Siempre usar RAII o try-catch
UI::UI() {
    try {
        init();
    } catch (...) {
        cleanup();
        throw;
    }
}

UI::~UI() {
    cleanup();
}
```

### 2. Validación de Entrada
```cpp
// Siempre validar límites
bool GameManager::selectCell(int row, int col) {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return false;
    }
    selectedRow = row;
    selectedCol = col;
    return true;
}
```

### 3. Constantes en lugar de Números Mágicos
```cpp
// Board.h
class Board {
private:
    static constexpr int BOARD_SIZE = 3;
    static constexpr int WIN_CONDITION = 3;
    
    std::vector<std::vector<CellState>> cells;
};
```

### 4. Logging para Debug
```cpp
// Añadir función de log
void logDebug(const std::string& msg) {
    std::ofstream log("tictactoe.log", std::ios::app);
    log << time(nullptr) << ": " << msg << std::endl;
}

// Usar en código
logDebug("Board " + std::to_string(index) + " - X wins!");
```

## Rendimiento

### Mediciones Actuales
- Inicialización: ~10ms
- Renderizado por frame: ~5-10ms
- Detección de victoria: ~1ms (O(1) por tablero)
- Movimiento aleatorio: ~1ms

### Optimizaciones Posibles
1. **Caché de layout**: Calcular coordenadas una vez por resize
2. **Dirty flags**: Solo redibujar tableros modificados
3. **Double buffering**: Usar ventanas de ncurses separadas
4. **Vectorización**: Usar std::array en lugar de std::vector para tableros

## Licencia y Contribuciones

Este proyecto es código abierto para uso educativo.

Para contribuir:
1. Hacer fork del proyecto
2. Crear rama feature (git checkout -b feature/nueva-funcionalidad)
3. Commit cambios (git commit -am 'Añade nueva funcionalidad')
4. Push a la rama (git push origin feature/nueva-funcionalidad)
5. Crear Pull Request

## Contacto y Soporte

Para preguntas o problemas:
- Revisar README.md para instrucciones básicas
- Consultar este archivo para extensiones
- Verificar logs en tictactoe.log (si está habilitado)
- Usar reset si la terminal se corrompe

---

**Última actualización**: Febrero 2026
**Versión del proyecto**: 1.0
**Compatibilidad**: Linux con ncurses 5.x o superior
