# Arquitectura del Proyecto Tictactoe

## Visión General

Este documento describe la arquitectura y diseño del juego Tictactoe implementado en C++ con ncurses.

## Diagrama de Componentes

```
┌─────────────────────────────────────────────────┐
│                   main.cpp                       │
│            (Punto de entrada)                    │
└─────────────┬───────────────────────────────────┘
              │
              ├──────────┬──────────┬──────────┐
              │          │          │          │
              ▼          ▼          ▼          ▼
       ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐
       │   Menu   │ │   Game   │ │    UI    │ │  Player  │
       └──────────┘ └─────┬────┘ └──────────┘ └──────────┘
                          │
                          ▼
                    ┌──────────┐
                    │  Board   │
                    └──────────┘
```

## Capas de Arquitectura

### 1. Capa de Presentación (Presentation Layer)
- **UI Class**: Maneja toda la interacción con ncurses
- **Menu Class**: Gestiona los menús y navegación
- Responsabilidades:
  - Renderizado de pantalla
  - Captura de eventos de teclado y ratón
  - Gestión de colores y formato

### 2. Capa de Lógica de Negocio (Business Logic Layer)
- **Game Class**: Coordina el flujo del juego
- **Board Class**: Implementa las reglas del Tictactoe
- **Player Class**: Define comportamiento de jugadores
- Responsabilidades:
  - Validación de movimientos
  - Detección de victorias y empates
  - Gestión de turnos
  - Movimientos automáticos

### 3. Capa de Datos (Data Layer)
- Estructuras de datos simples (vectores, matrices)
- GameConfig: Configuración del juego
- Puntuaciones y estadísticas

## Patrones de Diseño Utilizados

### 1. Strategy Pattern
- **Contexto**: Clase Player
- **Estrategias**: HUMAN vs AUTO
- **Beneficio**: Facilita la extensión con nuevas estrategias de IA

### 2. Model-View-Controller (MVC) Adaptado
- **Model**: Board, Game, Player
- **View**: UI
- **Controller**: Game + Menu
- **Beneficio**: Separación clara de responsabilidades

### 3. Facade Pattern
- **Fachada**: UI Class
- **Subsistema**: ncurses API
- **Beneficio**: Simplifica el uso de ncurses

## Flujo de Ejecución

### Inicialización
```
1. main() → UI::init() → ncurses setup
2. Menu::showMainMenu() → User input
3. Game::configure() → Setup players and boards
```

### Loop de Juego
```
while (game.isRunning()) {
    1. UI::renderGame() → Dibuja estado actual
    2. UI::processGameInput() → Captura entrada
       ├─ Mouse click → Game::processMove()
       └─ ESC/Q → game.stop()
    3. Game::processAutoMoves() → Movimientos IA
    4. Game::update() → Actualiza estado
}
```

### Detección de Victoria
```
Board::makeMove()
    → Board::checkWin()
        → Verifica filas, columnas, diagonales
        → Actualiza winner
    → Board::checkDraw()
        → Verifica si el tablero está lleno
```

## Estructuras de Datos Principales

### Board
```cpp
vector<vector<char>> grid;  // Tablero 3x3
char winner;                 // Estado del juego
int moves_count;             // Contador de movimientos
```

### Game
```cpp
vector<Board> boards;                 // Múltiples tableros
unique_ptr<Player> playerX, playerO;  // Jugadores
GameConfig config;                    // Configuración
int score_x, score_o, draws;          // Puntuaciones
```

### UI
```cpp
int term_width, term_height;  // Tamaño de terminal
bool mouse_enabled;           // Estado del ratón
```

## Gestión de Memoria

- **Smart Pointers**: Se usan `unique_ptr` para los jugadores
- **RAII**: UI usa constructor/destructor para init/cleanup de ncurses
- **Stack Allocation**: Boards y estructuras de datos en el stack cuando es posible
- **No leaks**: Verificado con valgrind (opcional)

## Manejo de Errores

### Try-Catch en main.cpp
```cpp
try {
    // Lógica del juego
} catch (const std::exception& e) {
    ui.cleanup();
    fprintf(stderr, "Error: %s\n", e.what());
    return 1;
}
```

### Validaciones
- Verificación de índices de tableros y celdas
- Validación de movimientos (casilla vacía, juego no terminado)
- Manejo de eventos de terminal (resize, mouse)

## Extensibilidad

### Agregar Nueva Estrategia de IA
1. Modificar `Player::getAutoMove()`
2. Agregar nuevo algoritmo (minimax, etc.)
3. Opcional: Agregar enum para seleccionar estrategia

### Agregar Nuevos Modos de Juego
1. Modificar `GameConfig`
2. Actualizar `Game::configure()`
3. Actualizar menú de ajustes

### Cambiar Tamaño de Tablero
1. Modificar constantes en `Board` (actualmente hardcoded a 3x3)
2. Actualizar `checkWin()` para nuevo tamaño
3. Ajustar renderizado en `UI::drawBoard()`

## Consideraciones de Rendimiento

- **Renderizado Eficiente**: Solo se actualiza cuando hay cambios
- **Delays Apropiados**: 50ms entre frames para balancear CPU y respuesta
- **Movimientos Automáticos**: Procesados en batch para todos los tableros
- **Layout Dinámico**: Calculado una vez por render

## Dependencias

### Compilación
- g++ con soporte C++14
- make

### Runtime
- libncurses
- libpthread (para std::this_thread)

### Sistema
- Linux (probado en Ubuntu)
- Terminal con soporte de ratón

## Testing (Recomendaciones)

### Tests Unitarios (No implementados)
- `BoardTest`: Verificar detección de victorias
- `GameTest`: Verificar lógica de turnos
- `PlayerTest`: Verificar movimientos automáticos

### Tests de Integración
- Menú de navegación
- Ciclo completo de juego
- Redimensionado de terminal

### Tests Manuales
```bash
# Modo 0 jugadores con múltiples tableros
# Modo 1 jugador - Victoria, derrota, empate
# Modo 2 jugadores
# Redimensionar terminal durante el juego
```

## Posibles Mejoras Futuras

1. **IA Mejorada**: Implementar algoritmo minimax
2. **Persistencia**: Guardar configuración y estadísticas
3. **Multijugador Red**: Soporte para juego en red
4. **Sonidos**: Efectos de sonido (con beep)
5. **Animaciones**: Transiciones suaves entre estados
6. **Temas**: Diferentes esquemas de colores
7. **Replays**: Grabar y reproducir partidas
8. **Torneos**: Sistema de torneos automáticos

## Conclusión

Este proyecto demuestra una arquitectura limpia y extensible para un juego de terminal, utilizando principios sólidos de diseño orientado a objetos y aprovechando las capacidades de ncurses para crear una experiencia de usuario interactiva.
