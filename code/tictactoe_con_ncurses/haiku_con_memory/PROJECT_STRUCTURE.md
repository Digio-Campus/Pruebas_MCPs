# Estructura del Proyecto

## Árbol de Directorios

```
code/tictactoe-ncurses/
├── main.cpp                 # Punto de entrada (303 bytes)
├── Board.h                  # Interfaz tablero (926 bytes)
├── Board.cpp                # Implementación tablero (3.1 KB)
├── UI.h                     # Interfaz UI (1.1 KB)
├── UI.cpp                   # Implementación UI (7.6 KB)
├── Input.h                  # Interfaz entrada (381 bytes)
├── Input.cpp                # Implementación entrada (919 bytes)
├── Settings.h               # Interfaz configuración (506 bytes)
├── Settings.cpp             # Implementación configuración (1.0 KB)
├── Game.h                   # Interfaz juego (786 bytes)
├── Game.cpp                 # Implementación juego (7.4 KB)
├── Makefile                 # Build system (455 bytes)
├── build.sh                 # Script utilidad (1.8 KB)
├── README.md                # Guía principal (4.8 KB)
├── INSTALL.md               # Guía instalación (6.6 KB)
├── EXAMPLES.md              # Ejemplos uso (5.3 KB)
├── FAQ.md                   # Preguntas frecuentes (6.6 KB)
├── TECHNICAL.md             # Documentación técnica (7.2 KB)
└── PROJECT_STRUCTURE.md     # Este archivo

Tamaño total de código: ~34 KB compilado
Tamaño de documentación: ~31 KB
```

## Descripción de Ficheros

### Código Fuente

#### main.cpp
- **Propósito**: Punto de entrada del programa
- **Responsabilidad**: Crear instancia Game y ejecutar loop principal
- **Error handling**: Try-catch para excepciones, limpieza de ncurses

#### Board.h / Board.cpp
- **Propósito**: Lógica de un tablero individual 3x3
- **Clases**: 
  - `CellState`: Enumeración de estado de celda (EMPTY, X, O)
  - `GameState`: Enumeración de estado del juego
  - `Board`: Clase principal
- **Métodos públicos**: 
  - `makeMove()` - Realiza un movimiento
  - `getCell()` - Obtiene valor de celda
  - `getCurrentTurn()` - Obtiene turno actual
  - `getGameState()` - Obtiene estado del juego
  - `isGameOver()` - Verifica si terminó
- **Métodos privados**:
  - `checkWin()` - Verifica victoria
  - `isBoardFull()` - Verifica tablero lleno
  - `updateGameState()` - Actualiza estado

#### UI.h / UI.cpp
- **Propósito**: Renderizado con ncurses
- **Responsabilidad**: Dibujar tableros, menús, detectar clics
- **Estructura de datos**:
  - `BoardPosition`: Almacena posición y tamaño de un tablero
- **Métodos públicos**:
  - `drawAllBoards()` - Dibuja todos los tableros
  - `drawMainMenu()` - Menú principal
  - `drawSettingsMenu()` - Menú configuración
  - `drawHelpMenu()` - Menú ayuda
  - `cellClicked()` - Detecta clic en celda
  - `calculateBoardPositions()` - Calcula layout
- **Sistema de colores**:
  - Color pair 1: Normal
  - Color pair 2: Seleccionado
  - Color pair 3: X (rojo)
  - Color pair 4: O (azul)
  - Color pair 5: Éxito (verde)

#### Input.h / Input.cpp
- **Propósito**: Abstracción de entrada (teclado + ratón)
- **Estructura**:
  - `MouseEvent`: Evento de ratón con x, y, botón, estado
- **Métodos**:
  - `getKeypress()` - Lee siguiente tecla
  - `getMouseEvent()` - Lee evento de ratón
  - `enableMouse()` - Activa detección de ratón
  - `disableMouse()` - Desactiva detección

#### Settings.h / Settings.cpp
- **Propósito**: Almacenar configuración global
- **Datos**:
  - `gameMode` - Modo de juego (0/1/2 jugadores)
  - `numBoards` - Cantidad de tableros
  - `boardsPerRow` - Tableros por fila (calculado)
- **Métodos**:
  - `getGameMode()` - Obtiene modo
  - `getNumBoards()` - Obtiene cantidad de tableros
  - `getBoardsPerRow()` - Obtiene tableros por fila
  - `setGameMode()` - Establece modo
  - `setNumBoards()` - Establece cantidad
  - `calculateLayout()` - Adapta al tamaño de terminal

#### Game.h / Game.cpp
- **Propósito**: Orquestación principal del juego
- **Responsabilidad**: Coordinar tableros, entrada, IA, turnos
- **Miembros principales**:
  - `boards` - Vector de tableros
  - `settings` - Configuración global
  - `currentBoardIdx` - Tablero seleccionado
  - `gameActive` - Flag de juego activo
- **Métodos principales**:
  - `run()` - Loop principal
  - `mainMenu()` - Muestra menú principal
  - `playGame()` - Loop de juego
  - `makeAIMove()` - Ejecuta movimiento de IA
  - `makeRandomMove()` - Movimiento aleatorio
  - `initializeGame()` - Inicializa juego

### Archivos de Build

#### Makefile
- **Compilador**: g++ con C++17
- **Flags**:
  - `-std=c++17` - Estándar C++17
  - `-Wall -Wextra` - Warnings completos
  - `-O2` - Optimización nivel 2
- **Librería enlazada**: ncurses
- **Targets**:
  - `all` - Compila (por defecto)
  - `clean` - Elimina .o y binario
  - `run` - Compila y ejecuta
  - `rebuild` - Clean + all

#### build.sh
- **Propósito**: Script de utilidad para compilación/ejecución
- **Comandos**:
  - `build` - Compila
  - `run` - Compila y ejecuta
  - `clean` - Limpia
  - `rebuild` - Recompila
  - `help` - Muestra ayuda

### Documentación

#### README.md
- Guía de inicio rápido
- Requisitos y dependencias
- Instrucciones compilación/ejecución
- Controles y modos de juego
- Estructura del proyecto

#### INSTALL.md
- Guía de instalación detallada por SO
- Verificación de requisitos
- Solución de problemas
- Instalación en servidores remotos
- Desinstalación

#### EXAMPLES.md
- 7 ejemplos prácticos de uso
- Casos de uso específicos
- Flujo típico de juego
- Solución de problemas comunes
- Casos educativos

#### FAQ.md
- Preguntas frecuentes organizadas por categoría
- Compilación, ejecución, gameplay, controles
- Modos, interfaz, técnico
- Contribuciones, rendimiento

#### TECHNICAL.md
- Arquitectura general del proyecto
- Descripción detallada de clases
- Flujo del juego
- Manejo de turnos independientes
- Detección de victorias
- Modos de juego
- Detección de ratón
- Layout adaptable
- Manejo de colores
- Extensiones futuras

#### PROJECT_STRUCTURE.md
- Este archivo
- Descripción de todos los ficheros
- Tamaños y propósitos
- Organización lógica

## Organización Lógica

### Módulos por Responsabilidad

```
┌─────────────────────────────────────────┐
│         main.cpp (Punto Entrada)        │
└────────────────┬────────────────────────┘
                 │
                 ▼
        ┌─────────────────┐
        │   Game (Orquesta)│
        └─────────────────┘
         /        |        \
        /         |         \
       ▼          ▼          ▼
    ┌─────────┐ ┌────────┐ ┌──────────┐
    │ Board[] │ │   UI   │ │  Input   │
    └─────────┘ └────────┘ └──────────┘
                    │
                    ▼
            ┌─────────────────┐
            │   Settings      │
            └─────────────────┘
```

### Flujo de Datos

```
Input (Keyboard/Mouse)
  ↓
Game::playGame()
  ├─→ Board::canMakeMove()
  ├─→ Board::makeMove()
  ├─→ Game::makeAIMove()
  └─→ UI::drawAllBoards()
  
Output (Screen)
```

## Tamaños Aproximados

```
Código fuente (.cpp/.h):
  Board: 4.0 KB
  UI: 8.8 KB
  Game: 8.2 KB
  Input: 1.3 KB
  Settings: 1.5 KB
  main: 0.3 KB
  ────────────
  Total: ~24 KB

Binario compilado: 38 KB (con símbolos)
Binario optimizado: ~18 KB (sin símbolos)

Documentación: 31 KB

Tamaño total: ~89 KB
```

## Dependencias Externas

- **ncurses**: Para UI en terminal
  - `<ncurses.h>`
  - Linker: `-lncurses`

- **C++ Standard Library** (incluidas):
  - `<array>` - Grillas de tablero
  - `<vector>` - Listas de tableros/celdas vacías
  - `<cstdlib>` - Números aleatorios
  - `<ctime>` - Inicializar seed
  - `<algorithm>` - Funciones std
  - `<string>` - Strings para UI
  - `<iostream>` - Input/output standard

## Compilación Condicional

El proyecto no usa ninguna compilación condicional (#ifdef/#ifndef),
pero podría extenderse con:

```cpp
#define DEBUG_MODE      // Para logs de debug
#define ENABLE_SOUND    // Para soporte de sonido
#define NETWORK_MODE    // Para multiplayer
```

## Convenciones de Nombres

- **Clases**: PascalCase (Board, Game, UI)
- **Métodos**: camelCase (makeMove, getCurrentTurn)
- **Variables**: camelCase (currentBoardIdx, gameMode)
- **Enumeraciones**: PascalCase (CellState, GameState)
- **Constantes**: UPPER_CASE (no hay en este proyecto)

## Comentarios en Código

El código está comentado minimalmente:
- Solo cuando es necesario clarificar lógica compleja
- Sin comentarios obvios (variable x, asignar valor)
- Enfoque: código autodocumentado

## Extensibilidad

El proyecto está diseñado para permitir:
- Nuevos modos de juego (modificar Game.cpp)
- Mejora de IA (modificar makeAIMove())
- Nuevos temas de color (modificar UI.cpp)
- Soporte de redes (refactorizar Game)
- Guardado de partidas (nueva clase Save)

---

Documento generado: Febrero 2025
