# Tic Tac Toe - ncurses Edition

Un juego clásico de Tres en Raya (Tic Tac Toe) implementado en C++ con interfaz en terminal usando la librería ncurses. Soporta múltiples tableros simultáneos, tres modos de juego, y control total por teclado y ratón.

## Características

- **Múltiples Tableros**: Visualiza y juega en 1-9 tableros simultáneamente en la misma pantalla
- **Tres Modos de Juego**:
  - **Modo 0 (Auto)**: CPU vs CPU - El juego se juega automáticamente con movimientos aleatorios
  - **Modo 1 (Manual)**: Un jugador controla tanto X como O manualmente
  - **Modo 2 (vs AI)**: Jugador controla O, CPU controla X automáticamente
- **Interfaz Intuitiva**: Menú navegable, visualización clara de estados, indicadores de turno
- **Control Total**:
  - Navegación por teclado (flechas, Tab, números 1-9)
  - Soporte completo para ratón
  - Atajos disponibles (H para ayuda, R para reinicio, Q para salir)
- **Estadísticas por Tablero**: Victorias de X, victorias de O, empates
- **Redimensionamiento de Terminal**: Se adapta automáticamente al tamaño disponible
- **Detección Automática**: Detecta victorias, empates y gestiona turnos independientes por tablero

## Requisitos

- Linux/Unix (o sistema compatible con ncurses)
- GCC 7.0+ (con soporte C++17)
- ncurses (libncurses5-dev en Debian/Ubuntu)
- Make (para compilación)

### Instalación de Dependencias

**En Debian/Ubuntu:**
```bash
sudo apt-get install libncurses5-dev build-essential
```

**En Fedora/RHEL:**
```bash
sudo dnf install ncurses-devel gcc-c++
```

**En macOS (con Homebrew):**
```bash
brew install ncurses
```

## Compilación y Ejecución

### Compilación

```bash
cd code/tictactoe-ncurses
make
```

O simplemente:
```bash
make -C code/tictactoe-ncurses
```

### Ejecución

```bash
make run
```

O ejecutar directamente:
```bash
./bin/tictactoe
```

### Limpieza

```bash
make clean
```

### Tests

```bash
make test
```

## Estructura del Proyecto

```
code/tictactoe-ncurses/
├── include/          # Headers (.h)
│   ├── Board.h      # Clase del tablero individual
│   ├── Game.h       # Controlador principal del juego
│   ├── Menu.h       # Sistema de menús
│   ├── Settings.h   # Configuración del juego
│   └── UI.h         # Interfaz con ncurses
├── src/             # Implementación (.cpp)
│   ├── Board.cpp
│   ├── Game.cpp
│   ├── Menu.cpp
│   ├── Settings.cpp
│   ├── UI.cpp
│   └── main.cpp     # Punto de entrada
├── tests/           # Tests unitarios
│   └── test_main.cpp
├── Makefile         # Script de compilación
└── README.md        # Este archivo
```

## Uso del Juego

### Menú Principal

Al ejecutar el programa, verás el menú principal con opciones:
- **Play**: Comienza una partida con los parámetros actuales
- **Settings**: Ajusta número de jugadores y tableros
- **Help**: Muestra instrucciones
- **Exit**: Sale del programa

Navega con **↑↓** y selecciona con **Enter**.

### Configuración (Settings)

En el menú de ajustes puedes configurar:

**Número de Jugadores:**
- `0`: Modo automático (CPU vs CPU)
- `1`: Manual (tú controlas X y O alternando)
- `2`: Versus CPU (tú eres O, CPU es X)

**Número de Tableros:**
- `1-9`: Cantidad de tableros a mostrar simultáneamente
- Usa **←→** para cambiar valores

### Controles Durante el Juego

| Control | Acción |
|---------|--------|
| **↑↓←→** | Mueve cursor dentro del tablero |
| **Tab** | Cambia al siguiente tablero |
| **1-9** | Selecciona tablero directamente |
| **Enter** | Coloca símbolo en celda seleccionada |
| **Ratón** | Click para colocar símbolo en celda |
| **R** | Reinicia tablero actual |
| **Shift+R** | Reinicia todos los tableros |
| **H** | Muestra pantalla de ayuda |
| **Q** | Vuelve al menú principal |

### Pantalla de Juego

Cada tablero muestra:
- **Título**: Número del tablero y estado (turno/ganador/empate)
- **Grid 3x3**: Celdas vacías, X, u O
- **Estadísticas**: Contadores de victorias y empates
- **Cursor**: Celda seleccionada resaltada en color

## Ejemplos de Uso

### Ejemplo 1: Modo Automático con 4 Tableros

```bash
./bin/tictactoe
# En el menú:
# - Settings → Cambiar "Number of Players" a 0
# - Settings → Cambiar "Number of Boards" a 4
# - Play
# Observa cómo las CPU juegan automáticamente en los 4 tableros
```

### Ejemplo 2: Juego Manual 1 Jugador

```bash
./bin/tictactoe
# En el menú:
# - Settings → "Number of Players" = 1
# - Settings → "Number of Boards" = 1
# - Play
# Usa flechas y Enter para jugar X y O manualmente en el mismo tablero
```

### Ejemplo 3: Versus CPU

```bash
./bin/tictactoe
# En el menú:
# - Settings → "Number of Players" = 2
# - Settings → "Number of Boards" = 2
# - Play
# Juega como O contra la CPU (X) en 2 tableros simultáneos
```

## Características Técnicas

### Diseño y Arquitectura

- **Namespace**: Todo el código está en el namespace `ttt`
- **Modularización**: Separación clara entre lógica (Board), UI (UI), control (Game), y configuración (Settings)
- **Encapsulación**: Todos los miembros de clase son privados con acceso controlado vía getters/setters
- **RAII**: Gestión automática de ncurses con constructores/destructores

### Estándares de Código

- **C++17**: Utiliza features modernas como structured bindings
- **Enums de Clase**: `Cell::X`, `Cell::O`, `Cell::Empty`, `Result::X_Win`, `Result::O_Win`, `Result::Draw`, `Result::Ongoing`
- **Generador Aleatorio**: `std::mt19937` con `std::random_device` (no `rand()`)
- **Compilación**: `-std=c++17 -Wall -Wextra -O2`

### Lógica del Juego

- Cada tablero mantiene su propio estado y turnos independientes
- Turno inicial siempre es X
- Alternancia X → O → X → O estricta por tablero
- La alternancia se detiene automáticamente al detectar victoria o empate
- Estadísticas se registran una única vez per juego (prevención de doble conteo)

### Interfaz de Usuario

- Dibuja bordes con caracteres ACS de ncurses (no ASCII simple)
- Códigos de color:
  - **Rojo**: Símbolos X
  - **Cyan**: Símbolos O
  - **Verde**: Selección y estadísticas
  - **Amarillo**: Menú e información
- Cursor visual con A_REVERSE | A_BOLD
- Barra de controles contextual en la parte inferior

### Manejo de Eventos

- **Teclado**: flechas (KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT), Tab, números, Enter
- **Ratón**: Click detection con getmouse() y hit testing
- **No-blocking**: Modo auto usa nodelay() con delays animados

## Testing

Se incluyen tests unitarios completos para las clases principales:

```bash
make test
```

Tests cubiertos:
- Creación de tableros y estadísticas
- Movimientos válidos e inválidos
- Detección de victorias (filas, columnas, diagonales)
- Detección de empates
- Reinicio de tableros
- Movimientos disponibles
- Validación de configuración

## Solución de Problemas

### "Error: Terminal is too small!"
El juego requiere un mínimo de 80x24 caracteres. Expande tu ventana de terminal.

### El programa no responde a clics de ratón
Algunos emuladores de terminal no soportan eventos de ratón. Usa el teclado como alternativa:
- Flechas para mover
- Enter para confirmar

### Compilación falla con "ncurses.h not found"
Instala el paquete de desarrollo de ncurses:
- Debian/Ubuntu: `sudo apt-get install libncurses5-dev`
- Fedora/RHEL: `sudo dnf install ncurses-devel`
- macOS: `brew install ncurses`

### Problemas de colores en terminal
Algunos terminales viejos tienen soporte limitado de colores. La interfaz debería funcionar igual aunque los colores no se muestren.

## Notas de Desarrollo

- El proyecto sigue convenciones de C++ moderno con strong typing y RAII
- Todos los métodos potencialmente problemáticos incluyen validación de rango
- El código es extensible: agregar nuevos modos o tipos de juego es relativamente simple
- La UI es independiente de la lógica del juego (patrón Model-View-Controller)

## Licencia

Proyecto educativo para demostrar programación en C++ con ncurses.

## Autor

Desarrollado como proyecto completo de TicTacToe en terminal.

---

**¡Diviértete jugando Tic Tac Toe en tu terminal!**
