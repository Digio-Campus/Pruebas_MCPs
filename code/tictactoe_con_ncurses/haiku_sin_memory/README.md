# Tic Tac Toe - Juego en Terminal con ncurses

Un juego completo de Tres en Raya (Tic Tac Toe) implementado en C++ usando la librería ncurses para una interfaz de terminal moderna y interactiva.

## Características

### Modos de Juego
- **0 Jugadores (Automático)**: Todos los tableros se rellenan automáticamente con jugadas completamente aleatorias
- **1 Jugador (Manual)**: Controlas tanto las X como las O manualmente, respetando el turno alternativo en cada tablero
- **2 Jugadores (Con IA)**: Tú eres O y la IA controla X automáticamente

### Características Técnicas
- **Múltiples Tableros**: Configurable de 1, 2, 4, 6 o 9 tableros simultáneamente
- **Tableros Independientes**: Cada tablero tiene su propio estado, turno y estadísticas
- **Interfaz Interactiva**: Navegación con teclado (flechas/WASD)
- **Detección Automática**: Detecta victorias, empates y reinicios
- **Adaptable**: Se ajusta al tamaño de la terminal
- **Manejo de Errores**: Validación completa de entrada y estados
- **Redimensionamiento**: Soporta cambios de tamaño de ventana durante el juego

## Requisitos

### Sistema
- Linux con compilador g++
- Librería ncurses development (ncurses-dev)
- Terminal estándar

### Instalación de Dependencias

En Ubuntu/Debian:
```bash
sudo apt-get install build-essential ncurses-dev
```

En Fedora/RHEL:
```bash
sudo dnf install gcc-c++ ncurses-devel
```

En macOS:
```bash
brew install ncurses
```

## Compilación

### Método 1: Usando Makefile
```bash
make              # Compilar
make run          # Compilar y ejecutar
make clean        # Limpiar archivos compilados
make help         # Ver ayuda
```

### Método 2: Compilación Manual
```bash
g++ -Wall -Wextra -std=c++11 -O2 -o tictactoe main.cpp board.cpp game.cpp ui.cpp -lncurses
./tictactoe
```

## Uso

### Menú Principal
Cuando inicia el programa, verá el menú principal con las opciones:
1. **Jugar**: Inicia una nueva partida
2. **Ajustes**: Configura el modo y número de tableros
3. **Ayuda**: Muestra instrucciones detalladas
4. **Salir**: Sale del programa

### Controles de Juego

| Control | Acción |
|---------|--------|
| Flechas/WASD | Mover cursor |
| ENTER/Espacio | Colocar marca |
| TAB | Cambiar entre tableros |
| R | Reiniciar tablero actual |
| ESC | Volver al menú principal |

### Configuración

Al seleccionar "Jugar", debe configurar:

1. **Número de Jugadores**:
   - 0: Automático aleatorio
   - 1: Control manual (ambos jugadores)
   - 2: Jugador vs IA

2. **Número de Tableros**:
   - 1 tablero
   - 2 tableros (1x2)
   - 4 tableros (2x2)
   - 6 tableros (2x3)
   - 9 tableros (3x3)

## Estructura del Proyecto

```
tictactoe/
├── board.h              # Definición de la clase Board
├── board.cpp            # Implementación de tablero individual
├── game.h               # Definición de la clase Game
├── game.cpp             # Implementación del juego (múltiples tableros)
├── ui.h                 # Definición de la interfaz ncurses
├── ui.cpp               # Implementación de la interfaz
├── main.cpp             # Punto de entrada
├── Makefile             # Script de compilación
└── README.md            # Este archivo
```

### Descripción de Módulos

#### board.h/board.cpp
- Gestiona un tablero individual de 3x3
- Controla el turno alternativo (X ↔ O)
- Detecta ganadores y empates
- Mantiene estadísticas por tablero

#### game.h/game.cpp
- Coordina múltiples tableros
- Gestiona los modos de juego
- Implementa la IA simple (movimientos aleatorios)
- Calcula estadísticas globales

#### ui.h/ui.cpp
- Interfaz con ncurses
- Renderización de menús y tableros
- Manejo de entrada del usuario
- Manejo de redimensionamiento de ventana

#### main.cpp
- Punto de entrada del programa
- Bucle principal de eventos
- Integración de componentes

## Ejemplos de Uso

### Ejemplo 1: Juego Automático
```
1. En el menú principal, selecciona "Jugar"
2. Selecciona "0" para 0 jugadores
3. Selecciona "5" para 9 tableros (3x3)
4. Observa cómo se rellenan automáticamente
```

### Ejemplo 2: Juego Manual de 1 Jugador
```
1. En el menú principal, selecciona "Jugar"
2. Selecciona "1" para 1 jugador (control manual)
3. Selecciona "1" para 1 tablero
4. Usa WASD/Flechas para mover el cursor
5. Presiona ENTER para colocar marca
6. Alterna entre X y O manualmente
```

### Ejemplo 3: Juego vs IA
```
1. En el menú principal, selecciona "Jugar"
2. Selecciona "2" para 2 jugadores (con IA)
3. Selecciona "2" para 2 tableros
4. Tú eres O, la IA es X
5. Presiona ENTER para hacer tu movimiento
6. La IA hará automáticamente su movimiento
7. TAB para cambiar entre tableros
```

## Reglas del Juego

1. **Tableros Independientes**: Cada tablero es completamente independiente
2. **Turno Alternativo**: En cada tablero, X → O → X → O
3. **Victoria**: Tres marcas alineadas (fila, columna o diagonal)
4. **Empate**: Tablero lleno sin ganador
5. **Reinicio Individual**: Cada tablero puede reiniciarse independientemente

## Características Técnicas Avanzadas

### Manejo de Errores
- Validación de límites en todos los índices
- Comprobación de casillas ocupadas
- Manejo seguro de memoria con punteros

### Adaptación de Terminal
- Cálculo dinámico de posiciones basado en tamaño
- Soporte para redimensionamiento en tiempo real
- Ajuste automático de tamaño de tableros

### Eficiencia
- Compilación con optimización (-O2)
- Uso de referencias y punteros donde sea apropiado
- Minimización de renderización innecesaria

## Notas de Desarrollo

### Implementación de IA
Actualmente usa estrategia aleatoria simple. Para mejorar:
1. Implementar detección de victoria inmediata
2. Implementar bloqueo de movimientos ganadores
3. Usar algoritmo minimax para jugadas óptimas

### Futuras Mejoras
- [ ] IA más inteligente (minimax)
- [ ] Persistencia de puntuaciones
- [ ] Temas de color personalizados
- [ ] Replay de partidas
- [ ] Estadísticas globales

## Troubleshooting

### Error: "ncurses not found"
```bash
sudo apt-get install ncurses-dev  # Ubuntu/Debian
```

### Error de compilación: "undefined reference"
Asegúrese de que el Makefile incluya `-lncurses` en LDFLAGS

### La terminal se ve distorsionada
Presione `Ctrl+L` para refrescar o redimensione la ventana

### El programa se cuelga en automático
Presione `ESC` para volver al menú

## Autor y Licencia

Proyecto educativo de Tres en Raya en C++ con ncurses.
Disponible bajo licencia MIT.

## Referencias

- [ncurses Documentation](https://tldp.org/HOWTO/NCURSES-Programming-HOWTO/)
- [Tic Tac Toe Rules](https://en.wikipedia.org/wiki/Tic-tac-toe)
- [C++ Standard Library](https://en.cppreference.com/)
