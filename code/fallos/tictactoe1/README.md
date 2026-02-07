# TicTacToe con ncurses

Juego de TicTacToe completo implementado en C++ con interfaz de terminal usando la librería ncurses. Soporta múltiples tableros simultáneos, diferentes modos de juego, y controles tanto por teclado como por ratón.

## Características

### Modos de Juego

- **0 jugadores**: Todos los tableros se llenan automáticamente con jugadas aleatorias
- **1 jugador**: Un solo jugador controla tanto las X como las O, jugando manualmente en todos los tableros respetando el turno alterno X → O → X → O
- **2 jugadores**: El jugador controla las O y las X se generan automáticamente tras cada turno

### Tableros Múltiples

- Configurable de 1 a 9 tableros simultáneos
- Cada tablero es completamente independiente:
  - Mantiene su propio estado de juego
  - Tiene su propio turno (siempre alterna X → O → X → O)
  - Cuenta sus propias victorias y empates
  - No comparte información con otros tableros
- Los tableros se adaptan automáticamente al tamaño de la terminal

### Controles

#### En el Menú
- **Flechas arriba/abajo**: Navegar por las opciones
- **Enter**: Seleccionar opción
- **Q**: Salir

#### Durante el Juego
- **Flechas**: Mover el cursor dentro del tablero seleccionado
- **Enter**: Colocar ficha en la posición del cursor
- **Tab**: Cambiar al siguiente tablero
- **R**: Reiniciar el tablero actual
- **Q**: Volver al menú principal
- **Ratón**: Clic en una casilla para jugar (si está disponible)

### Menús

#### Menú Principal
- Jugar
- Ajustes
- Ayuda
- Salir

#### Menú de Ajustes
- Número de jugadores (0, 1 o 2)
- Número de tableros (1 a 9)
- Flechas izquierda/derecha para cambiar valores

#### Menú de Ayuda
- Reglas del juego
- Descripción de modos de juego
- Lista completa de controles
- Información sobre tableros múltiples

## Estructura del Proyecto

```
tictactoe/
├── include/
│   ├── Board.h          # Clase para tablero individual
│   ├── Game.h           # Controlador del juego
│   ├── Menu.h           # Sistema de menús
│   ├── Settings.h       # Configuración del juego
│   └── UI.h             # Interfaz con ncurses
├── src/
│   ├── Board.cpp        # Implementación del tablero
│   ├── Game.cpp         # Implementación del juego
│   ├── Menu.cpp         # Implementación de menús
│   ├── Settings.cpp     # Implementación de configuración
│   ├── UI.cpp           # Implementación de interfaz
│   └── main.cpp         # Punto de entrada
├── Makefile             # Script de compilación
└── README.md            # Este archivo
```

## Requisitos

### Sistema Operativo
- Linux (Ubuntu, Debian, Fedora, etc.)

### Dependencias
- **g++**: Compilador de C++ (compatible con C++11 o superior)
- **ncurses**: Librería para interfaz de terminal
- **make**: Sistema de construcción

### Instalación de Dependencias

#### Ubuntu/Debian
```bash
sudo apt-get update
sudo apt-get install g++ make libncurses5-dev libncurses-dev
```

#### Fedora
```bash
sudo dnf install gcc-c++ make ncurses-devel
```

#### Arch Linux
```bash
sudo pacman -S gcc make ncurses
```

## Compilación

### Compilar el proyecto
```bash
make
```

### Limpiar archivos compilados
```bash
make clean
```

### Compilar y ejecutar directamente
```bash
make run
```

### Ver ayuda del Makefile
```bash
make help
```

## Ejecución

Una vez compilado, ejecuta el juego con:

```bash
./bin/tictactoe
```

O directamente con:

```bash
make run
```

## Ejemplos de Uso

### Ejemplo 1: Juego rápido con 2 jugadores y 1 tablero
1. Ejecutar el juego
2. Seleccionar "Jugar" en el menú principal
3. Usar las flechas para mover el cursor
4. Presionar Enter para colocar fichas
5. El jugador controla O, X se genera automáticamente

### Ejemplo 2: Configurar múltiples tableros
1. Ejecutar el juego
2. Seleccionar "Ajustes"
3. Navegar a "Número de tableros"
4. Usar flechas izquierda/derecha para ajustar (ej: 4 tableros)
5. Volver y seleccionar "Jugar"
6. Usar Tab para cambiar entre tableros

### Ejemplo 3: Modo automático completo
1. Ejecutar el juego
2. Ir a "Ajustes"
3. Configurar "Número de jugadores" en 0
4. Configurar "Número de tableros" (ej: 6)
5. Volver y seleccionar "Jugar"
6. Observar cómo todos los tableros se llenan automáticamente
7. Presionar Q para salir cuando termine

### Ejemplo 4: Modo 1 jugador (control manual completo)
1. Ejecutar el juego
2. Ir a "Ajustes"
3. Configurar "Número de jugadores" en 1
4. Volver y seleccionar "Jugar"
5. Controlar tanto X como O manualmente
6. El juego respeta automáticamente el turno alterno X → O → X → O

## Características Técnicas

### Modularización
- Código separado en archivos .h (headers) y .cpp (implementación)
- Responsabilidades claramente separadas entre clases
- Bajo acoplamiento y alta cohesión

### Manejo de Errores
- Validación de movimientos
- Comprobación de límites de tablero
- Verificación de configuraciones válidas

### Soporte de Redimensionado
- La interfaz se adapta dinámicamente al tamaño de la terminal
- Los tableros se reorganizan automáticamente

### Alternancia de Turnos
- Cada tablero mantiene su propio turno independiente
- Alternancia estricta: X → O → X → O
- El primer movimiento en cada tablero siempre es X

### Detección de Victoria/Empate
- Detección automática de tres en raya (horizontal, vertical, diagonal)
- Detección de empate cuando el tablero está lleno
- Estadísticas individuales por tablero
- Contador global de victorias y empates

## Solución de Problemas

### El juego no compila
- Asegúrate de tener instaladas todas las dependencias
- Verifica que g++ esté instalado: `g++ --version`
- Verifica que ncurses esté instalada: `dpkg -l | grep ncurses`

### Los colores no se muestran correctamente
- Verifica que tu terminal soporte colores
- Prueba con diferentes emuladores de terminal (gnome-terminal, konsole, xterm)

### El ratón no funciona
- El soporte de ratón depende del emulador de terminal
- No todos los terminales soportan eventos de ratón
- Los controles de teclado siempre están disponibles como alternativa

### La interfaz se ve distorsionada
- Asegúrate de que la terminal sea lo suficientemente grande
- Tamaño mínimo recomendado: 80x24 caracteres
- Para múltiples tableros, se recomienda una terminal más grande

## Licencia

Este proyecto es de código abierto y está disponible para uso educativo.

## Autor

Proyecto desarrollado como ejemplo de programación en C++ con ncurses.

## Contribuciones

Las contribuciones son bienvenidas. Por favor, asegúrate de que el código:
- Compile sin warnings
- Siga el estilo del código existente
- Incluya comentarios donde sea necesario
- Funcione correctamente con diferentes configuraciones
