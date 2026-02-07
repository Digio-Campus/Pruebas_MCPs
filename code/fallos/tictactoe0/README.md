# Tictactoe con ncurses

Un juego completo de Tictactoe implementado en C++ con interfaz de terminal usando la librería ncurses.

## Características

- **Interfaz interactiva** con ncurses y soporte para ratón
- **Múltiples modos de juego**:
  - 0 jugadores: Partidas automáticas continuas
  - 1 jugador: Humano (X) vs Automático (O)
  - 2 jugadores: Humano (O) + Automático (X)
- **Tableros simultáneos**: Configurable de 1 a 9 tableros en pantalla
- **Sistema de puntuación** acumulativa
- **Menús navegables** con teclado
- **Detección automática** de victorias y empates
- **Soporte para redimensionado** de ventana
- **Colores** para mejor visualización

## Requisitos

- Sistema operativo: Linux
- Compilador: g++ con soporte C++14
- Librería: ncurses
- Librería: pthread (usualmente incluida)

### Instalación de dependencias

En Ubuntu/Debian:
```bash
sudo apt-get update
sudo apt-get install build-essential libncurses5-dev libncursesw5-dev
```

En Fedora/RHEL:
```bash
sudo dnf install gcc-c++ ncurses-devel
```

En Arch Linux:
```bash
sudo pacman -S base-devel ncurses
```

## Compilación

```bash
cd code/tictactoe
make
```

Para limpiar los archivos generados:
```bash
make clean
```

## Ejecución

```bash
./tictactoe
```

O directamente:
```bash
make run
```

## Controles

### Menús
- **Flechas arriba/abajo**: Navegar opciones
- **Flechas izquierda/derecha**: Ajustar valores en configuración
- **Enter**: Seleccionar opción
- **ESC**: Volver al menú anterior

### Juego
- **Ratón**: Clic en las casillas para jugar
- **Q o ESC**: Salir del juego y volver al menú
- El juego detecta automáticamente el redimensionado de la terminal

## Modos de Juego

### 0 Jugadores (Automático)
Ambos jugadores son automáticos. Las partidas se juegan continuamente de forma automática, ideal para observar o como screensaver.

### 1 Jugador (Humano vs Automático)
- Tú juegas como **X**
- El oponente automático juega como **O**
- Haz clic en las casillas para realizar tus movimientos

### 2 Jugadores (Humano + Automático)
- Tú juegas como **O**
- **X** se genera automáticamente después de cada turno
- Modo especial donde controlas solo las O

## Estructura del Proyecto

```
tictactoe/
├── include/          # Archivos de cabecera (.h)
│   ├── board.h       # Lógica del tablero
│   ├── player.h      # Gestión de jugadores
│   ├── game.h        # Control del juego
│   ├── menu.h        # Sistema de menús
│   └── ui.h          # Interfaz con ncurses
├── src/              # Implementaciones (.cpp)
│   ├── board.cpp
│   ├── player.cpp
│   ├── game.cpp
│   ├── menu.cpp
│   ├── ui.cpp
│   └── main.cpp      # Punto de entrada
├── Makefile          # Script de compilación
└── README.md         # Este archivo
```

## Características Técnicas

- **Modularización**: Código separado en clases especializadas
- **Manejo de errores**: Try-catch para excepciones
- **Memoria dinámica**: Uso de smart pointers (unique_ptr)
- **Multithreading**: Soporte con pthread para delays
- **Responsive**: Adaptación automática al tamaño de terminal

## Ejemplos de Uso

### Ejemplo 1: Juego rápido de 1 jugador
1. Ejecuta `./tictactoe`
2. Selecciona "JUGAR" (debe estar en modo 1 jugador por defecto)
3. Haz clic en las casillas para jugar como X
4. Presiona Q o ESC para salir

### Ejemplo 2: Múltiples tableros automáticos
1. Ejecuta `./tictactoe`
2. Selecciona "AJUSTES"
3. Cambia "Número de jugadores" a 0 (con flechas izquierda/derecha)
4. Cambia "Número de tableros" a 4 o más
5. Vuelve y selecciona "JUGAR"
6. Observa las partidas automáticas en múltiples tableros

### Ejemplo 3: Modo 2 jugadores
1. Ve a "AJUSTES"
2. Cambia "Número de jugadores" a 2
3. Inicia el juego
4. Haz clic para colocar O, y X aparecerá automáticamente

## Solución de Problemas

### Error: "ncurses.h: No such file or directory"
Instala la librería ncurses-dev según tu distribución (ver sección de Requisitos).

### El ratón no funciona
Asegúrate de que tu emulador de terminal soporte eventos de ratón. La mayoría de emuladores modernos (gnome-terminal, konsole, xterm) lo soportan.

### La pantalla se ve mal
Redimensiona la terminal a un tamaño mayor. Se recomienda al menos 80x24 caracteres.

### Problemas de compilación
Verifica que tengas g++ con soporte C++14:
```bash
g++ --version
```

## Autor

Proyecto desarrollado como ejemplo de implementación de juegos en terminal con C++ y ncurses.

## Licencia

Este proyecto es de código abierto y puede ser usado libremente con fines educativos.
