# TicTacToe Multi-Tablero con ncurses

Juego de TicTacToe completo implementado en C++ con soporte para múltiples tableros simultáneos, interfaz de terminal con ncurses, y control por teclado y ratón.

## Características

### 🎮 Modos de Juego

- **0 Jugadores (Automático)**: Todos los tableros juegan automáticamente con jugadas aleatorias
- **1 Jugador (Manual)**: El jugador controla tanto X como O, jugando manualmente en todos los tableros con alternancia estricta X → O → X → O
- **2 Jugadores**: El jugador controla O, y X se genera automáticamente después de cada turno

### 🎯 Funcionalidades

- Múltiples tableros simultáneos (1-12 tableros configurables)
- Cada tablero es totalmente independiente con su propio estado y turnos
- Interfaz gráfica en terminal usando ncurses
- Soporte completo de ratón (clic para seleccionar y jugar)
- Navegación con teclado (flechas, TAB, ENTER)
- Detección automática de victorias, empates y reinicios
- Estadísticas separadas por tablero
- Adaptación automática al tamaño de la terminal
- Colores para mejor visualización

## Requisitos

### Sistema Operativo
- Linux (cualquier distribución)

### Dependencias
- g++ con soporte C++11 o superior
- biblioteca ncurses

### Instalación de dependencias

**Ubuntu/Debian:**
```bash
sudo apt-get install g++ libncurses5-dev libncursesw5-dev
```

**Fedora/RedHat:**
```bash
sudo dnf install gcc-c++ ncurses-devel
```

**Arch Linux:**
```bash
sudo pacman -S gcc ncurses
```

## Compilación

### Compilar el proyecto
```bash
make
```

### Compilar y ejecutar
```bash
make run
```

### Limpiar archivos compilados
```bash
make clean
```

### Recompilar desde cero
```bash
make rebuild
```

### Ver ayuda del Makefile
```bash
make help
```

## Ejecución

Después de compilar, ejecuta:
```bash
./tictactoe
```

## Controles

### Menú Principal
- **Flechas arriba/abajo**: Navegar opciones
- **ENTER**: Seleccionar opción
- **Clic del ratón**: Seleccionar directamente
- **Q**: Salir

### Durante el Juego
- **Flechas**: Mover selección dentro del tablero
- **TAB**: Cambiar entre tableros
- **ENTER o ESPACIO**: Realizar jugada
- **R**: Reiniciar tablero actual
- **ESC o Q**: Volver al menú principal
- **Clic del ratón**: Seleccionar y jugar directamente en cualquier casilla

### Menú de Ajustes
- **Flechas arriba/abajo**: Navegar opciones
- **Flechas izquierda/derecha**: Cambiar valores
- **ENTER**: Aplicar cambios y volver
- **ESC**: Cancelar y volver

## Estructura del Proyecto

```
tictactoe/
├── main.cpp           # Punto de entrada del programa
├── Board.h            # Definición de la clase Board
├── Board.cpp          # Implementación del tablero individual
├── GameManager.h      # Definición del gestor de juego
├── GameManager.cpp    # Gestión de múltiples tableros y modos
├── UI.h               # Definición de la interfaz de usuario
├── UI.cpp             # Implementación de la UI con ncurses
├── Makefile           # Sistema de compilación
└── README.md          # Este archivo
```

## Reglas del Juego

1. **Objetivo**: Conseguir 3 símbolos iguales en línea (horizontal, vertical o diagonal)

2. **Turnos**: 
   - Cada tablero alterna estrictamente X → O → X → O
   - Los tableros son independientes entre sí

3. **Victoria**: 
   - El primero en conseguir 3 en línea gana
   - Se detecta automáticamente

4. **Empate**: 
   - Si se llena el tablero sin ganador, es empate
   - El tablero se reinicia automáticamente

5. **Estadísticas**: 
   - Se registran victorias de X, O, empates y partidas jugadas
   - Las estadísticas persisten durante la sesión

## Ejemplos de Uso

### Ejemplo 1: Juego rápido con configuración por defecto
```bash
make run
# Selecciona "Jugar" en el menú
# Usa las flechas y ENTER para jugar
```

### Ejemplo 2: Configurar 4 tableros en modo 1 jugador
```bash
./tictactoe
# Selecciona "Ajustes"
# Cambia a "1 jugador (Manual)"
# Cambia "Numero de tableros" a 4
# Presiona ENTER en "Volver al menu principal"
# Selecciona "Jugar"
```

### Ejemplo 3: Modo automático con 6 tableros
```bash
./tictactoe
# Selecciona "Ajustes"
# Cambia a "0 jugadores (Auto)"
# Cambia "Numero de tableros" a 6
# Presiona ENTER en "Volver al menu principal"
# Selecciona "Jugar"
# Observa cómo los tableros juegan automáticamente
```

## Manejo de Errores

El programa incluye:
- Validación de entrada del usuario
- Verificación de límites de tablero
- Manejo de terminal demasiado pequeña
- Verificación de movimientos válidos
- Limpieza apropiada de recursos ncurses

## Características Adicionales

- ✅ **Modularización**: Código separado en archivos .h y .cpp
- ✅ **Comentarios**: Código bien comentado y documentado
- ✅ **Manejo de errores**: Validaciones en todas las operaciones críticas
- ✅ **Redimensionado**: Adaptación al cambio de tamaño de terminal
- ✅ **Soporte de ratón**: Activación y manejo completo del ratón con ncurses
- ✅ **Colores**: Uso de colores para mejor experiencia visual
- ✅ **Múltiples tableros**: Soporte de 1 a 12 tableros simultáneos

## Solución de Problemas

### Error: "ncurses.h: No such file or directory"
```bash
# Instala la biblioteca ncurses
sudo apt-get install libncurses5-dev libncursesw5-dev
```

### Error: "undefined reference to 'initscr'"
```bash
# Asegúrate de que el Makefile incluye -lncurses
# Recompila desde cero
make clean
make
```

### La terminal se ve mal después de ejecutar
```bash
# Si el programa termina abruptamente, resetea la terminal
reset
```

### Los colores no se ven
```bash
# Verifica que tu terminal soporte colores
echo $TERM
# Debe mostrar algo como "xterm-256color"
```

## Licencia

Este proyecto es de código abierto y está disponible para uso educativo.

## Autor

Desarrollado como proyecto educativo de programación en C++ con ncurses.

## Créditos

- Biblioteca ncurses para la interfaz de terminal
- Estándar C++11 para características modernas de C++
