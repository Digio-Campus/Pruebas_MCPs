# Tres en Raya con ncurses

Un juego completo de Tres en Raya implementado en C++ utilizando la librería ncurses para la interfaz en terminal.

## Características

- **Múltiples tableros independientes**: Cada tablero mantiene su propio estado, turnos, victorias y empates.
- **Modos de juego**:
  - 0 jugadores: Todos los movimientos son automáticos y aleatorios.
  - 1 jugador: El jugador controla manualmente tanto X como O, alternando turnos.
  - 2 jugadores: El jugador controla O, la X se genera automáticamente.
- **Interfaz ncurses**: Navegación con teclado y soporte básico para mouse.
- **Menú principal**: Opciones para Jugar, Ajustes, Ayuda y Salir.
- **Ajustes configurables**: Número de jugadores y número de tableros (1-9).
- **Estadísticas**: Muestra victorias, empates por tablero.
- **Soporte para redimensionado**: Se adapta al tamaño de la terminal.

## Requisitos

- Linux
- g++ (compilador C++)
- ncurses (instalar con `sudo apt-get install libncurses5-dev` en Ubuntu/Debian)

## Compilación

```bash
make
```

O alternativamente:

```bash
make all
```

## Ejecución

```bash
make run
```

O directamente:

```bash
./bin/tres_en_raya
```

## Controles

### Menú Principal
- **Flechas arriba/abajo**: Navegar entre opciones
- **Enter**: Seleccionar opción

### Menú de Ajustes
- **Flechas arriba/abajo**: Cambiar entre configuraciones
- **Flechas izquierda/derecha**: Modificar valores
- **Enter**: Guardar y volver

### Juego
- **Flechas**: Mover cursor en el tablero
- **Enter**: Colocar ficha
- **Tab**: Cambiar entre tableros
- **R**: Reiniciar tablero actual
- **ESC**: Volver al menú principal
- **Clic del ratón**: Seleccionar celda (si soportado)

## Estructura del Proyecto

```
tres_en_raya_ncurses_completo/
├── include/           # Archivos de cabecera (.h)
│   ├── Board.h       # Clase para gestionar un tablero
│   ├── Game.h        # Controlador principal del juego
│   ├── Menu.h        # Gestión de menús
│   ├── Settings.h    # Configuración del juego
│   └── UI.h          # Interfaz de usuario ncurses
├── src/              # Archivos fuente (.cpp)
│   ├── Board.cpp
│   ├── Game.cpp
│   ├── Menu.cpp
│   ├── Settings.cpp
│   ├── UI.cpp
│   └── main.cpp
├── obj/              # Archivos objeto (generados)
├── bin/              # Ejecutable (generado)
├── Makefile          # Script de compilación
└── README.md         # Este archivo
```

## Modos de Juego Detallados

### Modo 0 Jugadores
- Los movimientos se generan automáticamente de forma aleatoria.
- Todos los tableros juegan simultáneamente.
- Se reinician automáticamente cuando todos terminan.

### Modo 1 Jugador
- El jugador controla tanto las X como las O.
- Debe alternar turnos manualmente respetando X → O → X → O.
- Cada tablero mantiene su turno independiente.

### Modo 2 Jugadores
- El jugador controla las O.
- Después de colocar una O, la X se genera automáticamente.
- Simula un oponente automático.

## Manejo de Errores

- Verifica que ncurses esté instalado antes de compilar.
- Si hay problemas de compilación, asegúrate de tener g++ y las librerías correctas.
- El programa se adapta dinámicamente al tamaño de la terminal.

## Ejemplos de Uso

1. **Compilar y ejecutar**:
   ```bash
   make run
   ```

2. **Configurar juego**:
   - En el menú principal, seleccionar "Ajustes"
   - Configurar 2 jugadores y 4 tableros
   - Volver al menú y seleccionar "Jugar"

3. **Jugar**:
   - Usar flechas para seleccionar celda
   - Enter para colocar ficha
   - Tab para cambiar tablero
   - R para reiniciar tablero
   - ESC para volver al menú

## Comandos Make

- `make` o `make all`: Compilar el proyecto
- `make clean`: Limpiar archivos compilados
- `make run`: Compilar y ejecutar
- `make help`: Mostrar ayuda

## Notas Técnicas

- Utiliza colores ncurses para mejorar la interfaz (si soportados).
- Implementa hit testing básico para clics del ratón.
- Los tableros se organizan en una cuadrícula que se adapta al número total.
- Cada tablero es completamente independiente en estado y estadísticas.