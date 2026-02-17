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
- **Ajustes configurables**: Número de jugadores y número de tableros.
- **Estadísticas**: Muestra victorias, empates por tablero.

## Requisitos

- Linux
- g++
- ncurses (instalar con `sudo apt-get install libncurses5-dev` en Ubuntu)

## Compilación

```bash
make
```

## Ejecución

```bash
make run
# o
./bin/tres_en_raya
```

## Controles

- **Menú**: Flechas arriba/abajo para navegar, Enter para seleccionar.
- **Juego**: Flechas para mover cursor, Enter para colocar ficha, Tab para cambiar tablero, 'r' para reiniciar tablero, ESC para volver al menú.
- **Ajustes/Ayuda**: Seguir instrucciones en pantalla.

## Estructura del Proyecto

- `include/`: Archivos de cabecera (.h)
- `src/`: Archivos fuente (.cpp)
- `bin/`: Ejecutable compilado
- `Makefile`: Para compilación

## Ejemplos de Uso

1. Compilar y ejecutar.
2. En el menú, seleccionar Ajustes para configurar 2 jugadores y 4 tableros.
3. Seleccionar Jugar.
4. Usar flechas para seleccionar celda, Enter para jugar.
5. Cambiar entre tableros con Tab.

## Manejo de Errores

- Verifica que ncurses esté instalado.
- Si hay problemas de compilación, asegúrate de tener g++ y las librerías correctas.

## Soporte para Redimensionado

El programa se adapta al tamaño de la terminal, pero para mejores resultados, mantén una terminal de al menos 80x24.