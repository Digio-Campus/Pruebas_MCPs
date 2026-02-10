# Tres en Raya (ncurses) — Multi-tablero

Proyecto en C++ que implementa Tres en Raya con interfaz en terminal usando **ncurses**.

## Requisitos
- Linux
- g++
- ncurses (normalmente paquete `libncurses-dev`)

## Compilación
Desde la raíz del repositorio:

```bash
make -C code
```

## Ejecución
```bash
./code/bin/tres_en_raya
```

## Controles (resumen)
- **Menús**: Flechas ↑/↓ y **Enter**
- **Juego**:
  - Mover selección de casilla: flechas
  - Cambiar de tablero: **Tab** (siguiente) / **Shift+Tab** (anterior)
  - Colocar ficha: **Enter** o **Espacio**
  - Reiniciar tablero seleccionado (cuando haya terminado): **r**
  - Volver al menú: **q**

## Modos (Ajustes)
- **0 jugadores**: todos los tableros se juegan solos con jugadas aleatorias; al terminar un tablero se reinicia automáticamente y se acumulan estadísticas.
- **1 jugador**: el usuario juega **X y O** manualmente, manteniendo la alternancia **X→O→X→...** en *cada* tablero.
- **2 jugadores**: el usuario juega **O**; **X** se genera automáticamente cuando le toca a X en cada tablero.

## Notas
- Cada tablero es totalmente independiente (estado, turno, victorias y empates).
- La pantalla se adapta al tamaño de la terminal; si no hay espacio, se muestra un aviso.
