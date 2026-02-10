# Instrucciones de Uso - Tic Tac Toe en C++

## Inicio Rápido

### 1. Compilación
```bash
cd tictactoe
make
```

### 2. Ejecución
```bash
./tictactoe
```

### 3. Seleccionar Opción del Menú
Use números 1-4 para navegar el menú principal.

## Menús del Programa

### Menú Principal

```
=== TIC TAC TOE ===

1. JUGAR
2. AJUSTES
3. AYUDA
4. SALIR

Selecciona una opción (1-4):
```

**Opciones:**
- **1 - JUGAR**: Inicia una nueva partida
- **2 - AJUSTES**: Muestra opciones para configurar el juego
- **3 - AYUDA**: Muestra controles e instrucciones
- **4 - SALIR**: Cierra el programa

### Menú de Ajustes - Jugadores

```
=== AJUSTES ===

Número de Jugadores:
1. 0 jugadores (Automático aleatorio)
2. 1 jugador (Control manual X y O)
3. 2 jugadores (Jugador=O, IA=X)

Selecciona (1-3):
```

**Descripción de Modos:**

#### 0 Jugadores (Automático)
- Los tableros se rellenan automáticamente
- Las jugadas son completamente aleatorias
- No requiere entrada del usuario
- Las partidas avanzan automáticamente
- **Ideal para**: Ver el juego en acción sin intervención

#### 1 Jugador (Manual)
- Controlas AMBOS jugadores (X y O)
- Haces todos los movimientos manualmente
- Respetas el turno alternativo: X → O → X → O
- Cada tablero mantiene su propio turno
- **Ideal para**: Jugar contra ti mismo o ver diferentes estrategias

#### 2 Jugadores (Con IA)
- Tú eres O (símbolo azul)
- La IA es X (símbolo verde)
- Haces movimientos, la IA responde automáticamente
- El juego alterna X → O → X → O
- **Ideal para**: Jugar contra la máquina

### Menú de Ajustes - Tableros

```
=== AJUSTES ===

Número de Tableros:
1. 1 tablero
2. 2 tableros (1x2)
3. 4 tableros (2x2)
4. 6 tableros (2x3)
5. 9 tableros (3x3)

Selecciona (1-5):
```

**Descripción de Configuraciones:**

| Opción | Tableros | Layout | Descripción |
|--------|----------|--------|-------------|
| 1 | 1 | 1x1 | Un tablero único |
| 2 | 2 | 1x2 | Dos tableros lado a lado |
| 3 | 4 | 2x2 | Cuatro tableros en cuadrícula |
| 4 | 6 | 2x3 | Seis tableros (dos filas, tres columnas) |
| 5 | 9 | 3x3 | Nueve tableros en matriz |

**Características:**
- Cada tablero es **completamente independiente**
- Tienen **su propio turno** (X → O)
- Mantienen **sus propias estadísticas** (victorias, empates)
- Se **adaptan al tamaño** de la terminal

## Controles de Juego

### Movimiento del Cursor

| Control | Acción |
|---------|--------|
| **↑** Arrow Up | Mover cursor hacia arriba |
| **↓** Arrow Down | Mover cursor hacia abajo |
| **←** Arrow Left | Mover cursor hacia la izquierda |
| **→** Arrow Right | Mover cursor hacia la derecha |
| **W** o **w** | Mover cursor hacia arriba (alternativa) |
| **S** o **s** | Mover cursor hacia abajo (alternativa) |
| **A** o **a** | Mover cursor hacia la izquierda (alternativa) |
| **D** o **d** | Mover cursor hacia la derecha (alternativa) |

### Acciones de Juego

| Control | Acción |
|---------|--------|
| **ENTER** | Colocar marca en la posición del cursor |
| **ESPACIO** | Colocar marca en la posición del cursor (alternativa) |
| **TAB** | Cambiar al siguiente tablero |
| **R** o **r** | Reiniciar el tablero actual (limpia el tablero) |
| **ESC** | Volver al menú principal |

## Ejemplos de Uso

### Ejemplo 1: Juego Automático (0 Jugadores)

```
1. En el menú principal: presiona "1" (JUGAR)
2. En ajustes de jugadores: presiona "1" (0 jugadores)
3. En ajustes de tableros: presiona "5" (9 tableros 3x3)
4. El programa llena automáticamente los 9 tableros
5. Las puntuaciones se actualizan en tiempo real
6. Presiona ESC cuando termines para volver al menú
```

**Resultado esperado:**
- Verás 9 tableros llenarse automáticamente
- Cada tablero mostrará ganador o empate
- Las estadísticas se actualizarán: X:1 O:1 E:2 (ejemplo)

### Ejemplo 2: Juego Manual (1 Jugador)

```
1. En el menú principal: presiona "1" (JUGAR)
2. En ajustes de jugadores: presiona "2" (1 jugador)
3. En ajustes de tableros: presiona "1" (1 tablero)
4. Selecciona una casilla con flechas/WASD
5. Presiona ENTER para colocar X
6. Mueve cursor nuevamente y presiona ENTER para colocar O
7. Alterna X → O → X → O hasta terminar
8. Presiona R para reiniciar o ESC para volver al menú
```

**Flujo de entrada:**
```
Cursor comienza en centro del tablero
┌─────────┐
│ . . . │  ← Cursor aquí al inicio
│ . X . │  ← Presionas ENTER, coloca X
│ . . . │
└─────────┘

Turno de O
│ . . . │  
│ . X O │  ← Mueves y presionas ENTER para O
│ . . . │
└─────────┘

Turno de X nuevamente... (continúa)
```

### Ejemplo 3: Juego vs IA (2 Jugadores)

```
1. En el menú principal: presiona "1" (JUGAR)
2. En ajustes de jugadores: presiona "3" (2 jugadores)
3. En ajustes de tableros: presiona "2" (2 tableros)
4. Verás dos tableros independientes
5. Haz tu movimiento (O - azul)
6. La IA responde automáticamente (X - verde)
7. Continúa alternando
8. Usa TAB para cambiar entre tableros
```

**Estructura visual:**
```
Board 1                      Board 2
┌─────────┐                 ┌─────────┐
│ . . . │                 │ X . . │
│ . X . │                 │ . O . │
│ . O . │  ← Tu movimiento │ . . . │  ← IA responde aquí
└─────────┘                 └─────────┘
X:0 O:0 E:0                 X:0 O:0 E:0

Presiona TAB para cambiar entre tableros
```

### Ejemplo 4: Múltiples Tableros (4 Tableros 2x2)

```
1. Selecciona: 1 jugador, 4 tableros (2x2)
2. Verás layout:

Board 1      Board 2
[   ]        [   ]
Board 3      Board 4
[   ]        [   ]

3. Tablero activo: Board 1 (por defecto)
4. Haz movimientos en Board 1
5. Presiona TAB: cambias a Board 2
6. Presiona TAB: cambias a Board 3
7. Presiona TAB: cambias a Board 4
8. Presiona TAB: vuelves a Board 1
```

## Barra de Estado

En la parte inferior verás:
```
Board: 1 | Cursor: (1,1) | TAB: cambiar | R: reiniciar | ESC: menú
```

**Información:**
- **Board: 1** - Tablero actualmente seleccionado
- **Cursor: (1,1)** - Posición del cursor (fila, columna) 0-2
- Recordatorio de controles disponibles

## Estadísticas y Puntuaciones

Cada tablero muestra:
```
Board 1
┌─────────┐
│ X X O │
│ O X . │
│ O . . │
└─────────┘
X:2 O:1 E:0
```

**Significado:**
- **X:2** - Victorias de X en este tablero
- **O:1** - Victorias de O en este tablero
- **E:0** - Empates en este tablero

Las estadísticas se acumulan en cada tablero de forma independiente.

## Menú de Ayuda

Accesible desde el menú principal (opción 3).

Muestra:
- Controles (flechas, ENTER, TAB, etc.)
- Descripción de modos de juego
- Información sobre independencia de tableros
- Presiona cualquier tecla para volver

## Reinicio de Tableros

Hay dos formas de reiniciar:

### Reinicio Individual (Tecla R)
```
Presiona R durante el juego
↓
El tablero actual se limpia
↓
Todas las marcas desaparecen
↓
Turno vuelve a X
↓
Las estadísticas se mantienen (no se resetean)
```

### Reinicio Completo
```
ESC → Volver a menú → Seleccionar JUGAR nuevamente
↓
Se reinician TODOS los tableros
↓
Se pueden cambiar los ajustes (jugadores, número tableros)
↓
Las estadísticas anteriores se pierden
```

## Reglas del Juego

1. **Turnos Alternos**: X siempre comienza, luego O, luego X, etc.
2. **3 en Línea**: Gana quien consiga 3 marcas alineadas (fila, columna o diagonal)
3. **Empate**: Si el tablero se llena sin ganador, es empate
4. **Independencia**: Cada tablero tiene su propio turno y estado
5. **Sin Compartir**: Las victorias de un tablero no afectan a otros

### Ejemplo de Victoria

```
Columna           Fila            Diagonal
┌─────────┐   ┌─────────┐   ┌─────────┐
│ X . . │   │ X X X │   │ X O . │
│ X O . │   │ O . . │   │ O X . │
│ X O . │   │ . O . │   │ . O X │
└─────────┘   └─────────┘   └─────────┘
¡X GANA!      ¡X GANA!      ¡X GANA!
```

### Ejemplo de Empate

```
┌─────────┐
│ X O X │
│ X O O │
│ O X X │
└─────────┘
¡EMPATE! (Tablero lleno, sin ganador)
```

## Solución de Problemas en Juego

### La terminal se ve extraña
**Solución**: Presiona Ctrl+L o redimensiona la ventana

### El cursor está en el lugar incorrecto
**Solución**: Presiona flechas o WASD para reposicionarlo

### No puedo hacer un movimiento
**Probable causa**: 
- La casilla ya está ocupada (intenta otra)
- Es el turno de otro jugador (en modo manual, alterna)

### Quiero volver al menú
**Solución**: Presiona ESC

### Quiero reiniciar solo un tablero
**Solución**: Presiona R mientras ese tablero está activo

### Las estadísticas no se cuentan
**Solución**: Las estadísticas se actualzan automáticamente cuando:
- X gana: X:+1
- O gana: O:+1
- Empate: E:+1

## Tips y Trucos

1. **Cambio Rápido**: Presiona TAB varias veces para rotar tableros
2. **Movimiento Rápido**: Usa WASD es más rápido que flechas
3. **Reset Rápido**: R para limpiar un tablero, ESC+JUGAR para todo
4. **Modo Automático**: Perfecto para ver patrones de juego
5. **Práctica**: 1 Jugador es ideal para practicar estrategias
6. **Desafío**: 2 Jugadores vs IA para un desafío

## Atajos Avanzados

| Combinación | Efecto |
|-------------|--------|
| ESC | Retorna rápido al menú |
| TAB (múltiple) | Cicla por todos los tableros |
| R + ESC | Reset completo del juego |
| ENTER (vacía) | Intenta colocar en casilla vacía |
| ENTER (ocupada) | No hace nada (sin efecto) |

## Salida del Programa

Para salir completamente:
1. Presiona ESC (vuelves al menú)
2. Presiona 4 (opción SALIR)
3. O presiona Ctrl+C si algo falla

Después verás:
```
¡Hasta luego!
```

## Requisitos Mínimos de Terminal

- **Ancho**: 60 caracteres mínimo
- **Alto**: 20 líneas mínimo
- **Color**: Soporta colores (recomendado)
- **Unicode**: Soporta caracteres especiales (recomendado)

Si la terminal es muy pequeña, algunos tableros podrían no caber correctamente.
