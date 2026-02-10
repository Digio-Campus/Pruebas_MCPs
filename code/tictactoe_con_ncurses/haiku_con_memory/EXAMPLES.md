# Ejemplos de Uso - Tic-Tac-Toe ncurses

## Compilación y Ejecución Básica

```bash
# Compilar el proyecto
cd code/tictactoe-ncurses
make

# Ejecutar el juego
make run

# O simplemente:
./tictactoe
```

## Ejemplo 1: Juego Automático (0 Jugadores)

**Objetivo**: Ver cómo se juega automáticamente.

**Pasos**:
1. Ejecuta `./tictactoe`
2. Presiona `1` para "Play"
3. Presiona `2` para "Settings"
4. Presiona `1` para modo "0 Jugadores"
5. Presiona `4` para 4 tableros
6. Presiona `Enter` o `b` para volver
7. Presiona `1` para "Play"
8. Observa cómo se rellenan automáticamente todos los tableros

**Lo que ves**:
- Tableros al 3x3 con X y O alternando
- Al final, cada tablero mostrará X WINS, O WINS, o DRAW
- El juego termina cuando todos los tableros están completos

## Ejemplo 2: Modo Manual de 1 Jugador

**Objetivo**: Controlar ambos lados (X y O) manualmente.

**Pasos**:
1. Ejecuta `./tictactoe`
2. En el menú: `1` → Play
3. En configuración: `2` para 1 Jugador, `2` para 2 tableros
4. En el juego:
   - Muévete con flechas hasta una casilla vacía
   - Presiona SPACE para colocar tu marca
   - En modo 1 jugador, el turno alterna automáticamente: X → O
   - Presiona PgDn para cambiar al siguiente tablero
   - Presiona PgUp para volver al tablero anterior

**Secuencia de turnos esperada**:
```
Tablero 1: Juega X (SPACE) → Ahora es turno de O (SPACE)
Tablero 1: Juega X (SPACE) → Ahora es turno de O (SPACE)
...
```

**Usando ratón**:
- Haz clic directamente en una casilla para jugar
- El turno alternará automáticamente

## Ejemplo 3: Modo 2 Jugadores (Humano vs IA)

**Objetivo**: Jugar contra la IA.

**Pasos**:
1. Ejecuta `./tictactoe`
2. En el menú: `1` → Play
3. En configuración: `3` para 2 Jugadores, `1` para 1 tablero
4. En el juego:
   - **Tú juegas con O**
   - Muévete con flechas y presiona SPACE
   - La IA (X) jugará automáticamente después
   - Intenta ganar con O en 3 movimientos

**Ejemplo de flujo**:
```
Tu turno (O): Colocas O en una casilla
IA turno (X): IA coloca X automáticamente
Tu turno (O): Colocas O en otra casilla
...
Resultado: Tu ganas, IA gana, o Empate
```

## Ejemplo 4: Juego con Múltiples Tableros

**Objetivo**: Jugar simultáneamente en varios tableros independientes.

**Pasos**:
1. Ejecuta `./tictactoe`
2. En configuración: Elige `6` tableros
3. En el juego:
   - PgUp/PgDn cambia de tablero
   - Cada tablero mantiene su propio estado
   - Los turnos en cada tablero son independientes

**Observación importante**:
- El tablero 1 puede estar en turno de X
- El tablero 2 puede estar en turno de O
- El tablero 3 puede estar ya terminado
- Todo simultáneamente y sin interferencias

## Ejemplo 5: Reiniciar un Tablero

**Pasos**:
1. Durante el juego, coloca algunas marcas
2. Presiona `R` para reiniciar el tablero actual
3. El tablero vuelve a estado vacío pero otros tableros no se afectan

## Ejemplo 6: Navegación con Ratón

**Pasos**:
1. Ejecuta `./tictactoe`
2. Haz clic con el ratón en:
   - "1. Play" en el menú
   - Cualquier casilla del tablero para jugar
   - En configuración, podrías naveguar por campos

**Características de ratón**:
- Detecta clics precisos en casillas
- Funciona junto con entrada de teclado
- Utiliza el protocolo ncurses estándar

## Ejemplo 7: Redimensionamiento de Terminal

**Pasos**:
1. Ejecuta `./tictactoe`
2. Inicia un juego con 9 tableros
3. Redimensiona la ventana del terminal (hazla más pequeña)
4. El layout se adaptará automáticamente
5. Presiona cualquier tecla para refrescar la pantalla

## Combinaciones Útiles

### Jugar rápidamente (sin menú)
```bash
# Compila una versión rápida
make clean && make
./tictactoe
# Presiona 1, 1, 1, 1 para llegar rápido al juego
```

### Ver el código durante compilación
```bash
make -B  # Fuerza recompilación de todo
```

### Limpiar y recompilar
```bash
make rebuild
```

## Flujo Típico de Juego

```
┌─────────────────┐
│  MENÚ PRINCIPAL │
└────────┬────────┘
         │
    1. Play Game
    2. Settings  ← Aquí configuras
    3. Help      ← Instrucciones
    4. Quit
         │
         ▼
┌─────────────────┐
│   CONFIGURACIÓN │
├─────────────────┤
│ Modo: [1/2/3]   │
│ Tableros: [1-9] │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   JUEGO ACTIVO  │
├─────────────────┤
│ [Tablero 1]     │
│ [Tablero 2]     │
│ [Tablero 3]     │
│ ...             │
└─────────────────┘
    ▲         ▼
   PgUp     PgDn
   
    Juega con:
    - Flechas + SPACE
    - Ratón (clic directo)
    - R para reiniciar
    - Q para salir
```

## Solución de Problemas

### "ncurses not found"
```bash
sudo apt-get install libncurses-dev  # Debian/Ubuntu
```

### La terminal está corrupta después de salir
```bash
reset  # O escribe: stty sane
```

### El ratón no funciona
- Algunos terminales no soportan mouse events
- Usa teclado alternativemente
- Prueba con xterm o urxvt

### Tableros no se ven bien
- Redimensiona la ventana
- Asegúrate de que tienes al menos 80x24 caracteres
- Presiona una tecla para refrescar

## Casos de Uso Educativos

1. **Aprender C++17**: Usa este proyecto como referencia de OOP
2. **ncurses**: Ve cómo usar la librería para UI en terminal
3. **Algoritmos de IA**: Modifica `makeAIMove()` para mejorar
4. **Programación de Eventos**: Entiende event-driven input
5. **Testing**: Prueba cada modo con diferentes configuraciones

---

¡Disfruta del juego!
