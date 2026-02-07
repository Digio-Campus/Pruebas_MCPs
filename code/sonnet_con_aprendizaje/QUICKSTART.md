# Guía de Inicio Rápido - TicTacToe Multi-Tablero

## ⚡ Inicio Rápido (3 pasos)

```bash
# 1. Navegar al directorio
cd code/tictactoe

# 2. Compilar
make

# 3. Ejecutar
./tictactoe
```

## 🎮 Primeros Pasos en el Juego

### Al iniciar verás el menú principal:
```
=== TICTACTOE MULTI-TABLERO ===

> Jugar <
  Ajustes
  Ayuda
  Salir
```

### Opción 1: Jugar Inmediatamente (Configuración por Defecto)
- Presiona **ENTER** en "Jugar"
- Jugarás en modo 2 jugadores (tú controlas O, X es automática)
- Con 1 tablero
- Usa **flechas** para mover
- Presiona **ENTER** para jugar

### Opción 2: Configurar Primero
1. Ve a **Ajustes**
2. Cambia el modo de juego:
   - **0 jugadores**: Observa el juego automático
   - **1 jugador**: Controla X y O manualmente
   - **2 jugadores**: Juegas O, X automática
3. Cambia número de tableros (1-12)
4. Presiona ENTER en "Volver"
5. Ve a "Jugar"

## 🕹️ Controles Durante el Juego

### Teclado
| Tecla | Acción |
|-------|--------|
| ← ↑ ↓ → | Mover selección dentro del tablero |
| TAB | Cambiar entre tableros |
| ENTER o ESPACIO | Realizar jugada |
| R | Reiniciar tablero actual |
| ESC o Q | Volver al menú |

### Ratón
- **Clic izquierdo** en cualquier casilla vacía para jugar directamente
- Funciona en todos los tableros simultáneamente

## 📋 Modos de Juego Explicados

### 🤖 0 Jugadores (Automático)
```
┌─────────┐     ┌─────────┐     ┌─────────┐
│ X O X   │     │ O X O   │     │ X X O   │
│ O X O   │ --> │ X O X   │ --> │ O O X   │
│ X O X   │     │ O X X   │     │ X X O   │
└─────────┘     └─────────┘     └─────────┘
   Auto            Auto            Auto
```
- Todos los tableros juegan solos
- Jugadas completamente aleatorias
- Ideal para ver múltiples partidas simultáneas
- Los tableros se reinician automáticamente al terminar

### 👤 1 Jugador (Manual Completo)
```
Tablero 1        Tablero 2        Tablero 3
(Tu turno: X)    (Tu turno: O)    (Tu turno: X)

Tú controlas TODOS los movimientos en TODOS los tableros
Cada tablero mantiene su alternancia X → O → X → O
```
- Controlas tanto X como O
- Debes jugar manualmente en cada tablero
- Cada tablero alterna turnos independientemente
- Ideal para practicar estrategias

### 👥 2 Jugadores (Híbrido)
```
Tú juegas O          →          X responde automático
[ ][ ][ ]                       [X][ ][ ]
[ ][O][ ]            →          [ ][O][ ]
[ ][ ][ ]                       [ ][ ][ ]
```
- Tú siempre juegas con O
- Después de tu movimiento, X se coloca automáticamente
- Alternancia automática: O (tú) → X (auto) → O (tú) → X (auto)
- Ideal para juego casual rápido

## 📊 Interpretando la Pantalla

### Vista del Tablero
```
Tablero 1 [ACTIVO]              <- Tablero seleccionado

[ X ][ O ][ X ]                 <- Estado del tablero
[ O ][ X ][ O ]                    (Flechas para mover)
[   ][ X ][ O ]

 Turno: O                       <- Quién juega ahora
```

### Estadísticas (parte inferior)
```
ESTADISTICAS:
Total - X: 5 | O: 3 | Empates: 2 | Partidas: 10

Modo: 2 jugadores (O manual, X auto)
```

### Múltiples Tableros
```
Tablero 1        Tablero 2        Tablero 3
[ ][ ][ ]        [X][O][ ]        [ ][ ][ ]
[ ][ ][ ]        [ ][X][ ]        [ ][O][ ]
[ ][ ][ ]        [ ][ ][ ]        [ ][ ][ ]

Tablero 4        Tablero 5        Tablero 6
[O][X][ ]        [ ][ ][ ]        [X][X][O]
[X][O][ ]        [ ][ ][ ]        [O][O][X]
[ ][ ][ ]        [ ][ ][ ]        [ ][ ][ ]
```

## 🎯 Ejemplos de Uso

### Ejemplo 1: Partida Rápida
```bash
./tictactoe
# Presiona ENTER en "Jugar"
# Usa flechas para seleccionar casilla
# Presiona ENTER para jugar
# X responde automáticamente
# Repite hasta ganar o empatar
```

### Ejemplo 2: Modo Demostración (0 jugadores, 9 tableros)
```bash
./tictactoe
# ENTER en "Ajustes"
# Cambiar a "0 jugadores"
# Cambiar a "9 tableros"
# ENTER en "Volver"
# ENTER en "Jugar"
# ¡Observa 9 partidas simultáneas!
```

### Ejemplo 3: Práctica Estratégica (1 jugador, 4 tableros)
```bash
./tictactoe
# ENTER en "Ajustes"
# Cambiar a "1 jugador"
# Cambiar a "4 tableros"
# ENTER en "Volver"
# ENTER en "Jugar"
# TAB para cambiar entre tableros
# Juega X y O en cada uno
```

## 🏆 Objetivos y Victoria

### Condición de Victoria
```
Horizontal:  [X][X][X]
             [ ][ ][ ]
             [ ][ ][ ]

Vertical:    [X][ ][ ]
             [X][ ][ ]
             [X][ ][ ]

Diagonal:    [X][ ][ ]
             [ ][X][ ]
             [ ][ ][X]
```

### Empate
```
[X][O][X]
[O][X][O]
[O][X][O]

Tablero lleno, sin ganador
```

### Después de Terminar
- El tablero muestra "X GANA!", "O GANA!" o "EMPATE"
- Las estadísticas se actualizan
- Presiona **R** para reiniciar ese tablero
- O espera a que se reinicie automáticamente (modo 0 jugadores)

## 🐛 Solución de Problemas Comunes

### La pantalla se ve rara
```bash
# Resetear terminal
reset
```

### No puedo hacer clic con el ratón
- Verifica que tu terminal soporte ratón
- Intenta usar solo el teclado (flechas + ENTER)

### El juego se cierra inesperadamente
```bash
# Si la terminal queda rota
reset

# Recompilar y reintentar
make clean
make
./tictactoe
```

### Los colores no se ven
```bash
# Verificar soporte de color
echo $TERM

# Si no es xterm-256color:
export TERM=xterm-256color
./tictactoe
```

## 📚 Más Información

- **README.md**: Documentación completa del proyecto
- **DEVELOPMENT.md**: Guía para extender el proyecto
- **test.sh**: Script de verificación automática

## 💡 Consejos

1. **Modo 0 jugadores**: Perfecto para tenerlo de fondo mientras trabajas
2. **Modo 1 jugador**: Excelente para entender estrategias de TicTacToe
3. **Modo 2 jugadores**: Diversión rápida sin pensar mucho
4. **Múltiples tableros**: Aumenta la complejidad y diversión
5. **Usa el ratón**: Es la forma más rápida de jugar
6. **TAB es tu amigo**: Cambia rápidamente entre tableros

## 🚀 Comandos Make Útiles

```bash
make              # Compilar
make run          # Compilar y ejecutar
make clean        # Limpiar archivos
make rebuild      # Recompilar todo
make help         # Ver ayuda del Makefile
./test.sh         # Verificar todo antes de jugar
```

---

**¡Diviértete jugando!** 🎮
