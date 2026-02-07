# Tres en Raya (Tic-Tac-Toe)

Implementación del clásico juego de tres en raya en C++.

## Características

- **Arquitectura limpia**: Clase única `TresEnRaya` con encapsulación completa
- **Validación robusta**: Manejo de entradas inválidas con `cin.clear()` y `cin.ignore()`
- **UX mejorada**: Coordenadas visibles en cada renderizado del tablero
- **Verificación eficiente**: Combinación de filas y columnas en un solo bucle O(n)
- **Detección correcta**: Verifica ganador antes de tablero lleno
- **Juego recursivo**: Opción de jugar múltiples partidas

## Compilación

```bash
make
```

O manualmente:
```bash
g++ -Wall -Wextra -std=c++11 -o tres_en_raya tres_en_raya.cpp
```

## Ejecución

```bash
./tres_en_raya
```

O usando el Makefile:
```bash
make run
```

## Cómo jugar

1. El juego muestra un tablero 3x3 con coordenadas (0-2)
2. Los jugadores alternan turnos (X comienza)
3. Ingresa la fila y columna donde quieres colocar tu marca
4. Gana el primer jugador que consiga tres en línea (horizontal, vertical o diagonal)
5. Si el tablero se llena sin ganador, es empate

## Limpieza

```bash
make clean
```

## Decisiones de Diseño

- **Vector 2D**: Simplifica acceso y manipulación del tablero
- **Método independiente de validación**: `movimientoValido()` reutilizable
- **Manejo robusto de errores**: Previene bucles infinitos con entradas no numéricas
- **Compilación con warnings**: Flags `-Wall -Wextra` para detectar problemas
