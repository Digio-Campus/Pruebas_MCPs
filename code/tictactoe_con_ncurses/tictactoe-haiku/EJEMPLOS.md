# EJEMPLOS DE USO

Este documento proporciona ejemplos prácticos de cómo compilar y ejecutar el juego en diferentes escenarios.

## Ejemplo 1: Uso Básico

### Compilar y ejecutar el juego

```bash
cd code/tictactoe-ncurses
make run
```

**Resultado esperado:**
- Se compila el proyecto (si no está compilado)
- Se inicia el menú principal de Tic Tac Toe
- Ves 4 opciones: Play, Settings, Help, Exit

### Interacción:**
- Presiona **↓** para seleccionar "Play"
- Presiona **Enter** para comenzar
- Juega con los parámetros por defecto (2 jugadores, 1 tablero)

---

## Ejemplo 2: Compilar sin Ejecutar

Si solo quieres compilar sin ejecutar inmediatamente:

```bash
cd code/tictactoe-ncurses
make
```

**Resultado:**
- Se crea el ejecutable en `bin/tictactoe`
- Puedes ejecutarlo posteriormente con:

```bash
./bin/tictactoe
```

---

## Ejemplo 3: Ejecutar con Diferentes Modos de Juego

### Modo 0: Automático (CPU vs CPU)

```bash
cd code/tictactoe-ncurses && ./bin/tictactoe
```

1. Selecciona **Settings**
2. Establece "Number of Players" a **0**
3. Presiona **Enter**
4. Selecciona **Play**
5. **Observa** cómo las CPUs juegan automáticamente

---

### Modo 1: Manual (1 Jugador controla todo)

1. En Settings: "Number of Players" = **1**
2. Presiona **Enter** y ve a **Play**
3. **Tú controlas** tanto X como O
4. Usa flechas para mover el cursor
5. Presiona **Enter** para colocar símbolo

---

### Modo 2: vs CPU (2 Jugadores)

1. En Settings: "Number of Players" = **2** (por defecto)
2. Ve a **Play**
3. **Eres O** (cian), **CPU es X** (rojo)
4. Haz tu movimiento, la CPU responde automáticamente

---

## Ejemplo 4: Jugar con Múltiples Tableros

### Configurar 4 tableros

```bash
./bin/tictactoe
```

1. Abre **Settings**
2. Usa **←→** para cambiar "Number of Boards" a **4**
3. Presiona **Enter** y luego **Play**
4. **Verás 4 tableros** en la pantalla simultáneamente
5. Usa:
   - **Tab** para cambiar entre tableros
   - **1-4** para saltar directamente a un tablero
   - **↑↓←→** para mover dentro del tablero actual

---

## Ejemplo 5: Probar con 9 Tableros (Máximo)

```bash
./bin/tictactoe
```

1. **Settings** → "Number of Boards" = **9**
2. **Play**
3. Observa todos los 9 tableros simultáneamente (3x3 grid)
4. La ventana debe tener al menos 100 caracteres de ancho

---

## Ejemplo 6: Ejecutar Tests Unitarios

Para verificar que todo funciona correctamente:

```bash
cd code/tictactoe-ncurses
make test
```

**Salida esperada:**
```
Running unit tests...
✓ testBoardCreation passed
✓ testBoardMoveX passed
✓ testBoardMoveInvalidCell passed
✓ testBoardWinRow passed
✓ testBoardWinDiagonal passed
✓ testBoardDraw passed
✓ testBoardReset passed
✓ testBoardAvailableMoves passed
✓ testBoardStats passed
✓ testSettingsCreation passed
✓ testSettingsSetPlayers passed
✓ testSettingsSetBoards passed
✓ testSettingsValidation passed

✓ All tests passed!
```

---

## Ejemplo 7: Limpiar y Recompilar

Si quieres empezar desde cero:

```bash
cd code/tictactoe-ncurses
make clean     # Elimina archivos compilados
make           # Recompila todo
make run       # Ejecuta el juego
```

---

## Ejemplo 8: Compilación Manual (sin Make)

Si por alguna razón Make no funciona:

```bash
cd code/tictactoe-ncurses
mkdir -p obj bin

# Compilar cada archivo fuente
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/Board.cpp -o obj/Board.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/Settings.cpp -o obj/Settings.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/UI.cpp -o obj/UI.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/Menu.cpp -o obj/Menu.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/Game.cpp -o obj/Game.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/main.cpp -o obj/main.o

# Enlazar todo
g++ obj/Board.o obj/Settings.o obj/UI.o obj/Menu.o obj/Game.o obj/main.o -o bin/tictactoe -lncurses

# Ejecutar
./bin/tictactoe
```

---

## Ejemplo 9: Script de Compilación Automática

Crear un archivo `compile.sh` para automatizar el proceso:

```bash
#!/bin/bash

cd code/tictactoe-ncurses

# Limpiar compilación anterior
make clean

# Compilar
make

# Verifica si la compilación fue exitosa
if [ -f "bin/tictactoe" ]; then
    echo "✓ Compilación exitosa"
    echo "Iniciando juego..."
    ./bin/tictactoe
else
    echo "✗ Error en la compilación"
    exit 1
fi
```

**Uso:**
```bash
chmod +x compile.sh
./compile.sh
```

---

## Ejemplo 10: Debugging (Si Algo Falla)

```bash
cd code/tictactoe-ncurses

# Ver estructura del proyecto
ls -la
ls -la include/
ls -la src/
ls -la tests/

# Ver si el Makefile está bien
cat Makefile

# Ver si ncurses está instalado
pkg-config --list-all | grep ncurses

# Ver versión del compilador
g++ --version

# Compilar con información de depuración
g++ -std=c++17 -Wall -Wextra -g -I./include -c src/Board.cpp -o obj/Board.o
```

---

## Ejemplo 11: Jugabilidad Rápida - Una Línea de Comando

Para los impacientes:

```bash
cd code/tictactoe-ncurses && make clean && make run
```

---

## Ejemplo 12: Ver Ayuda Dentro del Juego

Una vez en el juego:
1. Presiona **H** en cualquier momento para ver la ayuda
2. O selecciona **Help** en el menú principal

---

## Ejemplo 13: Ajustes Recomendados

### Para Principiantes
- **Number of Players:** 2 (Versus CPU)
- **Number of Boards:** 1 (Un tablero simple)

### Para Jugadores Intermedios
- **Number of Players:** 1 (Control manual)
- **Number of Boards:** 2-3 (Múltiples desafíos)

### Para Expertos
- **Number of Players:** 0 (Ver estrategias de CPU)
- **Number of Boards:** 9 (Tableros simultáneos)

---

## Ejemplo 14: Terminar el Juego

Dentro del juego:
- Presiona **Q** en cualquier momento para volver al menú
- En el menú principal, selecciona **Exit** para salir completamente

---

## Notas Importantes

- La **terminal debe tener al menos 80x24 caracteres**
- Con múltiples tableros (4+), necesitas al menos 100+ caracteres de ancho
- Los **colores requieren un terminal compatible** con colores ANSI
- El **ratón** funciona en la mayoría de emuladores de terminal modernos
- Si el **ratón no funciona**, usa siempre el **teclado** (flechas y Enter)

---

## Solución de Problemas Comunes

### "Terminal is too small!"
→ Expande tu ventana de terminal a 80x24 o mayor

### "ncurses.h: No such file"
→ `sudo apt-get install libncurses5-dev`

### El programa no se inicia
→ Verifica que compilaste exitosamente: `ls -la bin/tictactoe`

### Sin colores
→ Cambia a un terminal moderno (GNOME Terminal, Konsole, iTerm2, etc.)

---

**¡Diviértete jugando!**
