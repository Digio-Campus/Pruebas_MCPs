# INSTRUCCIONES DE COMPILACIÓN Y EJECUCIÓN

## Requisitos Previos

Antes de compilar, asegúrate de tener instalados:
- **Compilador GCC 7.0 o superior** (con soporte C++17)
- **Librería ncurses** (libncurses5-dev)
- **Make** (utilidad de compilación)

## Instalación de Dependencias

### En Linux (Debian/Ubuntu)
```bash
sudo apt-get update
sudo apt-get install build-essential libncurses5-dev
```

### En Linux (Fedora/RHEL)
```bash
sudo dnf install gcc-c++ ncurses-devel
```

### En Linux (Arch)
```bash
sudo pacman -S base-devel ncurses
```

### En macOS (con Homebrew)
```bash
brew install ncurses
```

## Estructura del Proyecto

```
code/tictactoe-ncurses/
├── include/          # Archivos de cabecera (.h)
│   ├── Board.h      # Clase que representa un tablero individual
│   ├── Game.h       # Controlador principal del juego
│   ├── Menu.h       # Sistema de menús navegables
│   ├── Settings.h   # Configuración del juego
│   └── UI.h         # Interfaz gráfica con ncurses
├── src/             # Implementación (.cpp)
│   ├── Board.cpp
│   ├── Game.cpp
│   ├── Menu.cpp
│   ├── Settings.cpp
│   ├── UI.cpp
│   └── main.cpp     # Punto de entrada del programa
├── tests/           # Tests unitarios
│   └── test_main.cpp
├── Makefile         # Script de compilación automática
└── README.md        # Documentación del proyecto
```

## Compilación

### Opción 1: Desde el directorio del proyecto

```bash
cd code/tictactoe-ncurses
make
```

### Opción 2: Desde el directorio raíz del repositorio

```bash
make -C code/tictactoe-ncurses
```

### Resultado esperado

La compilación debería producir:
- Directorio `obj/`: Archivos objeto compilados
- Directorio `bin/`: Ejecutable `tictactoe` listo para ejecutar
- Mensaje: "Build successful: bin/tictactoe"

## Ejecución

### Opción 1: Usar el comando make

```bash
make run
```

### Opción 2: Ejecutar directamente

```bash
./bin/tictactoe
```

### Opción 3: Con ruta completa

```bash
code/tictactoe-ncurses/bin/tictactoe
```

## Tests Unitarios

Para compilar y ejecutar los tests:

```bash
cd code/tictactoe-ncurses
make test
```

Esto compilará y ejecutará automáticamente todos los tests unitarios. Deberías ver:
```
Running unit tests...
✓ testBoardCreation passed
✓ testBoardMoveX passed
...
✓ All tests passed!
```

## Comandos Make Disponibles

| Comando | Descripción |
|---------|-------------|
| `make` o `make all` | Compila el proyecto |
| `make run` | Compila y ejecuta el juego |
| `make test` | Compila y ejecuta tests unitarios |
| `make clean` | Elimina archivos compilados |
| `make help` | Muestra esta ayuda |

## Limpieza

Para eliminar todos los archivos compilados:

```bash
make clean
```

Esto elimina los directorios `obj/` y `bin/`.

## Ejemplos de Uso Rápido

### Compilar y ejecutar inmediatamente

```bash
cd code/tictactoe-ncurses && make run
```

### Compilar solo (sin ejecutar)

```bash
cd code/tictactoe-ncurses && make
```

### Ver estructura sin compilar

```bash
ls -la code/tictactoe-ncurses/
```

### Ejecutar tests y ver resultados

```bash
cd code/tictactoe-ncurses && make clean && make test
```

## Solución de Problemas

### Error: "command not found: make"
- **Solución**: Instala make
  - Debian/Ubuntu: `sudo apt-get install make`
  - Fedora: `sudo dnf install make`
  - macOS: `xcode-select --install`

### Error: "ncurses.h: No such file or directory"
- **Solución**: Instala la librería de desarrollo de ncurses
  - Debian/Ubuntu: `sudo apt-get install libncurses5-dev`
  - Fedora/RHEL: `sudo dnf install ncurses-devel`
  - macOS: `brew install ncurses`

### Error: "fatal error: 'iostream' file not found"
- **Solución**: Instala el compilador C++
  - Debian/Ubuntu: `sudo apt-get install build-essential`
  - Fedora/RHEL: `sudo dnf install gcc-c++`
  - macOS: `xcode-select --install`

### Advertencia: "Command 'ncurses' not found"
- **Solución**: La librería está instalada pero no en el PATH
  - En algunas instalaciones, necesitas especificar la ruta de ncurses
  - Consulta la documentación de tu distribución

### El programa se ejecuta pero no muestra colores
- **Posible causa**: Terminal antigua sin soporte completo de colores
- **Solución**: El programa funcionará igual sin colores, solo sin visualización de color

### El programa no responde al ratón
- **Posible causa**: Tu emulador de terminal no soporta eventos de ratón
- **Solución**: Utiliza el teclado (flechas y Enter) en su lugar

## Verificación de la Instalación

Para verificar que todo está correctamente instalado:

```bash
# Verificar compilador
g++ --version

# Verificar que ncurses está disponible
pkg-config --cflags --libs ncurses
```

Deberías ver versiones y rutas de bibliotecas.

## Compilación Manual (sin Make)

Si necesitas compilar sin usar make:

```bash
cd code/tictactoe-ncurses

# Crear directorios si no existen
mkdir -p obj bin

# Compilar cada archivo
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/Board.cpp -o obj/Board.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/Settings.cpp -o obj/Settings.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/UI.cpp -o obj/UI.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/Menu.cpp -o obj/Menu.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/Game.cpp -o obj/Game.o
g++ -std=c++17 -Wall -Wextra -O2 -I./include -c src/main.cpp -o obj/main.o

# Enlazar
g++ obj/Board.o obj/Settings.o obj/UI.o obj/Menu.o obj/Game.o obj/main.o -o bin/tictactoe -lncurses

# Ejecutar
./bin/tictactoe
```

## Notas Importantes

- El proyecto usa **C++17**, asegúrate de tener GCC 7.0 o superior
- La compilación es con optimización O2 para mejor rendimiento
- Los flags `-Wall -Wextra` aseguran que se muestren todas las advertencias de compilación
- El proyecto no tiene dependencias externas más allá de ncurses (librería estándar)

## Soporte Técnico

Si encuentras problemas durante la compilación:
1. Verifica que tienes todas las dependencias instaladas
2. Prueba con un `make clean` seguido de `make`
3. Verifica tu versión de GCC con `g++ --version`
4. Comprueba la instalación de ncurses con `pkg-config --list-all | grep ncurses`

---

**¡Listo! Ahora puedes compilar y ejecutar el juego de Tic Tac Toe.**
