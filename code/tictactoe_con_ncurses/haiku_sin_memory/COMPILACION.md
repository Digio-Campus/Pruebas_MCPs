# Guía de Compilación - Tic Tac Toe en C++ con ncurses

## Requisitos Previos

### Linux (Ubuntu/Debian)
```bash
sudo apt-get update
sudo apt-get install build-essential ncurses-dev
```

Verificar instalación:
```bash
gcc --version    # Verificar compilador C
g++ --version    # Verificar compilador C++
```

### Linux (Fedora/RHEL/CentOS)
```bash
sudo dnf install gcc gcc-c++ ncurses-devel
```

### macOS
```bash
# Instalar Homebrew si no lo tienes
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Instalar dependencias
brew install ncurses
```

### Instalación Manual de ncurses (si es necesario)
```bash
# Descargar
wget https://ftp.gnu.org/pub/gnu/ncurses/ncurses-6.4.tar.gz
tar -xzf ncurses-6.4.tar.gz
cd ncurses-6.4

# Compilar
./configure
make
sudo make install

# Configurar librería
sudo ldconfig
```

## Métodos de Compilación

### Método 1: Usando Makefile (RECOMENDADO)

```bash
# Ir al directorio del proyecto
cd tictactoe

# Ver opciones disponibles
make help

# Compilar el proyecto
make

# Compilar y ejecutar
make run

# Limpiar archivos compilados
make clean
```

**Resultado**: Ejecutable `tictactoe` en el directorio actual

### Método 2: Compilación Manual con g++

```bash
cd tictactoe

# Opción 1: Compilación en un paso
g++ -Wall -Wextra -std=c++11 -O2 -o tictactoe main.cpp board.cpp game.cpp ui.cpp -lncurses

# Opción 2: Compilación por pasos (más rápido en desarrollo)
g++ -Wall -Wextra -std=c++11 -O2 -c board.cpp -o board.o
g++ -Wall -Wextra -std=c++11 -O2 -c game.cpp -o game.o
g++ -Wall -Wextra -std=c++11 -O2 -c ui.cpp -o ui.o
g++ -Wall -Wextra -std=c++11 -O2 -c main.cpp -o main.o
g++ -Wall -Wextra -std=c++11 -O2 -o tictactoe main.o board.o game.o ui.o -lncurses
```

### Método 3: Con Banderas de Debugging

```bash
# Para debugging con gdb
g++ -g -Wall -Wextra -std=c++11 -o tictactoe main.cpp board.cpp game.cpp ui.cpp -lncurses

# Ejecutar con gdb
gdb ./tictactoe
```

### Método 4: Compilación Optimizada

```bash
# Máxima optimización
g++ -O3 -Wall -Wextra -std=c++11 -march=native -o tictactoe main.cpp board.cpp game.cpp ui.cpp -lncurses
```

## Explicación de Banderas del Compilador

| Bandera | Significado |
|---------|-------------|
| `-Wall` | Mostrar todos los warnings básicos |
| `-Wextra` | Mostrar warnings adicionales |
| `-std=c++11` | Usar estándar C++11 |
| `-O2` | Optimización nivel 2 (balance velocidad/tamaño) |
| `-O3` | Optimización máxima |
| `-g` | Incluir símbolos de debugging |
| `-march=native` | Optimizar para la CPU actual |
| `-lncurses` | Enlazar librería ncurses |

## Solución de Problemas

### Error: "ncurses.h: No such file or directory"
**Causa**: Librería ncurses no instalada
**Solución**:
```bash
# Ubuntu/Debian
sudo apt-get install ncurses-dev

# Fedora/RHEL
sudo dnf install ncurses-devel

# macOS
brew install ncurses
```

### Error: "undefined reference to `initscr'"
**Causa**: Falta enlazar la librería ncurses
**Solución**: Agregar `-lncurses` al final del comando de compilación

### Error: "permission denied" al ejecutar
**Causa**: Archivo no tiene permisos de ejecución
**Solución**:
```bash
chmod +x tictactoe
./tictactoe
```

### La terminal se ve corrupta después de ejecutar
**Causa**: ncurses no se limpió correctamente
**Solución**:
```bash
# Restaurar terminal
reset
# o
stty sane
```

### Error de compilación con g++ en macOS
**Causa**: Las herramientas de compilación no están instaladas
**Solución**:
```bash
xcode-select --install
```

## Verificación Post-Compilación

```bash
# Ver información del ejecutable
file tictactoe

# Ver dependencias dinámicas
ldd tictactoe
# En macOS:
otool -L tictactoe

# Ejecutar con diagnóstico
./tictactoe --version
# o simplemente
./tictactoe
```

## Distribución del Ejecutable

Para distribuir el ejecutable compilado:

```bash
# Crear paquete
mkdir -p tictactoe-1.0/bin
cp tictactoe tictactoe-1.0/bin/
cp README.md tictactoe-1.0/
tar -czf tictactoe-1.0.tar.gz tictactoe-1.0/

# O crear directorio portátil
mkdir tictactoe-portable
cp tictactoe tictactoe-portable/
cp README.md tictactoe-portable/
```

## Compilación Continua (Desarrollo)

Para desarrollo rápido, usar un archivo de configuración:

```bash
# Monitorear cambios y recompilar
while inotifywait -e modify *.cpp *.h; do make; done
```

O usar make con dependencies:

```bash
# Make solo recompila lo necesario
make
```

## Benchmarking

```bash
# Medir tiempo de compilación
time make

# Medir tamaño del ejecutable
ls -lh tictactoe

# Medir tiempo de ejecución
time ./tictactoe
```

## Compilación Cruzada (Cross-Compilation)

Para compilar para otra arquitectura:

```bash
# Para ARM (Raspberry Pi)
arm-linux-gnueabihf-g++ -Wall -Wextra -std=c++11 -O2 -o tictactoe main.cpp board.cpp game.cpp ui.cpp -lncurses

# Para x86 desde x64
g++ -m32 -Wall -Wextra -std=c++11 -O2 -o tictactoe main.cpp board.cpp game.cpp ui.cpp -lncurses
```

## Docker (Opcional)

Para ejecutar en un contenedor:

```dockerfile
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y build-essential ncurses-dev

WORKDIR /app
COPY . .

RUN make

CMD ["./tictactoe"]
```

Construir y ejecutar:
```bash
docker build -t tictactoe .
docker run -it tictactoe
```

## Verificación de Compilación

Ejecutar después de compilar para verificar:
```bash
# El ejecutable debe existir
test -f tictactoe && echo "Ejecutable creado correctamente"

# Debe ser ejecutable
test -x tictactoe && echo "Tiene permisos de ejecución"

# Ver dependencias
ldd tictactoe | grep ncurses
```

## Limpieza

```bash
# Eliminar solo los archivos objeto
make clean

# Hacer limpieza más profunda
rm -f *.o tictactoe core

# Hacer distclean (si está configurado)
make distclean 2>/dev/null || true
```
