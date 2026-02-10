# Guía de Instalación - Tic-Tac-Toe ncurses

## Requisitos Previos

- **Sistema Operativo**: Linux o macOS
- **Compilador**: GCC/G++ 7.0 o superior (recomendado 9.0+)
- **Librería ncurses**: development headers
- **Make**: Disponible en la mayoría de sistemas Linux

### Verificar Requisitos

```bash
# Verifica versión de g++
g++ --version

# Verifica si make está instalado
make --version

# Verifica si ncurses está disponible
pkg-config --modversion ncurses
# O
apt search libncurses-dev  # En Debian/Ubuntu
```

---

## Instalación por Sistema Operativo

### Debian/Ubuntu

```bash
# Actualizar lista de paquetes
sudo apt update

# Instalar dependencias
sudo apt install -y \
    build-essential \
    g++ \
    make \
    libncurses-dev \
    pkg-config

# (Opcional) Instalar git para clonar/actualizar
sudo apt install git
```

### Fedora/RHEL/CentOS

```bash
# Instalar dependencias
sudo dnf install -y \
    gcc-c++ \
    make \
    ncurses-devel

# (Opcional) Para pkg-config
sudo dnf install pkg-config
```

### Arch Linux

```bash
sudo pacman -S base-devel ncurses
```

### openSUSE

```bash
sudo zypper install gcc-c++ make ncurses-devel
```

### macOS (con Homebrew)

```bash
# Instalar Homebrew si no lo tienes
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Instalar dependencias
brew install gcc make ncurses pkg-config

# Nota: macOS puede requerir configuración adicional de rutas
export LDFLAGS="-L/usr/local/opt/ncurses/lib"
export CPPFLAGS="-I/usr/local/opt/ncurses/include"
```

### Windows (WSL2)

```bash
# En Windows, abre Ubuntu en WSL2

# Luego sigue los pasos de Debian/Ubuntu arriba
sudo apt update && sudo apt install build-essential g++ make libncurses-dev
```

---

## Instalación del Proyecto

### Opción 1: Usando git

```bash
# Clonar el repositorio (si está disponible)
git clone https://github.com/usuario/tictactoe-ncurses.git
cd tictactoe-ncurses

# O navegar a la carpeta si ya la tienes
cd code/tictactoe-ncurses
```

### Opción 2: Desde el repositorio local

```bash
cd code/tictactoe-ncurses
```

---

## Compilación

### Método 1: Usando make (Recomendado)

```bash
# Compilar
make

# O compilar y limpiar primero
make clean && make

# O recompilar desde cero
make rebuild
```

### Método 2: Usando el script build.sh

```bash
# Hacer script ejecutable (si no lo es)
chmod +x build.sh

# Compilar
./build.sh build

# O compilar y ejecutar
./build.sh run
```

### Método 3: Compilación manual

```bash
# Compilar cada archivo
g++ -std=c++17 -Wall -Wextra -O2 -c main.cpp
g++ -std=c++17 -Wall -Wextra -O2 -c Board.cpp
g++ -std=c++17 -Wall -Wextra -O2 -c UI.cpp
g++ -std=c++17 -Wall -Wextra -O2 -c Input.cpp
g++ -std=c++17 -Wall -Wextra -O2 -c Settings.cpp
g++ -std=c++17 -Wall -Wextra -O2 -c Game.cpp

# Enlazar
g++ -o tictactoe main.o Board.o UI.o Input.o Settings.o Game.o -lncurses
```

---

## Verificación de Instalación

```bash
# Verificar que el binario se creó
ls -lh tictactoe

# Debería mostrar algo como:
# -rwxr-xr-x 1 usuario grupo 38K Feb 10 09:00 tictactoe
```

---

## Ejecución

### Opción 1: Usando make

```bash
make run
```

### Opción 2: Usando el script

```bash
./build.sh run
```

### Opción 3: Ejecutar directamente

```bash
./tictactoe
```

---

## Solución de Problemas

### Error: "command not found: g++"

**Solución**: Instala build-essential o gcc-c++
```bash
# Debian/Ubuntu
sudo apt install build-essential

# Fedora
sudo dnf install gcc-c++ make
```

### Error: "ncurses.h: No such file or directory"

**Solución**: Instala headers de ncurses
```bash
# Debian/Ubuntu
sudo apt install libncurses-dev

# Fedora
sudo dnf install ncurses-devel

# macOS
brew install ncurses
```

### Error: "undefined reference to `initscr'"

**Solución**: Asegúrate de que ncurses está enlazado
```bash
# Verifica que el Makefile tenga:
# LDFLAGS = -lncurses

# Si usas compilación manual:
g++ -o tictactoe *.o -lncurses  # Importante el -lncurses al final
```

### Error: "pkg-config not found"

**Solución**: Instala pkg-config (opcional, no crítico)
```bash
# Debian/Ubuntu
sudo apt install pkg-config

# Fedora
sudo dnf install pkg-config
```

### El programa compila pero la pantalla se ve corrupta

**Soluciones**:
1. Asegúrate de que tu terminal soporta 80x24 caracteres mínimo
2. Instala una terminal moderna: GNOME Terminal, xterm, urxvt
3. Verifica que ncurses está actualizado

### "make: *** [Makefile:17: main.o] Error 1"

**Solución**: Compila con `make rebuild`:
```bash
make clean && make
```

---

## Verificación Post-instalación

```bash
# Test 1: Ejecutar programa
./tictactoe

# Deberías ver el menú principal
# Si lo ves, ¡la instalación es exitosa!

# Test 2: Usar todas las características
# 1. Presiona '1' para Jugar
# 2. Selecciona opciones en Settings
# 3. Juega un juego
# 4. Presiona 'q' para salir

# Si todo funciona, la instalación está completa
```

---

## Instalación en Sistema de Archivos Compartido

Si instalas en un directorio compartido (p.ej., Dropbox), asegúrate de:

```bash
# 1. Compila en el sistema destino
cd /ruta/compartida/tictactoe-ncurses
make

# 2. No compartas archivos .o o tictactoe (binarios compilados)
# Solo versionea los archivos .cpp, .h, Makefile y documentación

# .gitignore recomendado:
echo "*.o
tictactoe
.DS_Store" > .gitignore
```

---

## Instalación en Servidor Remoto

```bash
# 1. SSH a tu servidor
ssh usuario@servidor

# 2. Instala dependencias
sudo apt update && sudo apt install build-essential libncurses-dev

# 3. Descarga el proyecto
git clone <repo> || scp -r local/path usuario@servidor:/ruta/destino

# 4. Compila
cd tictactoe-ncurses
make

# 5. Ejecuta
./tictactoe
# Nota: El ratón podría no funcionar en SSH
# Usa solo teclado
```

---

## Desinstalación

```bash
# Limpiar archivos compilados
cd code/tictactoe-ncurses
make clean

# Eliminar directorio completo
cd ..
rm -rf tictactoe-ncurses

# Desinstalar dependencias (opcional)
# ADVERTENCIA: Esto también elimina otros paquetes que dependan de ncurses
# Es generalmente seguro, pero sé cuidadoso
sudo apt autoremove libncurses-dev  # Solo si quieres
```

---

## Siguientes Pasos

Después de instalar con éxito:

1. **Lee el README.md** para entender el proyecto
2. **Consulta EXAMPLES.md** para casos de uso
3. **Revisa TECHNICAL.md** para arquitectura
4. **Prueba los 3 modos de juego**

---

## Obtener Ayuda

- **Documentación local**: Consulta README.md, FAQ.md, TECHNICAL.md
- **Compilación**: Revisa los mensajes de error en "Solución de Problemas"
- **Gameplay**: Lee EXAMPLES.md para tutoriales
- **Código**: Ver comentarios en archivos .cpp

---

Documento actualizado: Febrero 2025
