# Quick Start - Tic-Tac-Toe ncurses

## 5 Segundos

```bash
cd code/tictactoe-ncurses
make && ./tictactoe
```

## 1 Minuto

```bash
# 1. Navega a la carpeta
cd code/tictactoe-ncurses

# 2. Compila (si no está compilado)
make

# 3. Ejecuta
./tictactoe

# 4. En el juego:
# - Presiona 1 para Play
# - Elige modo (1=normal vs IA)
# - Usa flechas + SPACE para jugar
# - Presiona Q para salir
```

## Primeros Pasos

### Paso 1: Instalar Dependencias

```bash
# Ubuntu/Debian
sudo apt install libncurses-dev build-essential

# Fedora
sudo dnf install ncurses-devel gcc-c++

# macOS
brew install ncurses
```

### Paso 2: Compilar

```bash
make clean
make
```

### Paso 3: Jugar

```bash
./tictactoe
# O: make run
```

### Paso 4: Entender Controles

| Tecla | Acción |
|-------|--------|
| **Flechas** | Mover celda |
| **SPACE** | Colocar X u O |
| **PgUp/Dn** | Cambiar tablero |
| **R** | Reiniciar tablero |
| **Q** | Salir |
| **Ratón** | Clic en casilla |

## Modos de Juego

### Modo 1: vs IA (Recomendado para empezar)
```
1. Ejecuta el juego
2. Presiona 1 (Play)
3. En settings: Presiona 3 (2 Jugadores)
4. Presiona Enter
5. Tú juegas O, IA juega X
6. ¡Intenta ganar!
```

### Modo 2: Automático (Ver máquina jugar)
```
1. Ejecuta el juego
2. Presiona 1 (Play)
3. En settings: Presiona 1 (0 Jugadores)
4. Presiona 4 (4 tableros)
5. Presiona Enter
6. ¡Observa cómo se juegan 4 juegos automáticamente!
```

### Modo 3: Manual (Juega ambos lados)
```
1. Ejecuta el juego
2. Presiona 1 (Play)
3. En settings: Presiona 2 (1 Jugador)
4. Juega manualmente como X y O
5. Los turnos alternan automáticamente
```

## Ejemplos Rápidos

### Ver juego automático
```bash
# Ejecuta, selecciona 1 → Jugar → 1 → 0 Jugadores → 6 tableros
```

### Jugar contra IA
```bash
# Ejecuta, selecciona 1 → Jugar → 2 → 2 Jugadores → 1 tablero
```

### Controlar ambos lados
```bash
# Ejecuta, selecciona 1 → Jugar → 2 → 1 Jugador → 2 tableros
```

## Estructura de Archivos

```
tictactoe-ncurses/
├── tictactoe          ← Ejecutable (genera al compilar)
├── Makefile           ← Compilación
├── build.sh           ← Script de utilidad
├── *.cpp *.h          ← Código fuente
└── *.md               ← Documentación
```

## Comandos Útiles

```bash
# Compilar
make

# Ejecutar
make run

# Limpiar archivos compilados
make clean

# Recompilar desde cero
make rebuild

# Ver ayuda de compilación
./build.sh help
```

## Navegación en el Juego

```
Menú Principal
    ↓
1. Play → Settings → Modo + Tableros → ¡Juega!
2. Settings → Configura → Vuelve al menú
3. Help → Lee instrucciones
4. Quit → Salir
```

## Tips de Juego

1. **Contra IA**: 
   - Intenta ocupar el centro
   - Crea dos amenazas simultáneamente
   - La IA es aleatoria (no es inteligente)

2. **Múltiples tableros**:
   - PgUp/PgDn cambia entre ellos
   - Cada uno es independiente
   - Puedes abandonar uno y continuar otro

3. **Reiniciar**:
   - Presiona R para resetear tablero actual
   - No afecta otros tableros

## Solución Rápida de Problemas

| Problema | Solución |
|----------|----------|
| "Command not found" | Instala build-essential |
| "ncurses.h not found" | Instala libncurses-dev |
| Pantalla corrupta | Redimensiona ventana, ejecuta `reset` |
| Ratón no funciona | Usa teclado en su lugar |
| Binario no existe | Ejecuta `make` |

## Documentación

- **README.md** - Guía completa
- **INSTALL.md** - Instalación detallada
- **EXAMPLES.md** - Casos de uso
- **FAQ.md** - Preguntas frecuentes
- **TECHNICAL.md** - Arquitectura
- **PROJECT_STRUCTURE.md** - Estructura de archivos

## Siguiente Paso

Lee **README.md** para entender características completas.

---

¡Disfruta del juego! 🎮
