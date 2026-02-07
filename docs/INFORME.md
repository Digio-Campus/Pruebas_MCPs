# INFORME — Comparativa de proyectos (gpt-5p2_con_memory vs gpt-5p2_sin_memory vs sonnet_con_aprendizaje)

Fecha del análisis: **2026-02-07**  
Repo: `Proyecto-MCPs/`  
Alcance: revisión de **código, arquitectura, build, UX en ncurses y documentación** de los subproyectos:

- `code/gpt-5p2_con_memory/`
- `code/gpt-5p2_sin_memory/`
- `code/sonnet_con_aprendizaje/`

> Nota: “memory / aprendizaje” aquí se interpreta como **contexto de generación/iteración con Copilot+MCPs** (persistencia de contexto), no como “memoria RAM” del juego.

---

## 1) Resumen ejecutivo

- **gpt-5p2_con_memory** es el más **modular** (separa *UI / Menu / Game / Board / Settings* y añade RAII explícito para ncurses), con buenas decisiones de portabilidad (usa `pkg-config` para `ncursesw`/`ncurses`).
- **gpt-5p2_sin_memory** es el más **simple/compacto**: una única clase `Ui` concentra menús, ajustes y bucle de juego; es más fácil de leer al principio, pero escala peor.
- **sonnet_con_aprendizaje** es el más **feature-rich** en interacción (incluye **ratón**) y el más **documentado** (README + QUICKSTART + DEVELOPMENT + script de test), pero con un diseño algo más “grande” y un *warning* de compilación por signed/unsigned.

En este entorno, los tres proyectos **compilan correctamente** (con un warning menor en `sonnet_con_aprendizaje`).

---

## 2) Comparativa rápida (tabla)

| Aspecto | gpt-5p2_sin_memory | gpt-5p2_con_memory | sonnet_con_aprendizaje |
|---|---|---|---|
| Estándar C++ (Makefile) | C++17 | C++17 | C++11 |
| Arquitectura | Monolítica (`Ui`) | Modular (`Menu`, `Game`, `UI`, `NcursesSession`) | MVC-like (`GameManager` + `UI`) |
| Inicialización ncurses | RAII interno (struct local) | **RAII dedicado** (`NcursesSession`) | `UI::init()` / `cleanup()` |
| Menús | Dentro de `Ui` | `Menu` separado + `UI` render | Dentro de `UI` |
| Lógica de juego multi-tablero | Dentro de `Ui` | `Game` dedicado | `GameManager` dedicado |
| Auto-juego | Sí (modo 0 / modo 2 con X auto) | Sí (modo 0 / modo 2 con X auto) + **pausa** (`p`) | Sí (modo 0 / modo 2 con X auto) |
| Ratón | No | No | **Sí** (`mousemask`, `KEY_MOUSE`) |
| Límite tableros | 1..12 (`Settings::kMaxBoards`) | clamp 1..24, pero UI limita por “lo que cabe” | 1..12 |
| Portabilidad de linkado | `-lncurses` fijo | `pkg-config ncursesw/ncurses` (fallback a `-lncurses`) | `-lncurses` fijo |
| Documentación | README breve | README breve (con una ruta desactualizada) | README + QUICKSTART + DEVELOPMENT + PROJECT_SUMMARY |
| Testing | (no visible) | (no visible) | `test.sh` + guía valgrind |

---

## 3) Análisis por proyecto

### A) `gpt-5p2_sin_memory`

**Objetivo funcional**: Tres en raya multi-tablero con ncurses, con modos 0/1/2 jugadores y estadísticas por tablero.

**Puntos fuertes**
- Código relativamente directo: `Ui` gestiona menú principal, ajustes, ayuda y bucle de partida.
- `Board` incluye RNG propio (`std::mt19937`) y ofrece `randomMove()`.
- `Settings` define límites explícitos: `kMinBoards=1`, `kMaxBoards=12`.

**Puntos a mejorar / riesgos**
- Concentración de responsabilidades: `Ui.cpp` mezcla *layout + render + input + reglas de modos*, lo que dificulta crecer (por ejemplo, añadir ratón o más pantallas) sin aumentar complejidad.
- El linkado va fijo a `-lncurses` (puede fallar en sistemas donde la opción preferida sea `ncursesw`).

**Build/Run**
- Build: `make -C code/gpt-5p2_sin_memory`
- Ejecutable: `code/gpt-5p2_sin_memory/bin/tres_en_raya`

---

### B) `gpt-5p2_con_memory`

**Objetivo funcional**: mismo dominio (multi-tablero + ncurses), pero con separación clara de capas.

**Arquitectura**
- `NcursesSession`: RAII explícito (inicializa y hace `endwin()` en destructor) y configura colores.
- `UI`: centrada en **render** (menú, ayuda, ajustes y partida), con cálculo de layout (`layoutBoards`, `maxBoardsFit`).
- `Menu`: navegación del menú principal/ajustes/ayuda.
- `Game`: bucle de juego, control de cursores por tablero, auto-movimientos, reset por tablero y global.

**Puntos fuertes**
- Mejor mantenibilidad: responsabilidades separadas (UI ≠ input de menú ≠ bucle de juego).
- Portabilidad de dependencias: Makefile usa `pkg-config --libs ncursesw || ncurses` con fallback.
- UX en modo auto: añade **pausa/reanudar** en modo 0 (`p`) y reset global (`R`).
- Gestión “lo que cabe en pantalla”: el juego puede ajustar el número de tableros al máximo visible (`maxBoardsFit`).

**Puntos a mejorar / riesgos**
- `Settings::clamp()` permite hasta 24 tableros, pero en la práctica se recorta por layout; conviene documentar este comportamiento como “cap por pantalla” para evitar confusión.
- README tiene una ruta de compilación desactualizada (`cd code/TicTacToe-ncurses`) que no coincide con el directorio real.

**Build/Run**
- Build: `make -C code/gpt-5p2_con_memory`
- Ejecutable: `code/gpt-5p2_con_memory/bin/tictactoe`

---

### C) `sonnet_con_aprendizaje`

**Objetivo funcional**: TicTacToe multi-tablero con ncurses, con enfoque en funcionalidad completa y documentación.

**Arquitectura**
- `Board`: lógica del tablero.
- `GameManager`: gestiona colección de tableros, selección, modos y estadísticas.
- `UI`: gestiona ncurses, menús, render y **ratón**.

**Puntos fuertes**
- Mejor UX: soporte de **ratón** (clic para seleccionar/jugar) además de teclado.
- Documentación extensa: `README.md`, `QUICKSTART.md`, `DEVELOPMENT.md`, `PROJECT_SUMMARY.txt`.
- Incluye script de verificación (`test.sh`) y recomendaciones de depuración/valgrind.

**Puntos a mejorar / riesgos**
- Compila con un *warning* de signed/unsigned en `UI.cpp` (comparación entre `int` y `size_t`). No rompe el build, pero conviene limpiarlo.
- Makefile usa C++11; no es un problema, pero limita features modernas frente a las variantes en C++17.
- La documentación de “rutas” también parece referirse a `code/tictactoe` en algunos archivos (no coincide con `code/sonnet_con_aprendizaje`).

**Build/Run**
- Build: `make -C code/sonnet_con_aprendizaje`
- Ejecutable: `code/sonnet_con_aprendizaje/tictactoe`

---

## 4) Comparación enfocada a “memory / aprendizaje” (MCP)

En el repositorio hay configuración de MCP Memory en `.vscode/mcp.json`, persistiendo en `data/memory.jsonl`:

```json
"memory": {
  "command": "npx",
  "args": ["-y", "@modelcontextprotocol/server-memory"],
  "env": {
    "MEMORY_PERSIST": "true",
    "MEMORY_FILE_PATH": "${workspaceFolder}/data/memory.jsonl"
  }
}
```

**Implicación práctica en proyectos**
- Un flujo “con memory / con aprendizaje” suele reflejarse en:
  - mayor modularidad y consistencia (p.ej., RAII explícito, separación de capas),
  - más documentación reutilizable,
  - y artefactos persistidos (procedimientos, resúmenes, decisiones) para iteraciones futuras.

En esta comparativa, esos rasgos aparecen especialmente en **gpt-5p2_con_memory** (diseño más limpio) y **sonnet_con_aprendizaje** (documentación y features).

---

## 5) Recomendación según objetivo

- Si buscas **base mantenible y ampliable** (p.ej. añadir IA, nuevas pantallas, refactors): **`gpt-5p2_con_memory`**.
- Si buscas **mejor experiencia de usuario** en terminal (ratón + documentación para usuarios): **`sonnet_con_aprendizaje`**.
- Si buscas **referencia mínima** para explicar el proyecto o enseñar ncurses sin demasiadas capas: **`gpt-5p2_sin_memory`**.
