# INFORME 2 — Comparativa completa de los 7 proyectos Tres en Raya con ncurses

Fecha del análisis: **2026-02-10**  
Repo: `Proyecto-MCPs/`  
Alcance: revisión de **código, arquitectura, build, UX y documentación** de todos los subproyectos dentro de `code/tictactoe_con_ncurses/`.

Proyectos analizados:

| # | Directorio | Modelo generador | Contexto MCP |
|---|------------|-----------------|--------------|
| 1 | `gpt-5p2_con_memory` | GPT-5-preview-2 | Con memoria |
| 2 | `gpt-5p2_sin_memory` | GPT-5-preview-2 | Sin memoria |
| 3 | `gpt-5p2_con_memory&mouse` | GPT-5-preview-2 | Con memoria + ratón |
| 4 | `prueba_gpt5p2_con memory&mouse` | GPT-5-preview-2 | Con memoria + ratón (prueba) |
| 5 | `haiku_con_memory` | Claude Haiku | Con memoria |
| 6 | `haiku_sin_memory` | Claude Haiku | Sin memoria |
| 7 | `sonnet_con_aprendizaje` | Claude Sonnet | Con aprendizaje |

> **Nota:** "memory / aprendizaje" se refiere al **contexto de generación/iteración con Copilot+MCPs** (persistencia de contexto entre turnos), no a memoria RAM del juego.

---

## 1) Resumen ejecutivo

- **GPT-5-preview-2 con memoria** (`gpt-5p2_con_memory`) es el proyecto más **voluminoso y modular** (~2.200 LOC, 14 archivos fuente), con RAII explícito para ncurses y detección dinámica de `ncursesw` vía `pkg-config`.
- **GPT-5-preview-2 sin memoria** (`gpt-5p2_sin_memory`) es el más **compacto** (~920 LOC, 7 archivos), con una clase `Ui` monolítica que concentra menús, ajustes y bucle de juego.
- **GPT-5-preview-2 con memoria y ratón** (`gpt-5p2_con_memory&mouse`) añade una clase `App` como máquina de estados y soporte completo de ratón (~1.200 LOC, 14 archivos).
- **Prueba GPT-5-preview-2** (`prueba_gpt5p2_con memory&mouse`) es una versión experimental simplificada (~1.000 LOC, 11 archivos) del anterior.
- **Haiku con memoria** (`haiku_con_memory`) es el más **extenso en documentación** (6 archivos .md) y el segundo en LOC (~2.450, 12 archivos fuente), con soporte WASD y ratón.
- **Haiku sin memoria** (`haiku_sin_memory`) es una versión **ligera** (~950 LOC, 8 archivos) que compila con C++11 y ofrece funcionalidad básica.
- **Sonnet con aprendizaje** (`sonnet_con_aprendizaje`) es el más **feature-rich** en interacción (~1.350 LOC, 8 archivos), con esqueleto de IA estratégica, seguimiento de estadísticas y script de test.

---

## 2) Comparativa rápida

| Aspecto | gpt-5p2 sin mem | gpt-5p2 con mem | gpt-5p2 mem+mouse | prueba gpt-5p2 | haiku sin mem | haiku con mem | sonnet aprendizaje |
|---|---|---|---|---|---|---|---|
| **Estándar C++** | C++17 | C++17 | C++17 | C++17 | C++11 | C++17 | C++11 |
| **LOC aprox.** | ~920 | ~2.200 | ~1.200 | ~1.000 | ~950 | ~2.450 | ~1.350 |
| **Archivos fuente** | 7 | 14 | 14 | 11 | 8 | 12 | 8 |
| **Clases principales** | Board, Settings, Ui | Board, Game, Menu, Settings, UI, NcursesSession | App, Board, Game, Menu, Settings, UI, Rect | Board, Game, Menu, Settings, UI | Board, Game, UI | Board, Game, UI, Input, Settings | Board, GameManager, UI |
| **Soporte ratón** | ❌ | ❌ | ✅ | ✅ | ❌ | ✅ | ✅ |
| **Soporte WASD** | ❌ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| **Modos de juego** | 0/1/2 jugadores | 0/1/2 jugadores | 0/1/2 jugadores | 0/1/2 jugadores | 0/1/2 jugadores | 0/1/2 jugadores | 0/1/2 jugadores |
| **Multi-tablero** | ✅ (Tab) | ✅ (1-12) | ✅ (1-9) | ✅ (1-9) | ✅ | ✅ (1/2/4/6/9) | ✅ (1-12) |
| **IA oponente** | Random | Random | Random | Random | Random | Random | Random (esqueleto estratégico) |
| **Colores ncurses** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **pkg-config** | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | ❌ |
| **Docs (archivos .md)** | 1 | 1 | 1 | 1 | 2 | 6 | 4 |

---

## 3) Arquitectura y diseño

### 3.1 GPT-5-preview-2 sin memoria

Diseño **monolítico**: la clase `Ui` gestiona menús, configuración y el bucle de juego. `Board` encapsula la lógica del tablero y `Settings` almacena preferencias. Es fácil de leer pero escala peor si se quieren añadir funcionalidades.

### 3.2 GPT-5-preview-2 con memoria

Diseño **modular con RAII**: introduce `NcursesSession` para gestionar el ciclo de vida de ncurses de forma segura. Separa `Menu`, `Game`, `UI`, `Board` y `Settings` en módulos independientes. Usa `pkg-config` para detectar automáticamente la biblioteca ncurses disponible.

### 3.3 GPT-5-preview-2 con memoria y ratón

Añade una clase `App` que actúa como **máquina de estados** (MainMenu → Settings → Help → Game). Incorpora `Rect` como estructura auxiliar para detección de clics (hit-testing). Es la versión GPT con mayor separación de responsabilidades.

### 3.4 Prueba GPT-5-preview-2 con memoria y ratón

Versión **experimental simplificada** del anterior. Reduce el número de clases eliminando `App` y `Rect`, y utiliza una estructura `Layout` para la disposición de la interfaz. Sirve como banco de pruebas para validar el enfoque.

### 3.5 Haiku sin memoria

Diseño **MVC básico**: `Board` (modelo), `UI` (vista) y `Game` (controlador). Compila con C++11, lo que indica un enfoque conservador y compatible. Es la implementación más directa y minimalista.

### 3.6 Haiku con memoria

Diseño **MVC extendido**: añade `Input` (gestión unificada de teclado/ratón) y `Settings` (configuración persistente) al modelo base. Es el proyecto con más documentación generada (6 archivos .md incluyendo FAQ, EXAMPLES, QUICKSTART).

### 3.7 Sonnet con aprendizaje

Diseño **MVC-like** con `GameManager` en lugar de `Game`. Destaca por incluir un **esqueleto de IA estratégica** (comprobación de victoria, bloqueo, centro, esquina, random) aunque en la práctica solo ejecuta movimientos aleatorios. Incluye seguimiento de estadísticas y un script `test.sh` para validación automática.

---

## 4) Impacto de la memoria MCP en la generación

### 4.1 GPT-5-preview-2: con memoria vs sin memoria

| Aspecto | Sin memoria | Con memoria | Diferencia |
|---|---|---|---|
| LOC | ~920 | ~2.200 | **+139%** |
| Archivos | 7 | 14 | **+100%** |
| Clases | 3 | 6 | **+100%** |
| Modularidad | Baja (monolítica) | Alta (RAII, separación) | Gran mejora |
| pkg-config | ✅ | ✅ | Igual |

**Conclusión:** La memoria MCP permitió a GPT-5-preview-2 generar un código significativamente más modular, con el doble de clases y archivos, y patrones de diseño más sofisticados como RAII.

### 4.2 Haiku: con memoria vs sin memoria

| Aspecto | Sin memoria | Con memoria | Diferencia |
|---|---|---|---|
| LOC | ~950 | ~2.450 | **+158%** |
| Archivos | 8 | 12 | **+50%** |
| Clases | 3 | 5 | **+67%** |
| Soporte ratón | ❌ | ✅ | Nueva funcionalidad |
| Documentación | 2 .md | 6 .md | **+200%** |

**Conclusión:** Con memoria, Haiku generó código más extenso, añadió módulos especializados (`Input`, `Settings`), incorporó soporte de ratón y produjo documentación mucho más completa.

### 4.3 Efecto del ratón (GPT-5-preview-2)

Comparando `gpt-5p2_con_memory` (sin ratón) con `gpt-5p2_con_memory&mouse` (con ratón):
- Se introduce la clase `App` como máquina de estados.
- Se añade `Rect` para hit-testing de clics.
- La complejidad general disminuye ligeramente en LOC (~2.200 → ~1.200) pero aumenta en responsabilidades por clase.

---

## 5) Comparativa entre modelos

### 5.1 GPT-5-preview-2 vs Haiku vs Sonnet (todos con memoria/contexto)

| Aspecto | GPT-5-preview-2 | Haiku | Sonnet |
|---|---|---|---|
| **LOC** | ~2.200 | ~2.450 | ~1.350 |
| **Estándar C++** | C++17 | C++17 | C++11 |
| **Modularidad** | Alta (6 clases + RAII) | Alta (5 clases + Input) | Media (3 clases) |
| **Documentación** | Básica (1 README) | Extensa (6 archivos) | Buena (4 archivos) |
| **IA** | Random | Random | Esqueleto estratégico |
| **Ratón** | ❌ (sí en variante) | ✅ | ✅ |
| **Calidad build** | pkg-config, C++17 | Directo, C++17 | Directo, C++11 |
| **Testing** | ❌ | ❌ | ✅ (`test.sh`) |

**Observaciones:**
- **GPT-5-preview-2** produce el código más limpio en estructura de proyecto (separación `src/`, `include/`, `bin/`) y con mejor portabilidad de build.
- **Haiku** genera la mayor cantidad de documentación y un diseño completo de Input/Settings.
- **Sonnet** ofrece la mayor riqueza funcional (esqueleto IA, estadísticas, script de test) en menos líneas de código.

---

## 6) Compilación

Todos los proyectos usan `make` como sistema de build con `g++` y enlace contra `-lncurses`.

| Proyecto | Comando | Flags destacados |
|---|---|---|
| gpt-5p2_sin_memory | `make` | `-std=c++17 -Wall -Wextra -O2` |
| gpt-5p2_con_memory | `make` | `-std=c++17 -Wall -Wextra -O2` + pkg-config |
| gpt-5p2_con_memory&mouse | `make` | `-std=c++17 -Wall -Wextra -O2` + pkg-config |
| prueba_gpt5p2 | `make` | `-std=c++17 -Wall -Wextra -O2` + pkg-config |
| haiku_sin_memory | `make` | `-std=c++11 -Wall -Wextra -O2` |
| haiku_con_memory | `make` | `-std=c++17 -Wall -Wextra -O2` |
| sonnet_con_aprendizaje | `make` | `-std=c++11 -Wall -Wextra -pedantic` |

> **Nota:** Sonnet es el único que usa `-pedantic`, lo que muestra un enfoque más estricto en conformidad con el estándar.

---

## 7) Documentación generada

| Proyecto | Archivos de documentación |
|---|---|
| gpt-5p2_sin_memory | README.md |
| gpt-5p2_con_memory | README.md |
| gpt-5p2_con_memory&mouse | README.md |
| prueba_gpt5p2 | README.md |
| haiku_sin_memory | README.md, COMPILACION.md, ESTRUCTURA_CODIGO.md, INDICE.md, INSTRUCCIONES.md, RESUMEN_PROYECTO.md + textos (.txt) |
| haiku_con_memory | README.md, EXAMPLES.md, FAQ.md, INDEX.md, INSTALL.md, PROJECT_STRUCTURE.md, QUICKSTART.md + SUMMARY.txt |
| sonnet_con_aprendizaje | README.md, DEVELOPMENT.md, QUICKSTART.md, PROJECT_SUMMARY.txt |

**Haiku** (ambas versiones) genera con diferencia la mayor cantidad de documentación, incluyendo guías de instalación, FAQ y ejemplos de uso.

---

## 8) Conclusiones

1. **La memoria MCP mejora sustancialmente la calidad del código generado.** Tanto en GPT-5-preview-2 como en Haiku, las versiones con memoria produjeron más del doble de líneas, con arquitecturas más modulares y mejor separación de responsabilidades.

2. **Cada modelo tiene fortalezas distintas:**
   - **GPT-5-preview-2** destaca en estructura de proyecto y portabilidad de build (pkg-config, directorios `src/include/bin`).
   - **Haiku** sobresale en generación de documentación exhaustiva.
   - **Sonnet** ofrece mayor riqueza funcional (esqueleto IA, estadísticas, testing) con código más conciso.

3. **Ninguno implementa una IA real.** Todos los oponentes automáticos se limitan a movimientos aleatorios. Solo Sonnet incluye un esqueleto preparado para estrategia (bloqueo, centro, esquinas).

4. **El soporte de ratón es un diferenciador claro** entre variantes, y su incorporación conlleva cambios arquitectónicos significativos (máquinas de estado, hit-testing).

5. **La variante "prueba"** (`prueba_gpt5p2_con memory&mouse`) confirma que las versiones experimentales son útiles para iterar sobre el diseño antes de consolidar la versión final.
