# INFORME 5: Comparativa Final de los 6 Proyectos TicTacToe-ncurses

## Cambios realizados en la memoria 

El problema central que revelan los informes anteriores es que la memoria actual contiene conocimiento descriptivo/arquitectónico pero no prescriptivo/cualitativo. Esto ancla a los modelos a replicar un diseño mediocre en lugar de mejorar.

**ELIMINAR (causan sesgo de anclaje negativo):**

1. Observaciones con implementaciones concretas mediocres — Por ejemplo, makefile-structure dice CC = g++ y -std=c++11. Esto ancla al modelo a usar C++11 en vez de C++17.

2. Datos demasiado específicos de una implementación anterior — Como char grid[3][3] en Board-class, que ancla al modelo a usar arrays C en lugar de std::vector o enum class.

3. Entidades no relacionadas con el proyecto — Las 3 entidades de Ticketmaster y los profesores/exámenes de la UMU consumen contexto sin aportar nada.

**AÑADIR (conocimiento prescriptivo que mejore la calidad):**

1. Entidad de "Buenas Prácticas C++" con observaciones como: "Usar enum class para Cell y Result", "Usar std::mt19937 en vez de rand()", "Usar namespaces", "Usar C++17"

2. Entidad de "Calidad de Código" con: "Encapsulación estricta (todo privado con getters)", "Prevenir doble conteo de resultados", "Centralizar lógica sin duplicación", "RAII para ncurses"

3. Actualizar las observaciones existentes para que sean prescriptivas, no descriptivas

Aquí el resumen de los cambios realizados en la memoria:

**RESUMEN DE CAMBIOS EN MEMORY**

🆕 4 nuevas entidades creadas:

| Entidad | Tipo | Propósito |
|---|---|---|
| **CppBestPractices** | standard | enum class, mt19937, namespaces, RAII, C++17, encapsulación|
| **CodeQuality** | standard | Centralizar lógica, prevenir doble conteo, manejo de errores, animación modo auto |
| **MakefileBestPractices** | standard |CXX (no CC), C++17, pkg-config, target test, directorios estándar |
| **UIBestPractices** | standard | Caracteres ACS, colores diferenciados X/O, barra contextual, detección ventana pequeña |

🔗 9 nuevas relaciones debe_seguir/debe_cumplir:

* TicTacToe-ncurses → CppBestPractices, CodeQuality, MakefileBestPractices, UIBestPractices
* Board-class → CppBestPractices, CodeQuality
* UI-class → UIBestPractices
* Game-class → CodeQuality
* makefile-structure → MakefileBestPractices

✏️ Observaciones actualizadas en 7 entidades existentes:

* Board-class: Añadidas 5 observaciones OBLIGATORIO (enum class, mt19937, privados, availableMoves, detener turno)
* UI-class: Añadidas 4 observaciones OBLIGATORIO (stats integradas, cabecera estado, barra controles, terminal mínima)
* Game-class: Añadidas 4 observaciones OBLIGATORIO (playerMove centralizado, teclas 1-9, nodelay, reinicio r/R)
* makefile-structure: Actualizado a CXX, C++17, pkg-config, target test
* ncurses-integration: Añadidos ACS, use_default_colors, colores diferenciados
* Settings-class: Encapsulación obligatoria
* TicTacToe-ncurses: Ubicación corregida, C++17, tests obligatorios, ratón obligatorio

🧹 Observaciones eliminadas (causaban sesgo):

* CC = g++ y -std=c++11 de makefile-structure
* Ubicación incorrecta code/tictactoe/
* Rutas de ejecución hard-coded

🎯 Clave del cambio:

Las observaciones pasaron de descriptivas ("El proyecto usa char para celdas") a prescriptivas ("OBLIGATORIO usar enum class Cell"). Esto evita el sesgo de anclaje que INFORME4 identificó y guía activamente a Raptor/Grok a usar patrones de C++ moderno.

## Objetivo de este informe

Comparar exhaustivamente los seis proyectos de Tres en Raya con ncurses en C++ generados por los modelos **Grok** y **Raptor** (Claude) bajo tres condiciones experimentales:

1. **Con Memory descriptivo** (1ª ronda): grafo de conocimiento con observaciones descriptivas
2. **Sin Memory**: sin acceso al MCP Memory
3. **Con Memory prescriptivo** (2ª ronda): grafo con directivas OBLIGATORIO/PREFERIR y relaciones incrustadas

| Condición | Grok | Raptor |
|---|---|---|
| Con Memory descriptivo (1ª ronda) | `grok_con_memory` | `raptor_con_memory` |
| Sin Memory | `grok_sin_memory` | `raptor_sin_memory` |
| Con Memory prescriptivo (2ª ronda) | `tictactoe-grok` | `tictactoe-raptor` |

---

## 1. Datos Estructurales

### 1.1 Ficheros y Líneas de Código

| Proyecto | Archivos (.cpp + .h) | Líneas totales | Clases/Módulos | Tests | Compila |
|---|---|---|---|---|---|
| **grok_con_memory** | 11 | 604 | 5 (Board, Game, Menu, Settings, UI) | ❌ No | ✅ Sí |
| **grok_sin_memory** | 11 | 878 | 5 (Board, Game, Menu, Settings, UI) | ❌ No | ✅ Sí |
| **raptor_con_memory** | 12 (+1 test) | 606 | 5 (Board, Game, Menu, Settings, UI) | ✅ 1 test | ✅ Sí (con warnings) |
| **raptor_sin_memory** | 7 | 799 | 3 (Board, Game, UI) | ❌ No | ✅ Sí |
| **tictactoe-grok** | 12 (+1 test) | 891 | 5 (Board, Game, Menu, Settings, UI) | ✅ 1 test (6 asserts) | ✅ Sí |
| **tictactoe-raptor** | 13 (+1 test +1 script) | 698 | 5 (Board, Game, Menu, Settings, UI) | ✅ 1 test + 1 script integración | ✅ Sí |

### 1.2 Desglose por Fichero

#### grok_con_memory (604 líneas)

| Fichero | Líneas |
|---|---|
| src/UI.cpp | 181 |
| src/Game.cpp | 105 |
| src/Board.cpp | 95 |
| src/Menu.cpp | 44 |
| src/Settings.cpp | 26 |
| src/main.cpp | 26 |
| include/UI.h | 38 |
| include/Board.h | 28 |
| include/Game.h | 27 |
| include/Menu.h | 17 |
| include/Settings.h | 17 |

#### grok_sin_memory (878 líneas)

| Fichero | Líneas |
|---|---|
| src/UI.cpp | 167 |
| src/Game.cpp | 132 |
| src/Board.cpp | 95 |
| src/Menu.cpp | 60 |
| src/main.cpp | 39 |
| src/Settings.cpp | 27 |
| include/UI.h | 104 |
| include/Board.h | 85 |
| include/Game.h | 76 |
| include/Settings.h | 51 |
| include/Menu.h | 42 |

#### raptor_con_memory (606 líneas)

| Fichero | Líneas |
|---|---|
| src/UI.cpp | 186 |
| src/Game.cpp | 171 |
| src/Board.cpp | 66 |
| src/Menu.cpp | 14 |
| src/Settings.cpp | 15 |
| src/main.cpp | 7 |
| include/UI.h | 32 |
| include/Game.h | 31 |
| include/Board.h | 28 |
| include/Menu.h | 19 |
| include/Settings.h | 13 |
| tests/test_mouse_logic.cpp | 24 |

#### raptor_sin_memory (799 líneas)

| Fichero | Líneas |
|---|---|
| src/UI.cpp | 420 |
| src/Game.cpp | 132 |
| src/Board.cpp | 91 |
| src/main.cpp | 15 |
| include/Game.h | 51 |
| include/Board.h | 46 |
| include/UI.h | 44 |

#### tictactoe-grok (891 líneas)

| Fichero | Líneas |
|---|---|
| src/UI.cpp | 212 |
| src/Game.cpp | 180 |
| src/Board.cpp | 136 |
| src/Menu.cpp | 84 |
| src/main.cpp | 31 |
| src/Settings.cpp | 2 |
| include/Board.h | 61 |
| include/Settings.h | 41 |
| include/Game.h | 38 |
| include/UI.h | 34 |
| include/Menu.h | 31 |
| tests/test_board.cpp | 41 |

#### tictactoe-raptor (698 líneas)

| Fichero | Líneas |
|---|---|
| src/UI.cpp | 165 |
| src/Game.cpp | 162 |
| src/Board.cpp | 103 |
| src/Menu.cpp | 26 |
| src/Settings.cpp | 25 |
| src/main.cpp | 13 |
| include/UI.h | 61 |
| include/Board.h | 51 |
| include/Game.h | 31 |
| include/Settings.h | 22 |
| include/Menu.h | 15 |
| tests/test_mouse_logic.cpp | 24 |
| tests/test_integration.sh | (script) |

### 1.3 Estructura de Directorios

| Proyecto | Estructura |
|---|---|
| **grok_con_memory** | `include/` + `src/` + `obj/` + `bin/` |
| **grok_sin_memory** | `include/` + `src/` + `obj/` + `bin/` |
| **raptor_con_memory** | `include/` + `src/` + `bin/` + `tests/` |
| **raptor_sin_memory** | `include/` + `src/` + `bin/` (objetos en `src/`) |
| **tictactoe-grok** | `include/` + `src/` + `obj/` + `bin/` + `tests/` |
| **tictactoe-raptor** | `include/` + `src/` + `obj/` + `bin/` + `tests/` |

---

## 2. Tabla Comparativa de Características C++ Moderno

| Criterio | grok_con | grok_sin | raptor_con | raptor_sin | **tictactoe-grok** | **tictactoe-raptor** |
|---|---|---|---|---|---|---|
| **Namespaces** | ❌ | ❌ | ❌ | ✅ `ttt` | ✅ `ttt` | ✅ `ttt` |
| **enum class Cell** | ❌ `char` | ❌ `char` | ❌ `char` | ✅ | ✅ | ✅ |
| **enum class Result** | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **std::mt19937** | ❌ `rand()` | ❌ `rand()` | ❌ `std::rand()` | ✅ | ✅ | ✅ |
| **std::optional** | ❌ | ❌ | ❌ | ❌ | ✅ | ✅ |
| **std::clamp** | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ |
| **Estándar C++** | C++11 | C++17 | C++11 | C++17 | C++17 | C++17 |
| **Optimización -O2** | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **try/catch en main** | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **Variable Makefile** | ❌ `CC` | ✅ `CXX` | ✅ `CXX` | ✅ `CXX` | ✅ `CXX` | ✅ `CXX` |

### Representación del Tablero

| Proyecto | Tipo de grid | Tipo de celda |
|---|---|---|
| **grok_con_memory** | `char grid[3][3]` (array C) | `char` ('X','O',' ') |
| **grok_sin_memory** | `vector<vector<char>>` (STL) | `char` ('X','O',' ') |
| **raptor_con_memory** | `char cells[3][3]` (array C) | `char` ('X','O',' ') |
| **raptor_sin_memory** | `array<array<Cell,3>,3>` (STL) | `enum class Cell` |
| **tictactoe-grok** | `vector<vector<Cell>>` (STL) | `enum class Cell` |
| **tictactoe-raptor** | `array<Cell, 9>` (flat STL) | `enum class Cell` |

---

## 3. Tabla Comparativa de Características ncurses

| Criterio | grok_con | grok_sin | raptor_con | raptor_sin | **tictactoe-grok** | **tictactoe-raptor** |
|---|---|---|---|---|---|---|
| **mousemask** | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| **KEY_MOUSE** | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| **Caracteres ACS** | ✅ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **nodelay()** | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **use_default_colors** | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **keypad()** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

---

## 4. Tabla Comparativa de Funcionalidades de Juego

| Criterio | grok_con | grok_sin | raptor_con | raptor_sin | **tictactoe-grok** | **tictactoe-raptor** |
|---|---|---|---|---|---|---|
| **Selección directa 1-9** | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **Reinicio `r` (1 tablero)** | ❌ (r=R) | ❌ (r=R) | ❌ (r=R) | ✅ | ✅ | ✅ |
| **Reinicio `R` (todos)** | ❌ (r=R) | ❌ (r=R) | ❌ (r=R) | ✅ | ✅ | ✅ |
| **Wrap-around cursor** | ❌ | ✅ modulo | ❌ | ❌ | ✅ modulo | ❌ |
| **Tab cambio tablero** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Modo auto animado** | ❌ | ❌ | ❌ | ✅ nodelay | ✅ napms | ✅ nodelay |
| **Prevención doble conteo** | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **Ayuda `h` en juego** | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Tests unitarios** | ❌ | ❌ | ✅ (1) | ❌ | ✅ (6 asserts) | ✅ (1) |
| **Test integración** | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ (script) |
| **`make test`** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |

---

## 5. Tabla Comparativa de Calidad de Código

| Criterio | grok_con | grok_sin | raptor_con | raptor_sin | **tictactoe-grok** | **tictactoe-raptor** |
|---|---|---|---|---|---|---|
| **Encapsulación** | ✅ privado | ✅ privado | ❌ miembros públicos | ✅ todo privado | ✅ todo privado | ✅ todo privado |
| **Documentación Doxygen** | ❌ | ✅ completa | ❌ | ❌ | ❌ | ❌ |
| **Semilla aleatoria** | ❌ en Board ctor | ✅ en main | ❌ en Game ctor | ✅ random_device | ✅ random_device | ✅ random_device |
| **Motor aleatorio** | `rand()` + `srand()` | `rand()` + `srand()` | `std::rand()` + `srand()` | `std::mt19937` | `std::mt19937` | `std::mt19937` |
| **Objetos intermedios** | ✅ `obj/` | ✅ `obj/` | ❌ inline | ✅ `src/*.o` | ✅ `obj/` | ✅ `obj/` |
| **availableMoves()** | ❌ inline | ❌ inline | ❌ inline | ✅ método | ✅ método | ✅ método |
| **playerMove() central** | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |

---

## 6. Tabla Resumen Consolidada (✅ / ❌)

| # | Criterio | grok_con | grok_sin | raptor_con | raptor_sin | **t-grok** | **t-raptor** |
|---|---|---|---|---|---|---|---|
| 1 | Namespaces | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 2 | Enums tipados | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 3 | mt19937 | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 4 | std::optional | ❌ | ❌ | ❌ | ❌ | ✅ | ✅ |
| 5 | std::clamp | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ |
| 6 | C++17 | ❌ | ✅ | ❌ | ✅ | ✅ | ✅ |
| 7 | -O2 | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 8 | try/catch | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 9 | CXX correcto | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| 10 | Mouse | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| 11 | ACS chars | ✅ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 12 | nodelay | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 13 | use_default_colors | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 14 | Selección 1-9 | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 15 | Reinicio r/R separado | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 16 | Doble conteo prevenido | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| 17 | Tests | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| 18 | Doxygen | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ |
| 19 | Encapsulación | ✅ | ✅ | ❌ | ✅ | ✅ | ✅ |
| 20 | availableMoves() | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| | **TOTAL ✅** | **3/20** | **5/20** | **3/20** | **15/20** | **17/20** | **19/20** |

---

## 7. Ranking Final

| # | Proyecto | Puntuación | ✅ | Modelo | Condición |
|---|---|---|---|---|---|
| 🥇 | **tictactoe-raptor** | ⭐⭐⭐⭐⭐ | 19/20 | Raptor | Memory prescriptivo (2ª ronda) |
| 🥈 | **tictactoe-grok** | ⭐⭐⭐⭐½ | 17/20 | Grok | Memory prescriptivo (2ª ronda) |
| 🥉 | **raptor_sin_memory** | ⭐⭐⭐⭐ | 15/20 | Raptor | Sin Memory |
| 4º | **grok_sin_memory** | ⭐⭐⭐ | 5/20 | Grok | Sin Memory |
| 5º | **grok_con_memory** | ⭐⭐ | 3/20 | Grok | Memory descriptivo (1ª ronda) |
| 5º | **raptor_con_memory** | ⭐⭐ | 3/20 | Raptor | Memory descriptivo (1ª ronda) |

### Observaciones del Ranking

- **tictactoe-raptor** es el único proyecto que cumple 19 de 20 criterios. Solo le falta la documentación Doxygen.
- **tictactoe-grok** tiene 17/20 pero pierde en mouse (que tenía en versiones anteriores), std::clamp y Doxygen.
- **raptor_sin_memory** es el mejor de la 1ª ronda con 15/20, demostrando que Raptor tiene un fuerte conocimiento base de C++ moderno.
- Existe un **abismo** entre los proyectos de la 2ª ronda (17-19) y los de la 1ª ronda con Memory descriptivo (3).
- La documentación Doxygen solo la generó `grok_sin_memory`, y nunca más se repitió en ningún otro proyecto.

---

## 8. Análisis por Modelo

### 8.1 Evolución de Grok (3 versiones)

```
grok_con_memory (1ª)  →  grok_sin_memory  →  tictactoe-grok (2ª)
      3/20                    5/20                   17/20
```

| Capacidad nueva en 2ª ronda | ¿Existía antes en Grok? |
|---|---|
| Namespaces `ttt` | **Nunca** |
| enum class Cell/Result | **Nunca** |
| std::mt19937 | **Nunca** |
| std::optional | **Nunca** |
| Prevención doble conteo | **Nunca** |
| Tests unitarios | **Nunca** |
| Selección directa 1-9 | **Nunca** |
| Reinicio r/R separado | **Nunca** |
| nodelay / use_default_colors | **Nunca** |
| try/catch | **Nunca** |

**Impacto del Memory prescriptivo en Grok**: +14 criterios respecto a la 1ª ronda. El modelo pasó de 3/20 a 17/20. La única regresión fue la **pérdida del soporte de ratón** (que existía en ambas versiones anteriores).

### 8.2 Evolución de Raptor (3 versiones)

```
raptor_con_memory (1ª)  →  raptor_sin_memory  →  tictactoe-raptor (2ª)
        3/20                     15/20                    19/20
```

| Capacidad nueva en 2ª ronda | ¿Existía antes en Raptor? |
|---|---|
| std::optional | **Nunca** (ni con ni sin Memory) |
| std::clamp | **Nunca** |
| Test de integración (script) | **Nunca** |
| std::array<Cell,9> (flat) | **Nunca** (usaba array 2D o char) |

**Impacto del Memory prescriptivo en Raptor**: +16 criterios respecto a la 1ª ronda, +4 respecto a sin Memory. El salto fue menor que en Grok porque raptor_sin_memory ya era muy fuerte (15/20).

### 8.3 Grok vs Raptor: ¿Quién se beneficia más del Memory?

| Métrica | Grok | Raptor |
|---|---|---|
| Puntuación base (sin Memory) | 5/20 | 15/20 |
| Puntuación con Memory prescriptivo | 17/20 | 19/20 |
| **Ganancia absoluta** | **+12** | **+4** |
| **Ganancia relativa** | **+240%** | **+27%** |

> **Grok se beneficia mucho más del Memory prescriptivo** porque parte de una base más débil. Raptor ya aplica muchas buenas prácticas por sí solo.

---

## 9. Análisis por Condición Experimental

### 9.1 Memory Descriptivo (1ª ronda) — Peor resultado

| Proyecto | Puntuación | Problema |
|---|---|---|
| grok_con_memory | 3/20 | El Memory ancló al modelo a `char grid[3][3]`, `rand()`, C++11 |
| raptor_con_memory | 3/20 | Miembros públicos, `std::rand()`, sin enums, duplicación de lógica |

Los hechos descriptivos ("Board usa char grid[3][3]") actúan como **sesgo de anclaje**, forzando al modelo a replicar patrones mediocres.

### 9.2 Sin Memory — Resultado intermedio

| Proyecto | Puntuación | Fortaleza |
|---|---|---|
| grok_sin_memory | 5/20 | Única versión con Doxygen; C++17; STL (vector) |
| raptor_sin_memory | 15/20 | Diseño profesional: enums, namespaces, mt19937, encapsulación |

Sin restricciones externas, los modelos aplican libremente su conocimiento. Raptor demuestra un conocimiento intrínseco de C++ moderno muy superior al de Grok.

### 9.3 Memory Prescriptivo (2ª ronda) — Mejor resultado

| Proyecto | Puntuación | Logro |
|---|---|---|
| tictactoe-grok | 17/20 | 10 capacidades que Grok **nunca** había demostrado |
| tictactoe-raptor | 19/20 | Combina lo mejor de todas las versiones + novedades propias |

Las directivas `OBLIGATORIO` y `PREFERIR` actúan como **guía de buenas prácticas**, no como ancla.

### 9.4 Correlación Directivas OBLIGATORIO → Cumplimiento

| Directiva | grok_con | grok_sin | raptor_con | raptor_sin | **t-grok** | **t-raptor** |
|---|---|---|---|---|---|---|
| Usar enum class Cell | ❌ | ❌ | ❌ | ✅ (propio) | ✅ | ✅ |
| Usar enum class Result | ❌ | ❌ | ❌ | ✅ (propio) | ✅ | ✅ |
| Usar std::mt19937 | ❌ | ❌ | ❌ | ✅ (propio) | ✅ | ✅ |
| Compilar con C++17 | ❌ | ✅ | ❌ | ✅ | ✅ | ✅ |
| CXX = g++ (no CC) | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Miembros privados | ✅ | ✅ | ❌ | ✅ | ✅ | ✅ |
| availableMoves() | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| Mouse (mousemask) | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| Tests con make test | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| playerMove() central | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| Selección 1-9 + Tab | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| Reinicio r/R separado | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| nodelay + delay auto | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| **Cumplimiento** | **2/13** | **3/13** | **3/13** | **11/13** | **12/13** | **13/13** |

**Tasa de cumplimiento de directivas**: Grok 2ª ronda 92%, Raptor 2ª ronda **100%**.

---

## 10. Conclusiones

### 10.1 Conclusión Principal

> **El tipo de contenido del Memory determina si mejora o perjudica la calidad del código. Las observaciones descriptivas anclan al modelo a patrones mediocres; las directivas prescriptivas lo guían hacia buenas prácticas.**

```
Memory descriptivo:   3/20 (Grok) y  3/20 (Raptor) — PEOR resultado
Sin Memory:           5/20 (Grok) y 15/20 (Raptor) — resultado intermedio
Memory prescriptivo: 17/20 (Grok) y 19/20 (Raptor) — MEJOR resultado
```

### 10.2 Conclusiones Secundarias

1. **tictactoe-raptor es el proyecto más completo** (19/20): namespaces, enums tipados, mt19937, std::optional, std::clamp, std::array flat, mouse completo, ACS, nodelay, tests + script de integración.

2. **tictactoe-grok es la mayor sorpresa**: pasó de 3/20 a 17/20. El Memory prescriptivo hizo que Grok demostrara 10 capacidades que nunca había mostrado.

3. **Raptor tiene un conocimiento base de C++ moderno muy superior a Grok**: sin Memory, Raptor logra 15/20 vs 5/20 de Grok.

4. **Los modelos más débiles se benefician más del Memory prescriptivo**: Grok ganó +240% vs +27% de Raptor.

5. **La documentación Doxygen es la gran olvidada**: solo grok_sin_memory la generó. Ninguna directiva OBLIGATORIO la incluía, y ningún modelo la añadió por iniciativa propia en la 2ª ronda.

6. **El soporte de ratón es la única regresión de tictactoe-grok**: existía en grok_con_memory y grok_sin_memory, pero se perdió en la 2ª ronda. La directiva OBLIGATORIO de mousemask se cumplió en Raptor pero no en Grok.

---

## 11. Análisis de Ficheros .md del Repositorio: Relevancia y Recomendación

### 11.1 Inventario de ficheros .md y .txt en la raíz

| Fichero | Líneas | Descripción |
|---|---|---|
| `INFORME3.md` | 611 | Árbol de relaciones completo del grafo de conocimiento |
| `INFORME4.md` | 473 | Comparativa del impacto del MCP Memory (1ª y 2ª ronda) |
| `BUSQUEDA_MEMORY.md` | 274 | Registro de conversación explorando el grafo recursivamente |
| `GUIA_NAVEGACION_ARBOL.md` | 400 | Guía para navegar el grafo con memory-open_nodes |
| `NAVEGACION_RAPIDA.md` | 221 | Versión simplificada de la guía de navegación |
| `PROMPT_ARBOL_CONOCIMIENTO.txt` | 1 | Prompt para construir el árbol de conocimiento |
| `TEXTO_NAVEGACION.txt` | 1 | Instrucciones de navegación para modelos |

### 11.2 Inventario de ficheros .md en `docs/`

| Fichero | Líneas | Descripción |
|---|---|---|
| `docs/PRUEBAS_MCP.md` | 315 | Pruebas de Memory + Chrome DevTools (navegación UM) |
| `docs/CHROME_DEVTOOLS.md` | 633 | Pruebas del MCP Chrome DevTools |
| `docs/MCP_MEMORY.md` | 265 | Pruebas del MCP Memory |
| `docs/COPILOT_CLI.md` | 107 | Registro de extracción Ticketmaster |
| `docs/INFORME.md` | 144 | Comparativa gpt-5p2 con/sin memory + sonnet |
| `docs/INFORME2.md` | 190 | Comparativa de los 7 proyectos tictactoe_con_ncurses |
| `docs/MODELOS.md` | 163 | Información añadida al grafo de conocimiento |
| `docs/PROCESO_UNION_ARBOLES_MEMORY.md` | 417 | Proceso de unión de subgrafos en Memory |
| `docs/TicTacToe-ncurses-relations-tree.md` | 60 | Árbol de relaciones (versión corta) |

### 11.3 Clasificación por relevancia

#### ✅ CONSERVAR — Documentación final de resultados

| Fichero | Razón |
|---|---|
| `INFORME4.md` | **Informe principal de la investigación** (1ª y 2ª ronda). Contiene el análisis completo y las conclusiones sobre el impacto del Memory. |
| `INFORME5.md` | **Este informe**. Comparativa final consolidada de los 6 proyectos. |
| `INFORME3.md` | **Árbol de relaciones del grafo**. Documenta la estructura del conocimiento almacenado en Memory. Referencia complementaria a INFORME4. |
| `docs/INFORME.md` | **Primer informe** (gpt-5p2 + sonnet). Valor histórico como primera comparativa. |
| `docs/INFORME2.md` | **Segundo informe** (7 proyectos). Valor histórico. |

#### ✅ CONSERVAR — Documentación de pruebas MCP

| Fichero | Razón |
|---|---|
| `docs/PRUEBAS_MCP.md` | Pruebas combinadas de Memory + Chrome DevTools. Documenta el proceso experimental. |
| `docs/CHROME_DEVTOOLS.md` | Pruebas específicas del MCP Chrome DevTools. Documentación de referencia. |
| `docs/MCP_MEMORY.md` | Pruebas específicas del MCP Memory. Documentación de referencia. |

#### ⚠️ PRESCINDIBLES — Contenido redundante o auxiliar

| Fichero | Razón | Recomendación |
|---|---|---|
| `BUSQUEDA_MEMORY.md` | Registro de una conversación (log bruto). Su contenido está sintetizado en INFORME3.md. | 🗑️ Borrar |
| `GUIA_NAVEGACION_ARBOL.md` | Guía operativa para navegar el grafo. Ya no es necesaria si no se va a navegar el grafo interactivamente. Su contenido relevante está en INFORME3.md y INFORME4.md (sección 7.1). | 🗑️ Borrar |
| `NAVEGACION_RAPIDA.md` | Versión resumida de GUIA_NAVEGACION_ARBOL.md. Doblemente redundante. | 🗑️ Borrar |
| `PROMPT_ARBOL_CONOCIMIENTO.txt` | Prompt de 1 línea. Su contenido está documentado en INFORME4.md sección 7.1. | 🗑️ Borrar |
| `TEXTO_NAVEGACION.txt` | Instrucciones de 1 línea. Contenido en INFORME4.md sección 7.1. | 🗑️ Borrar |
| `docs/COPILOT_CLI.md` | Registro breve de extracción de Ticketmaster. Tangencial al proyecto. | 🗑️ Borrar |
| `docs/MODELOS.md` | Información sobre qué se añadió al grafo. Contenido cubierto por INFORME3 e INFORME4. | 🗑️ Borrar |
| `docs/PROCESO_UNION_ARBOLES_MEMORY.md` | Proceso técnico de unir subgrafos. Auxiliar, no resultado final. | 🗑️ Borrar |
| `docs/TicTacToe-ncurses-relations-tree.md` | Árbol de relaciones (60 líneas). Versión corta de INFORME3.md (611 líneas). Totalmente redundante. | 🗑️ Borrar |

### 11.4 Resumen de la recomendación

| Acción | Ficheros | Total |
|---|---|---|
| ✅ **Conservar** | INFORME3.md, INFORME4.md, INFORME5.md, docs/INFORME.md, docs/INFORME2.md, docs/PRUEBAS_MCP.md, docs/CHROME_DEVTOOLS.md, docs/MCP_MEMORY.md | **8 ficheros** |
| 🗑️ **Borrar** | BUSQUEDA_MEMORY.md, GUIA_NAVEGACION_ARBOL.md, NAVEGACION_RAPIDA.md, PROMPT_ARBOL_CONOCIMIENTO.txt, TEXTO_NAVEGACION.txt, docs/COPILOT_CLI.md, docs/MODELOS.md, docs/PROCESO_UNION_ARBOLES_MEMORY.md, docs/TicTacToe-ncurses-relations-tree.md | **9 ficheros** |

> **Criterio**: se conservan los informes finales de resultados y la documentación de pruebas de los MCPs. Se eliminan los ficheros auxiliares (logs de conversación, guías operativas, prompts, y documentos cuyo contenido ya está integrado en los informes).

---

**Fecha de generación**: 2026-02-17  
**Método**: Análisis comparativo de código fuente (6 proyectos, ~4.476 líneas), verificación de compilación, escaneo automatizado de características  
**Modelos evaluados**: Grok, Raptor (Claude)  
**Variables de estudio**: Memory descriptivo, sin Memory, Memory prescriptivo
