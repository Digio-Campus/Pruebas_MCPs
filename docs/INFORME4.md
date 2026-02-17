# INFORME 4: Comparativa del Impacto del MCP Memory en la Generación de Código

## Objetivo

Analizar si el uso del servidor **MCP Memory** (grafo de conocimiento persistente) mejora la calidad del código generado por los modelos **Grok** y **Raptor** (Claude), comparando las versiones producidas **con** y **sin** acceso a Memory.

Los cuatro proyectos evaluados implementan el mismo juego: **Tres en Raya (TicTacToe) con ncurses en C++**, con soporte para múltiples tableros, varios modos de juego (0, 1 y 2 jugadores), controles de teclado y ratón, y menú de ajustes.

---

## 1. Resumen Estructural de los Proyectos

### 1.1 Ficheros y Líneas de Código

| Proyecto | Archivos fuente (.cpp + .h) | Líneas totales | Clases/Módulos | Tests |
|---|---|---|---|---|
| **grok_con_memory** | 11 | 604 | 5 (Board, Game, Menu, Settings, UI) | No |
| **grok_sin_memory** | 11 | 878 | 5 (Board, Game, Menu, Settings, UI) | No |
| **raptor_con_memory** | 12 (+1 test) | 582 (+25 test) | 5 (Board, Game, Menu, Settings, UI) | Sí (1 test) |
| **raptor_sin_memory** | 7 | 799 | 3 (Board, Game, UI) | No |

### 1.2 Estructura de Directorios

| Proyecto | Estructura |
|---|---|
| **grok_con_memory** | `include/` + `src/` + `obj/` + `bin/` — separación estándar |
| **grok_sin_memory** | `include/` + `src/` + `obj/` + `bin/` — separación estándar |
| **raptor_con_memory** | `include/` + `src/` + `bin/` + `tests/` — incluye directorio de tests |
| **raptor_sin_memory** | `include/` + `src/` + `bin/` — objetos generados dentro de `src/` |

---

## 2. Comparativa Detallada: Grok con Memory vs Grok sin Memory

### 2.1 Arquitectura y Diseño

| Aspecto | Grok con Memory | Grok sin Memory |
|---|---|---|
| **Modularidad** | 5 clases bien separadas | 5 clases bien separadas |
| **Encapsulación** | Miembros privados, interfaz pública | Miembros privados, interfaz pública |
| **Patrón MVC** | Implícito (Board=M, UI=V, Game=C) | Implícito (Board=M, UI=V, Game=C) |
| **Dependencias** | Game recibe UI& y Settings& por referencia | Game recibe Settings& y UI& por referencia |

**Observación**: Ambas versiones de Grok tienen una arquitectura prácticamente idéntica. No hay diferencias significativas en la separación de responsabilidades.

### 2.2 Calidad del Código

| Criterio | Grok con Memory | Grok sin Memory |
|---|---|---|
| **Documentación en headers** | Sin documentación Doxygen | ✅ Documentación Doxygen completa en todos los headers |
| **Estándar C++** | C++11 (`-std=c++11`) | C++17 (`-std=c++17`) |
| **Almacenamiento del grid** | `char grid[3][3]` (array C) | `std::vector<std::vector<char>>` (STL) |
| **Método makeAutoMove** | Retorna `void` | ✅ Retorna `bool` (más informativo) |
| **Acceso a estadísticas** | 3 getters separados (getXWins, getOWins, getDraws) | ✅ `getStats()` con parámetros por referencia |
| **Semilla aleatoria** | `srand()` dentro del constructor de Board | ✅ `srand()` en main (una sola vez) |
| **Inicialización Game** | Constructor crea boards directamente | ✅ Método `initializeBoards()` separado |
| **Método checkGameEnd** | Declarado pero vacío ("not used") | ✅ Implementado y funcional |
| **Manejo de ESC** | Solo `'q'`/`'Q'` para salir | ✅ `ESC` (código 27) + `'q'`/`'Q'` |
| **Navegación con wrap** | Sin wrap (se detiene en bordes) | ✅ Wrap-around en cursor y menús |
| **Menú ayuda** | Integrado en UI como método | ✅ Separado en Menu::showHelp() |
| **Variable Makefile** | `CC = g++` (incorrecto, CC es para C) | ✅ `CXX = g++` (correcto para C++) |

### 2.3 Interfaz de Usuario (UI)

| Aspecto | Grok con Memory | Grok sin Memory |
|---|---|---|
| **Colores** | 6 pares de colores definidos | 5 pares de colores definidos |
| **Layout de tableros** | Layout propio con struct BoardLayout | Layout basado en sqrt con cálculos inline |
| **Cursor visual** | Cambia color de celda | ✅ Muestra `[]` alrededor del cursor |
| **Estadísticas por tablero** | Pantalla separada de stats | ✅ Estadísticas integradas debajo de cada tablero |
| **Dibujo de bordes** | Caracteres ACS de ncurses | Espacios y coordenadas calculadas |
| **Ayuda en menú** | "Presione Q para salir" | ✅ Indicaciones contextuales en barra inferior |

### 2.4 Veredicto Grok

> **Grok SIN Memory produjo código de mayor calidad que Grok CON Memory.**

Sin Memory, Grok generó:
- Documentación Doxygen completa en todos los headers
- Estándar C++ más moderno (C++17 vs C++11)
- Mejor uso de la STL (vector<vector> vs arrays C)
- Métodos más informativos (bool vs void)
- Mejor manejo de errores y edge cases
- Variable de Makefile correcta (CXX vs CC)
- Código más idiomático y robusto

---

## 3. Comparativa Detallada: Raptor con Memory vs Raptor sin Memory

### 3.1 Arquitectura y Diseño

| Aspecto | Raptor con Memory | Raptor sin Memory |
|---|---|---|
| **Módulos** | 5 clases (Board, Game, Menu, Settings, UI) | 3 clases (Board, Game, UI) — Settings y Menu integrados |
| **Namespaces** | No usa namespaces | ✅ Namespace `ttt` para todo el proyecto |
| **Enums** | `char` para celdas y turnos | ✅ `enum class Cell` y `enum class Result` (tipado fuerte) |
| **Separación lógica** | Game contiene todo el flujo | ✅ Game gestiona lógica pura, UI gestiona toda la interfaz |
| **Encapsulación** | Miembros públicos (`turn`, `xWins`, etc.) | ✅ Todo privado con acceso controlado |
| **Estadísticas** | Miembros públicos en Board | ✅ `BoardStats` como struct separada en Game |
| **Motor aleatorio** | `std::rand()` con `srand()` | ✅ `std::mt19937` con `std::random_device` |
| **Tests** | ✅ 1 test unitario (test_mouse_logic.cpp) | No incluye tests |

### 3.2 Calidad del Código

| Criterio | Raptor con Memory | Raptor sin Memory |
|---|---|---|
| **Type safety** | `char` para todo | ✅ Enums tipados (`Cell::X`, `Result::Draw`) |
| **RAII** | Básico (init/shutdown) | ✅ Mejor gestión de recursos |
| **Duplicación de código** | ❌ Lógica de modo 2 duplicada (mouse + teclado) | ✅ `playerMove()` centraliza la lógica de modos |
| **Contabilización de resultados** | ❌ Puede contar doble (`checkAndUpdateStats`) | ✅ `maybeRecordResult()` con `lastRecordedResult_` evita duplicados |
| **Manejo de redimensionado** | `KEY_RESIZE` + `refreshSize()` | ✅ `KEY_RESIZE` + `resizeHandler()` + limpieza |
| **Modo auto (0 jug.)** | Relleno instantáneo sin animación | ✅ `nodelay()` + `updateAutoPlayStep(delay)` con animación |
| **Selección rápida** | Solo Tab para cambiar tablero | ✅ Teclas `1-9` para selección directa + Tab |
| **Reinicio** | `R` reinicia tablero actual | ✅ `r` reinicia actual, `R` reinicia todos |
| **Makefile** | Compila todos los .cpp sin objetos intermedios | ✅ Compilación con objetos intermedios y optimización `-O2` |
| **Compilación target test** | ✅ `make test` disponible | No incluye target de test |
| **Líneas de código** | 582 (más conciso) | 799 (más completo) |

### 3.3 Robustez y Patrones

| Patrón | Raptor con Memory | Raptor sin Memory |
|---|---|---|
| **Prevención de doble conteo** | No | ✅ `lastRecordedResult_` vector |
| **Movimiento forzado** | No | ✅ `makeMoveForced()` para IA |
| **Consulta de movimientos disponibles** | Cálculo inline | ✅ `availableMoves()` como método reutilizable |
| **Estado del juego** | `isFinished()` + `winner()` separados | ✅ `Result` enum con `Ongoing`, `X_Win`, `O_Win`, `Draw` |
| **Actualización de resultado** | Calculado por checkAndUpdateStats externo | ✅ `updateResult()` privado llamado internamente tras cada movimiento |
| **Manejo de errores** | Básico (validación de bounds) | ✅ `try/catch` en main + validaciones internas |
| **Consistencia de turno post-victoria** | No detiene alternancia de turno | ✅ Solo alterna turno si `result_ == Ongoing` |

### 3.4 Interfaz de Usuario

| Aspecto | Raptor con Memory | Raptor sin Memory |
|---|---|---|
| **Bordes de tablero** | Caracteres `'-'` y `'|'` simples | ✅ Caracteres ACS de ncurses (esquinas, líneas) |
| **Líneas internas del grid** | No dibuja separadores | ✅ Líneas internas entre celdas |
| **Cabecera de tablero** | Número de tablero + highlight | ✅ Número + estado actual (turno / ganador / empate) |
| **Menú con ratón** | No | ✅ Soporte de ratón en menú principal |
| **Colores de fichas** | 1 par para marks (amarillo) | ✅ Pares separados para X (rojo) y O (cyan) |
| **Cursor** | `A_REVERSE` básico | ✅ `mvchgat()` con `A_REVERSE | A_BOLD` |
| **Modo no-bloqueante** | No (bloquea en getch) | ✅ `nodelay()` para modo auto |
| **use_default_colors** | No | ✅ Permite herencia del fondo de terminal |
| **Ayuda contextual** | `'h'` no implementada en juego | ✅ `'h'` muestra ayuda durante el juego |
| **Menú de Settings** | Funcional pero sin opciones "Guardar/Cancelar" | ✅ Opciones claras de "Guardar y volver" / "Cancelar" |
| **Detección de ventana pequeña** | Mensaje genérico | ✅ Mensaje específico con instrucción |

### 3.5 Veredicto Raptor

> **Raptor SIN Memory produjo código significativamente más sofisticado que Raptor CON Memory.**

Sin Memory, Raptor generó:
- Diseño con enums tipados y namespaces (C++ moderno)
- Prevención de errores de doble conteo con patrón de registro
- Motor aleatorio profesional (`mt19937` vs `rand()`)
- Interfaz más rica con caracteres ACS, colores diferenciados y modo no-bloqueante
- Mejor encapsulación (todo privado vs miembros públicos)
- Lógica centralizada sin duplicación
- Detención correcta de alternancia de turno post-victoria

Sin embargo, Raptor CON Memory aportó:
- **Tests unitarios** (aunque solo 1 archivo)
- **Menor cantidad de código** (más conciso)
- **Target `make test`** en Makefile

---

## 4. Comparativa Cruzada: Todos los Proyectos

### 4.1 Ranking de Calidad General

| # | Proyecto | Puntuación | Justificación |
|---|---|---|---|
| 🥇 | **raptor_sin_memory** | ⭐⭐⭐⭐⭐ | Diseño más profesional: enums tipados, namespaces, mt19937, UI más rica, prevención de bugs |
| 🥈 | **grok_sin_memory** | ⭐⭐⭐⭐ | Buena documentación Doxygen, C++17, STL moderna, diseño sólido |
| 🥉 | **raptor_con_memory** | ⭐⭐⭐ | Funcional y conciso, pero miembros públicos, duplicación, sin enums tipados. Incluye tests |
| 4º | **grok_con_memory** | ⭐⭐⭐ | Funcional pero con CC incorrecto, sin docs, C++11, array C, checkGameEnd vacío |

### 4.2 Tabla Comparativa por Criterios

| Criterio | grok_con_memory | grok_sin_memory | raptor_con_memory | raptor_sin_memory |
|---|---|---|---|---|
| **Namespaces** | ❌ | ❌ | ❌ | ✅ |
| **Enums tipados** | ❌ | ❌ | ❌ | ✅ |
| **Documentación código** | ❌ | ✅ Doxygen | ❌ | ❌ |
| **Tests** | ❌ | ❌ | ✅ | ❌ |
| **Motor aleatorio moderno** | ❌ | ❌ | ❌ | ✅ mt19937 |
| **Prevención doble conteo** | ❌ | ❌ | ❌ | ✅ |
| **Encapsulación** | ✅ | ✅ | ❌ Miembros públicos | ✅ |
| **Manejo de errores** | Básico | Básico | Básico | ✅ try/catch |
| **Modo auto animado** | ✅ usleep | ✅ usleep | ❌ Instantáneo | ✅ nodelay + delay |
| **Caracteres ACS ncurses** | ✅ | ❌ | ❌ | ✅ |
| **Ratón en menú** | ❌ | ❌ | ❌ | ✅ |
| **Estándar C++** | C++11 | C++17 | C++11 | C++17 |
| **Optimización -O2** | ❌ | ❌ | ❌ | ✅ |
| **Compilación OBJ separados** | ✅ | ✅ | ❌ | ✅ |

---

## 5. Análisis: ¿Es Útil el MCP Memory para Generar Mejor Código?

### 5.1 Resultado Empírico

En ambos modelos, la versión **SIN Memory** generó código de mayor calidad:

```
Grok:   SIN Memory > CON Memory  (documentación, C++17, STL, robustez)
Raptor: SIN Memory > CON Memory  (enums, namespaces, encapsulación, mt19937, UI)
```

### 5.2 ¿Por qué el Memory NO mejoró el código?

El MCP Memory almacena hechos y relaciones sobre el proyecto en un grafo de conocimiento. Sin embargo, la naturaleza de la información almacenada no es la óptima para mejorar la generación de código:

1. **El Memory almacena conocimiento arquitectónico, no estilístico**: Los hechos como "Board-class gestiona un tablero 3x3" o "UI-class usa ncurses" son descriptivos pero no prescriben buenas prácticas de código (como usar `enum class` o `mt19937`).

2. **El contexto del Memory puede limitar la creatividad**: Al recibir información previa sobre la estructura del proyecto, el modelo puede tender a replicar el diseño existente en lugar de proponer mejores alternativas. El modelo sin Memory parte de cero y puede aplicar más libremente sus conocimientos sobre buenas prácticas de C++.

3. **Ruido informativo**: El grafo de conocimiento puede introducir información no relevante que consume tokens del contexto, dejando menos espacio para que el modelo aplique su conocimiento intrínseco sobre patrones de diseño y C++ moderno.

4. **Sesgo de anclaje**: El Memory actúa como un "ancla" que condiciona las decisiones del modelo. Si el grafo describe una implementación con `char` para celdas, el modelo con Memory tenderá a mantener esa representación, mientras que el modelo sin Memory puede elegir libremente `enum class`.

### 5.3 ¿Cuándo SÍ sería útil el Memory?

El Memory sí aporta valor en escenarios diferentes:

| Escenario | Utilidad del Memory |
|---|---|
| **Generación de código desde cero** (este caso) | ❌ Baja — puede limitar la calidad |
| **Mantenimiento y evolución de código existente** | ✅ Alta — conoce la arquitectura |
| **Debugging y corrección de bugs** | ✅ Alta — recuerda relaciones y dependencias |
| **Generación de tests** | ✅ Media — raptor_con_memory fue el único que generó tests |
| **Documentación del proyecto** | ✅ Alta — conoce la estructura |
| **Refactoring** | ✅ Alta — sabe qué depende de qué |
| **Onboarding de nuevos desarrolladores** | ✅ Alta — puede explicar el sistema |

### 5.4 El Caso de los Tests

Un hallazgo notable: **raptor_con_memory fue el único proyecto que incluyó tests** (`test_mouse_logic.cpp` + target `make test`). Esto sugiere que el Memory sí puede influir positivamente en la completitud del proyecto al recordar la importancia del testing, aunque no mejore la calidad intrínseca del código de producción.

---

## 6. Conclusiones

### 6.1 Conclusión Principal

> **Para la tarea de generar código desde cero, el MCP Memory no mejora la calidad del código producido por Grok ni por Raptor. De hecho, las versiones sin Memory fueron consistentemente superiores en diseño, robustez y uso de C++ moderno.**

### 6.2 Conclusiones Secundarias

1. **Raptor (Claude) sin Memory genera el código de mayor calidad** entre los cuatro proyectos, con diseño profesional que incluye namespaces, enums tipados, motor aleatorio moderno y mejor encapsulación.

2. **Grok sin Memory destaca en documentación**, siendo el único que generó documentación Doxygen completa en todos los headers.

3. **El Memory puede ser contraproducente para generación desde cero** al anclar el modelo a decisiones de diseño previas y consumir contexto.

4. **El Memory sí aporta valor para testing**: raptor_con_memory fue el único que incluyó tests unitarios.

5. **El Memory tiene su lugar en tareas de mantenimiento**, no de creación. Su valor está en recordar la arquitectura existente, no en mejorar el código nuevo.

### 6.3 Recomendación

| Tarea | ¿Usar Memory? |
|---|---|
| Generar un proyecto nuevo desde cero | ❌ No — dejar que el modelo use su conocimiento libremente |
| Añadir funcionalidad a un proyecto existente | ✅ Sí — el Memory recuerda la arquitectura |
| Corregir bugs | ✅ Sí — el Memory recuerda dependencias |
| Generar tests para código existente | ✅ Sí — el Memory conoce los componentes |
| Refactorizar | ✅ Sí — el Memory sabe qué impacta a qué |
| Documentar | ✅ Sí — el Memory tiene la visión completa |

---

## 7. Segunda Ronda: Memory Prescriptivo con Relaciones Incrustadas

### 7.1 Contexto

Tras el análisis de la primera ronda (secciones 1–6), se identificó que el MCP Memory perjudicaba la calidad del código por dos motivos:

1. **Sesgo de anclaje**: Las observaciones descriptivas ("Board usa char grid[3][3]") condicionaban a los modelos a replicar patrones mediocres en lugar de aplicar sus conocimientos de C++ moderno.
2. **Navegación rota**: La herramienta `memory-open_nodes` no devuelve relaciones (el campo `relations` siempre viene vacío), por lo que los modelos no podían navegar el grafo recursivamente como indicaba el prompt.

Para la segunda ronda se aplicaron dos correcciones:

- **Observaciones prescriptivas**: Se reemplazaron los hechos descriptivos por directivas con marcadores `OBLIGATORIO` y `PREFERIR` (ej. "OBLIGATORIO usar enum class Cell { X, O, Empty }", "OBLIGATORIO usar std::mt19937 con std::random_device").
- **Relaciones incrustadas en observaciones**: Se añadió a cada entidad una observación con formato `RELACIONES SALIENTES: tipo→entidad` y `RELACIONES ENTRANTES: entidad→tipo`, permitiendo navegar el grafo con `open_nodes` sin necesidad de `read_graph` ni entidades guía auxiliares.
- **Nuevo prompt de navegación**: Se instruyó a los modelos a leer las observaciones de `TicTacToe-ncurses`, identificar las entidades en `RELACIONES SALIENTES`, abrirlas recursivamente, y seguir las directivas `OBLIGATORIO`/`PREFERIR` encontradas.

Los proyectos resultantes son `tictactoe-grok` (Grok con Memory prescriptivo) y `tictactoe-raptor` (Raptor con Memory prescriptivo).

### 7.2 Datos Estructurales de la Segunda Ronda

| Proyecto | Archivos fuente (.cpp + .h) | Líneas totales | Clases/Módulos | Tests |
|---|---|---|---|---|
| **tictactoe-grok** (Memory prescriptivo) | 12 (+1 test) | 891 (+41 test) | 5 (Board, Game, Menu, Settings, UI) | ✅ Sí (test_board.cpp) |
| **tictactoe-raptor** (Memory prescriptivo) | 11 (+1 test +1 script) | 698 (+24 test) | 5 (Board, Game, Menu, Settings, UI) | ✅ Sí (test_mouse_logic.cpp + test_integration.sh) |

### 7.3 Comparativa: Grok — Tres Generaciones

| Criterio | grok_con_memory (1ª ronda) | grok_sin_memory | **tictactoe-grok** (2ª ronda) |
|---|---|---|---|
| **Namespaces** | ❌ | ❌ | ✅ `namespace ttt` |
| **Enums tipados** | ❌ char | ❌ char | ✅ `enum class Cell`, `enum class Result` |
| **Motor aleatorio** | ❌ rand()/srand() | ❌ rand()/srand() | ✅ std::mt19937 + random_device |
| **Estándar C++** | C++11 | C++17 | C++17 |
| **Variable Makefile** | ❌ CC = g++ | ✅ CXX = g++ | ✅ CXX = g++ |
| **Optimización** | ❌ | ❌ | ✅ -O2 |
| **Tests** | ❌ | ❌ | ✅ test_board.cpp (6 tests) |
| **try/catch** | ❌ | ❌ | ✅ en main.cpp |
| **std::optional** | ❌ | ❌ | ✅ lastRecordedResult_ |
| **Prevención doble conteo** | ❌ | ❌ | ✅ lastRecordedResult_ |
| **nodelay (modo auto)** | ❌ | ❌ | ✅ napms(500) |
| **Documentación Doxygen** | ❌ | ✅ | ❌ |
| **Mouse** | ✅ | ✅ | ❌ |
| **Caracteres ACS** | ✅ | ❌ | ✅ |
| **Selección directa 1-9** | ❌ | ❌ | ✅ |
| **Reinicio individual+global** | ❌ | ❌ | ✅ r/R |
| **LOC** | 604 | 878 | 891 |

**Veredicto Grok 2ª ronda**: El Memory prescriptivo produjo una **mejora drástica** respecto a la 1ª ronda y supera también a la versión sin Memory en la mayoría de criterios técnicos. `tictactoe-grok` es el primer proyecto de Grok que usa namespaces, enums tipados, mt19937, std::optional, y prevención de doble conteo — ninguna de estas características apareció en las dos versiones anteriores. La única regresión es la **pérdida de soporte de ratón** y la **ausencia de documentación Doxygen** (que sí tenía grok_sin_memory).

### 7.4 Comparativa: Raptor — Tres Generaciones

| Criterio | raptor_con_memory (1ª ronda) | raptor_sin_memory | **tictactoe-raptor** (2ª ronda) |
|---|---|---|---|
| **Namespaces** | ❌ | ✅ `ttt` | ✅ `namespace ttt` |
| **Enums tipados** | ❌ char | ✅ `enum class` | ✅ `enum class Cell`, `enum class Result` |
| **Motor aleatorio** | ❌ std::rand() | ✅ mt19937 | ✅ std::mt19937 + random_device |
| **Estándar C++** | C++11 | C++17 | C++17 |
| **Optimización** | ❌ | ✅ -O2 | ✅ -O2 |
| **Tests** | ✅ test_mouse_logic | ❌ | ✅ test_mouse_logic + test_integration.sh |
| **try/catch** | ❌ | ✅ | ✅ en main.cpp |
| **std::optional** | ❌ | ❌ | ✅ getCellFromCoord, mapClickToCell |
| **std::clamp** | ❌ | ❌ | ✅ en Settings.cpp |
| **Mouse completo** | ✅ | ✅ | ✅ mousemask + getmouse + mapClickToCell |
| **Prevención doble conteo** | ❌ | ✅ | ✅ (implícito vía Result tracking) |
| **nodelay (modo auto)** | ❌ instantáneo | ✅ | ✅ |
| **Selección directa 1-9** | ❌ | ✅ | ✅ |
| **Reinicio r/R** | ❌ solo R | ✅ | ✅ r/R |
| **Encapsulación** | ❌ miembros públicos | ✅ todo privado | ✅ todo privado |
| **Hit testing puro** | ❌ | ❌ | ✅ getCellFromCoord() inline pura |
| **Test integración** | ❌ | ❌ | ✅ test_integration.sh |
| **LOC** | 606 | 799 | 698 |

**Veredicto Raptor 2ª ronda**: `tictactoe-raptor` combina **lo mejor de ambas versiones anteriores** y añade mejoras propias. Iguala a raptor_sin_memory en enums, namespaces, mt19937 y encapsulación, pero además incluye tests (como raptor_con_memory) y añade novedades como `std::optional`, `std::clamp`, funciones puras de hit-testing, y un script de integración. Mantiene el soporte completo de ratón. Es el proyecto más equilibrado de los seis.

### 7.5 Ranking Actualizado (6 proyectos)

| # | Proyecto | Puntuación | Cambio vs 1ª ronda |
|---|---|---|---|
| 🥇 | **tictactoe-raptor** (Memory prescriptivo) | ⭐⭐⭐⭐⭐ | 🆕 Nuevo #1 |
| 🥈 | **tictactoe-grok** (Memory prescriptivo) | ⭐⭐⭐⭐½ | 🆕 Nuevo #2 |
| 🥉 | **raptor_sin_memory** | ⭐⭐⭐⭐ | Baja de #1 a #3 |
| 4º | **grok_sin_memory** | ⭐⭐⭐½ | Baja de #2 a #4 |
| 5º | **raptor_con_memory** (1ª ronda) | ⭐⭐⭐ | Se mantiene |
| 6º | **grok_con_memory** (1ª ronda) | ⭐⭐½ | Se mantiene |

### 7.6 Tabla Comparativa Completa (6 proyectos)

| Criterio | grok_con (1ª) | grok_sin | **grok (2ª)** | raptor_con (1ª) | raptor_sin | **raptor (2ª)** |
|---|---|---|---|---|---|---|
| **Namespaces** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **Enums tipados** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **mt19937** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **C++ estándar** | C++11 | C++17 | C++17 | C++11 | C++17 | C++17 |
| **-O2** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **Tests** | ❌ | ❌ | ✅ | ✅ | ❌ | ✅ |
| **Mouse** | ✅ | ✅ | ❌ | ✅ | ✅ | ✅ |
| **try/catch** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **std::optional** | ❌ | ❌ | ✅ | ❌ | ❌ | ✅ |
| **Doble conteo** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **nodelay** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **Selección 1-9** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **Reinicio r/R** | ❌ | ❌ | ✅ | ❌ | ✅ | ✅ |
| **Doxygen** | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ |
| **ACS chars** | ✅ | ❌ | ✅ | ❌ | ✅ | ❌ |
| **CXX correcto** | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **LOC** | 604 | 878 | 891 | 606 | 799 | 698 |

### 7.7 Análisis: ¿Qué cambió con el Memory Prescriptivo?

#### 7.7.1 Indicadores que pasaron de ❌ a ✅ (mejoras directamente atribuibles al Memory)

**Para Grok** (6 indicadores nuevos):
- Namespaces `ttt` — **nunca** apareció en ninguna versión anterior de Grok
- Enums tipados — **nunca** en Grok antes
- mt19937 — **nunca** en Grok antes
- Tests unitarios — **nunca** en Grok antes
- std::optional — **nunca** en Grok antes
- Prevención doble conteo — **nunca** en Grok antes

**Para Raptor** (3 indicadores nuevos respecto a raptor_con_memory):
- std::optional — no existía en ninguna versión anterior de Raptor
- std::clamp — no existía en ninguna versión anterior
- Test de integración (script bash) — novedad absoluta

#### 7.7.2 Indicadores que se mantuvieron ❌ (limitaciones persistentes)

**Para Grok**:
- Soporte de ratón — curiosamente perdido en esta versión (existía en las dos anteriores)
- Documentación Doxygen — solo grok_sin_memory la generó

**Para Raptor**:
- Caracteres ACS de ncurses — solo raptor_sin_memory los usó
- Documentación Doxygen — ninguna versión de Raptor la genera

#### 7.7.3 Correlación con directivas OBLIGATORIO del Memory

Las directivas prescriptivas que se cumplieron:

| Directiva OBLIGATORIO | Grok 2ª | Raptor 2ª |
|---|---|---|
| Usar enum class Cell { X, O, Empty } | ✅ | ✅ |
| Usar enum class Result | ✅ | ✅ |
| Usar std::mt19937 con random_device | ✅ | ✅ |
| Compilar con -std=c++17 | ✅ | ✅ |
| CXX = g++ (no CC) | ✅ | ✅ |
| Miembros privados, getters | ✅ | ✅ |
| availableMoves() reutilizable | ✅ | ✅ |
| Soporte de ratón (mousemask) | ❌ | ✅ |
| Tests unitarios con make test | ✅ | ✅ |
| playerMove() centralizado | ✅ | ✅ |
| Selección directa 1-9 + Tab | ✅ | ✅ |
| Reinicio individual r y global R | ✅ | ✅ |
| nodelay + delay animado en modo auto | ✅ | ✅ |

**Tasa de cumplimiento**: Grok 12/13 (92%), Raptor 13/13 (100%).

### 7.8 Conclusiones de la Segunda Ronda

#### 7.8.1 Conclusión Principal

> **El Memory prescriptivo con relaciones incrustadas invierte completamente el resultado de la primera ronda: ahora las versiones CON Memory superan a las versiones SIN Memory en ambos modelos.**

```
Primera ronda:  SIN Memory > CON Memory (en ambos modelos)
Segunda ronda:  CON Memory prescriptivo > SIN Memory (en ambos modelos)
```

#### 7.8.2 El problema no era el Memory, era su contenido

La primera ronda demostró que almacenar hechos descriptivos perjudica la calidad del código. La segunda ronda demuestra que almacenar **directivas prescriptivas** (qué hacer, no qué existe) produce código significativamente mejor. La diferencia clave es:

| Tipo de observación | Ejemplo | Efecto en el modelo |
|---|---|---|
| **Descriptiva** (1ª ronda) | "Board usa char grid[3][3]" | Ancla al modelo a replicar esa decisión |
| **Prescriptiva** (2ª ronda) | "OBLIGATORIO usar enum class Cell" | Guía al modelo hacia mejores prácticas |

#### 7.8.3 La navegación importa tanto como el contenido

La corrección de la navegación (relaciones incrustadas en observaciones) fue crucial. Sin ella, los modelos no podían recorrer el grafo y solo accedían a las entidades que el prompt listaba explícitamente. Con la navegación funcional, los modelos descubren las directivas por sí mismos siguiendo las relaciones desde `TicTacToe-ncurses`.

#### 7.8.4 El impacto es mayor en Grok que en Raptor

Grok ganó **6 capacidades nuevas** que nunca había demostrado (namespaces, enums, mt19937, tests, std::optional, prevención doble conteo). Raptor ganó 3 capacidades nuevas pero ya tenía muchas en su versión sin Memory. Esto sugiere que **los modelos con menos conocimiento de base se benefician más** del Memory prescriptivo.

#### 7.8.5 Recomendación revisada

| Tarea | ¿Usar Memory? | Tipo de observaciones |
|---|---|---|
| Generar proyecto nuevo | ✅ **Sí** | Prescriptivas (OBLIGATORIO/PREFERIR) |
| Mantenimiento de código existente | ✅ Sí | Descriptivas + prescriptivas |
| Generación de tests | ✅ Sí | Prescriptivas (directivas de testing) |
| Debugging | ✅ Sí | Descriptivas (arquitectura actual) |

> **Nota**: La recomendación de la sección 5.3 ("no usar Memory para generar desde cero") queda **invalidada** por los resultados de la segunda ronda. El Memory sí mejora la generación desde cero, siempre que contenga directivas prescriptivas, no descripciones del código existente.

---

**Fecha de generación**: 2026-02-16  
**Método**: Análisis comparativo de código fuente (6 proyectos, ~4.476 líneas analizadas)  
**Modelos evaluados**: Grok, Raptor (Claude)  
**Variables de estudio**: Presencia/ausencia del MCP Memory; tipo de observaciones (descriptivas vs prescriptivas)
