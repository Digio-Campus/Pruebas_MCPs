# Proyecto-MCPs: ¿Beneficia realmente la memoria a los modelos de IA?

## Descripción General

**Proyecto-MCPs** es una investigación experimental que busca responder una pregunta fundamental: **¿mejora la calidad del código generado por un modelo de IA cuando se le proporciona una memoria externa?**

Para responderla, se diseñaron experimentos comparativos con **múltiples modelos de IA** (GPT-5.2, Claude Sonnet 4.5, Claude Haiku 4.5, Claude Opus 4.6, Grok, Raptor, Gemini 3.1 Pro) aplicados a **dos dominios** diferentes:

1. **Desarrollo de software**: Juego TicTacToe en C++ con ncurses (interfaz de terminal)
2. **Traducción entre paradigmas**: Programas COBOL bancarios traducidos a Spring Boot (Java)

En cada experimento, el mismo modelo genera código **con y sin acceso a memoria externa** (MCP Memory, Remembrances), y se comparan los resultados en arquitectura, calidad, modularidad, seguridad y adherencia a buenas prácticas.

Además, se explora la integración de servidores **MCP (Model Context Protocol)** con **GitHub Copilot**, evaluando tanto la **automatización de navegador** (Chrome DevTools MCP) como la **persistencia de conocimiento** (Memory MCP / Remembrances MCP).

> **Conclusión principal:** La memoria mejora significativamente la calidad del código, pero solo cuando está correctamente diseñada. Una memoria *prescriptiva* (directivas accionables) supera a una *descriptiva* (hechos sobre lo existente), que puede incluso perjudicar al modelo.

Este proyecto se desarrolló como parte de unas prácticas externas universitarias.

**Características implementadas**:
- ✓ Menú principal con 4 opciones (Jugar, Ajustes, Ayuda, Salir)
- ✓ Tableros múltiples (1-12 simultáneos)
- ✓ Modos: 0 Jugadores, 1 Jugador, 2 Jugadores
- ✓ Detección automática de victorias y empates
- ✓ Estadísticas independientes por tablero
- ✓ Soporte de ratón (en variantes avanzadas)
- ✓ Redimensionamiento dinámico de terminal

---

## Índice

### Pruebas de MCPs
- [Pruebas del MCP chrome-devtools con GitHub Copilot](docs/CHROME_DEVTOOLS.md)
- [Pruebas del MCP memory con GitHub Copilot](docs/MCP_MEMORY.md)
- [Uso combinado de los MCPs chrome-devtools y memory](docs/PRUEBAS_MCP.md)
- [Prueba de funcionamiento de GitHub Copilot CLI con ambos MCPs](docs/COPILOT_CLI.md)

### Comparativas TicTacToe C++ (con y sin memoria)
- [Comparativa: GPT-5.2 sin/con memory vs Sonnet con aprendizaje](docs/INFORME.md)
- [Comparativa completa de los 7 proyectos TicTacToe con ncurses](docs/INFORME2.md)
- [Árbol de Relaciones del Grafo de Conocimiento para TicTacToe](docs/INFORME3.md)
- [Impacto del MCP Memory en Grok vs Raptor (memoria descriptiva y prescriptiva)](docs/INFORME4.md)
- [Mejora de la memoria con buenas prácticas C++ para TicTacToe](docs/INFORME5.md)
- [Análisis comparativo de proyectos TicTacToe con Claude Haiku](docs/INFORME6.md)

### Comparativas Traducción COBOL → Spring Boot (con y sin memoria)
- [Memory MCP para traducción COBOL → Spring Boot](docs/INFORME7.md)
- [Comparativa: Opus (con memory) vs GPT-5.2 (leyendo memory)](docs/INFORME8.md)
- [Comparativa: Haiku con vs sin memory en BANCO-INGRESOS](docs/INFORME9.md)
- [Evolución de la memoria semántica: Opus 4.6 vs Gemini 3.1 Pro](docs/INFORME10.md)
- [Traducción COBOL baseline vs memory-guided con Gemini 3.1 Pro (narrativo)](docs/INFORME11.md)
- [Comparativa técnica: Gemini 3.1 Pro baseline vs memory-guided (detallado)](docs/INFORME12.md)

### Metodología y Proceso
- [Proceso de unión de árboles con memory](docs/PROCESO_UNION_ARBOLES_MEMORY.md)

### Conclusiones
- [**Conclusión general: ¿Beneficia realmente la memoria a los modelos de IA?**](docs/CONCLUSION.md)

---

## Hallazgos Clave

### 1. El tipo de memoria importa más que su presencia

| Tipo de memoria | Efecto sobre la calidad del código |
|---|---|
| **Descriptiva** (hechos sobre lo existente) | Puede **perjudicar** — ancla al modelo a replicar patrones mediocres |
| **Prescriptiva** (directivas OBLIGATORIO/PREFERIR) | **Mejora significativamente** — guía hacia buenas prácticas |
| **Semántica granular** (vectores especializados) | **Mayor mejora observada** — inyección precisa según contexto |

### 2. Impacto cuantificado

| Métrica | Sin Memory | Con Memory (bien diseñada) |
|---|---|---|
| **Modularidad** | Monolítica (1-3 clases) | Separada (5-6 clases/capas) |
| **LOC** | Base | +42% a +158% (más estructura, no inflado) |
| **Adherencia a buenas prácticas** | Variable | Tasa de cumplimiento 92-100% |
| **Thread safety** (Spring Boot) | Roto (singleton mutable) | Correcto (stateless) |
| **Persistencia** (Spring Boot) | Hardcoded en memoria | JPA con `@Transactional` |

### 3. Los modelos más débiles se benefician más

Grok ganó 6 capacidades nuevas con memoria prescriptiva (namespaces, enums tipados, mt19937, tests) que nunca había demostrado sin ella. Raptor solo ganó 3, porque ya tenía muchas por defecto. La memoria **nivela** las diferencias entre modelos.

### 4. Comparativa de modelos

| Modelo | Fortaleza principal | Beneficio de la memoria |
|---|---|---|
| **GPT-5.2** | C++ moderno, modernización Java | Mejor modularidad (+100% clases) |
| **Claude Sonnet 4.5** | Documentación, features avanzadas (ratón) | Aprendizaje iterativo |
| **Claude Haiku 4.5** | Balance calidad/velocidad | +158% LOC, +5 clases, +6 docs |
| **Claude Opus 4.6** | Fidelidad al original, SRP | Generó memoria base para otros modelos |
| **Grok** | Generación rápida | Mayor salto cualitativo (6 capacidades nuevas) |
| **Raptor (Claude)** | Diseño profesional sin memoria | Consolidación de todas las mejores prácticas |
| **Gemini 3.1 Pro** | Traducción COBOL | Mayor delta baseline→memory de todos los modelos |

---

## Estructura del Repositorio

### `code/` — Implementaciones TicTacToe C++

#### TicTacToe con ncurses (comparativas con/sin memory)
- `tictactoe_con_ncurses/gpt-5p2_con_memory/` — GPT-5.2 con memoria
- `tictactoe_con_ncurses/gpt-5p2_sin_memory/` — GPT-5.2 sin memoria (baseline)
- `tictactoe_con_ncurses/gpt-5p2_con_memory&mouse/` — GPT-5.2 con memoria + ratón
- `tictactoe_con_ncurses/haiku_con_memory/` — Claude Haiku con memoria
- `tictactoe_con_ncurses/haiku_sin_memory/` — Claude Haiku sin memoria (baseline)
- `tictactoe_con_ncurses/sonnet_con_aprendizaje/` — Claude Sonnet con aprendizaje

#### TicTacToe Grok/Raptor (3 generaciones por modelo)
- `tictactoe_grok_raptor/grok_con_memory/` — Grok con memoria descriptiva (1ª ronda)
- `tictactoe_grok_raptor/grok_sin_memory/` — Grok sin memoria (baseline)
- `tictactoe_grok_raptor/tictactoe-grok/` — Grok con memoria prescriptiva (2ª ronda)
- `tictactoe_grok_raptor/raptor_con_memory/` — Raptor con memoria descriptiva (1ª ronda)
- `tictactoe_grok_raptor/raptor_sin_memory/` — Raptor sin memoria (baseline)
- `tictactoe_grok_raptor/tictactoe-raptor/` — Raptor con memoria prescriptiva (2ª ronda)

#### Fallos y primeros intentos
- `fallos/` — Versiones no funcionales o experimentales

### `translate/` — Traducciones COBOL → Spring Boot

- `banco-ingresos.cbl`, `banco-consulta-saldo.cbl`, etc. — Programas COBOL originales

### `translate/spring-boot`

- `spring-boot-banco/` — Traducción base con Haiku
- `spring-boot-banco-haiku_memory/` — Claude Haiku con memoria
- `spring-boot-banco-sonnet_remembrances/` — Claude Sonnet con Remembrances
- `spring-boot-banco-opus/` — Claude Opus 4.6 con memoria
- `spring-boot-gpt5_2_memory/` — GPT-5.2 con memoria
- `baseline_gemini_3_1_pro/` — Gemini 3.1 Pro sin memoria (baseline)
- `memory_gemini_3_1_pro/` — Gemini 3.1 Pro con Remembrances

### `docs/` — Informes y documentación

Contiene los 12 informes comparativos, pruebas de MCPs y la conclusión general.

### `data/` — Almacenamiento de sesiones MCP

- `memory.jsonl` — Grafo de conocimiento consolidado
- `memory_0.jsonl`, `memory_1.jsonl` — Logs incrementales

---

## Tecnologías Utilizadas

| Tecnología | Uso |
|---|---|
| **GitHub Copilot** | Asistente IA para generación de código |
| **MCP Memory** | Grafo de conocimiento persistente entre sesiones |
| **Remembrances MCP** | Memoria semántica vectorial con RAG |
| **Chrome DevTools MCP** | Automatización de navegador |
| **C++11/C++17** | TicTacToe con ncurses |
| **Java 17+ / Spring Boot 3.2** | Traducciones COBOL |
| **ncurses** | Interfaz de terminal para juegos |
| **Maven** | Build system Java |
| **Make** | Build system C++ |
| **H2 Database** | Base de datos en memoria (Spring Boot) |

---

## Cómo Navegar el Proyecto

### Para entender la investigación
1. Empezar por la [Conclusión general](docs/CONCLUSION.md)
2. Ver el [INFORME4](docs/INFORME4.md) — el experimento más completo (memoria descriptiva vs prescriptiva)
3. Ver el [INFORME12](docs/INFORME12.md) — comparativa técnica Gemini baseline vs memory

### Para ver comparativas de código
1. **TicTacToe**: Comparar `gpt-5p2_sin_memory/` vs `gpt-5p2_con_memory/` en `code/tictactoe_con_ncurses/`
2. **Spring Boot**: Comparar `baseline_gemini_3_1_pro/` vs `memory_gemini_3_1_pro/` en `translate/`

### Para aprender sobre MCPs
1. [Chrome DevTools MCP](docs/CHROME_DEVTOOLS.md) — automatización web
2. [Memory MCP](docs/MCP_MEMORY.md) — grafos de conocimiento
3. [Integración combinada](docs/PRUEBAS_MCP.md) — caso de uso completo

---

## Autor

Proyecto de prácticas externas — Universidad

## Licencia

Este proyecto es de uso académico/educativo
