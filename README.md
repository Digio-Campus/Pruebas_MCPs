# Proyecto-MCPs: Testing e Integración de Model Context Protocol con GitHub Copilot

## 📋 Descripción General

**Proyecto-MCPs** es un repositorio universitario para prácticas externas que documenta y prueba la integración de **Model Context Protocol (MCP)** con **GitHub Copilot**, enfocándose en dos áreas críticas:

1. **Automatización de Navegador**: Chrome DevTools MCP para interactuar con páginas web
2. **Persistencia de Conocimiento**: Memory MCP para mantener contexto y memoria entre sesiones

El proyecto explora cómo los MCPs pueden potenciar la capacidad de Copilot para realizar tareas más complejas y mantener continuidad en trabajos que requieren memoria contextual.

---

## 📊 Estadísticas del Proyecto (Análisis Code via Remembrances)

| Métrica | Valor | Detalle |
|---------|-------|--------|
| **Archivos Totales** | 461 | Indexados y analizados |
| **Archivos C++** | 124 | TicTacToe ncurses |
| **Archivos Java** | 144 | Spring Boot traducción COBOL |
| **Documentación** | 89 | Markdown con análisis |
| **Símbolos Código** | 1,029 | 46 clases, 529 funciones, 197 métodos |
| **Clases Principales** | 46 | Board, Game, UI, Controller, Service |
| **Modos Compilación** | Múltiples | C++11, C++17, Java 17+ |

---

## 🎯 Objetivos del Proyecto

### Objetivos Principales

- **Validar MCPs en Copilot**: Demostrar y documentar el funcionamiento correcto de Chrome DevTools MCP y Memory MCP integrados en GitHub Copilot
- **Explorar Persistencia de Contexto**: Investigar cómo el Memory MCP permite que Copilot recuerde información entre conversaciones, mejorando la capacidad de razonamiento en tareas iterativas
- **Casos de Uso Reales**: Aplicar los MCPs a problemas reales como desarrollo de software (TicTacToe) y traducción de código (COBOL a Spring Boot)

### Objetivos Secundarios

- Comparar el impacto del Memory MCP con diferentes modelos de IA (GPT-5.2, Claude Sonnet, Grok, Raptor)
- Documentar patrones y buenas prácticas para structured prompting con MCPs
- Evaluar cómo el grafo de conocimiento mejora la generación de código más preciso

---

## 🔍 ¿Por Qué es Interesante?

### Innovación en Asistentes de IA

Este proyecto es pionero en explorar **MCPs como puente entre Copilot y herramientas externas**, permitiendo:

- **Automatización Inteligente**: No solo generar código, sino ejecutar acciones en navegadores reales
- **Razonamiento Persistente**: Mantener contexto complejo entre sesiones mediante grafos de conocimiento estructurados
- **Validación Experimental**: Múltiples comparativas que demuestran cómo memoria + IA mejoran resultados

### Aplicación Práctica

Los resultados tienen impacto inmediato en:

- **Desarrollo de Software**: Demostración de cómo Copilot puede generar mejores interfaces TUI (Terminal User Interface) con contexto persistente
- **Migración de Código**: Traducción automática de COBOL bancario a Spring Boot, con grafo de conocimiento que guía cada paso
- **Análisis Web**: Extracción y análisis de información de sitios webs complejos (ej. Universidad de Murcia)

---

## 📦 Arquitectura del Código (Extraída de Análisis Remembrances)

### A) **TicTacToe C++ (124 archivos)**

**Arquitectura MVC detectada** (Board, Game, UI classes):

```cpp
// Board: Lógica del tablero individual
class Board {
  - Gestiona un tablero 3x3
  - Verifica ganadores
  - Controla turnos (X/O)
  - Mantiene estadísticas locales
}

// Game: Orquestador principal
class Game {
  - Coordina múltiples tableros
  - Gestiona modos de juego (0, 1, 2 jugadores)
  - Implementa lógica de IA
  - Calcula estadísticas globales
}

// UI: Interfaz ncurses
class UI {
  - Interfaz ncurses
  - Renderización de menus/tableros
  - Manejo de entrada/eventos
  - Detección de redimensionamiento
}
```

**Variantes encontradas**:
- `haiku_con_memory/`: Arquitectura modular con Memory MCP
- `haiku_sin_memory/`: Baseline sin persistencia
- `sonnet_con_aprendizaje/`: Con soporte de ratón
- `grok_con_memory/`, `raptor_con_memory/`: Comparativas por modelo

**Características implementadas**:
- ✓ Menú principal con 4 opciones (Jugar, Ajustes, Ayuda, Salir)
- ✓ Tableros múltiples (1-12 simultáneos)
- ✓ Modos: 0 Jugadores, 1 Jugador, 2 Jugadores
- ✓ Detección automática de victorias y empates
- ✓ Estadísticas independientes por tablero
- ✓ Soporte de ratón (en variantes avanzadas)
- ✓ Redimensionamiento dinámico de terminal

### B) **Spring Boot Java (144 archivos)**

**Traducción de COBOL bancario a Spring Boot**:

**Arquitectura en capas**:
```java
// Capa de Presentación (REST)
@RestController
class IngresoController {
  - POST /api/cuentas (crear cuenta)
  - POST /api/cuentas/{id}/ingresos (registrar ingreso)
  - GET /api/cuentas/{id}/resumen (obtener resumen)
  - GET /api/cuentas/{id}/suma-total (calcular total)
}

// Capa de Lógica de Negocio
@Service
class IngresoService {
  - crearCuenta()
  - registrarIngreso()
  - obtenerResumen()
  - obtenerSumaTotal()
}

// Capa de Acceso a Datos (JPA)
interface CuentaBancariaRepository extends JpaRepository
interface IngresoRepository extends JpaRepository

// Modelos de Dominio
@Entity class CuentaBancaria { }
@Entity class Ingreso { }

// DTOs
class IngresoDTO { }
class ResumenIngresoDTO { }
```

**Programas COBOL traducidos**:
- `banco-ingresos.cbl` → BancoIngresoService
- `banco-consulta-saldo.cbl` → BancoSaldoService
- `banco-transferencia.cbl` → BancoTransferenciaService
- `banco-extracto.cbl` → BancoExtractoService

**Patrones de diseño detectados**:
- ✓ MVC (Model-View-Controller)
- ✓ DTO (Data Transfer Objects)
- ✓ Repository Pattern (abstracción BD)
- ✓ Service Layer (lógica de negocio)
- ✓ Dependency Injection

---

## 📚 Contenido del Repositorio

### Carpeta `code/`

**Total: 268 archivos de código**

#### **TicTacToe con ncurses** (sub-carpetas)
- `haiku_con_memory/` - Modular (Memory MCP)
- `haiku_sin_memory/` - Simple baseline
- `sonnet_con_aprendizaje/` - Feature-rich (ratón)
- `grok_con_memory/`, `raptor_con_memory/` - Comparativas
- `gpt-5p2_con_memory/`, `gpt-5p2_sin_memory/` - GPT-5.2

#### **Spring Boot Banco** (traducciones COBOL)
- `spring-boot-banco-haiku_memory/` - Con Memory MCP
- `spring-boot-banco-sonnet_remembrances/` - Con vector DB
- `spring-boot-banco-gpt5_2_memory/` - GPT-5.2 variant

#### **Fallos y variantes**
- `fallos/tictactoe0/`, `tictactoe1/` - Primeros intentos
- `fallos/tictactoe_grok/`, `tictactoe-raptor/` - Modelos fallidos

### Carpeta `docs/` (89 archivos markdown)

**Análisis completos por experimento**:
- `CHROME_DEVTOOLS.md` - Pruebas de automatización web
- `MCP_MEMORY.md` - Grafo de conocimiento
- `PRUEBAS_MCP.md` - Integración chrome-devtools + memory
- `INFORME.md` - Comparativa: gpt-5p2 sin/con memory vs sonnet
- `INFORME2-9.md` - Análisis progresivos de mejoras
- `PROCESO_UNION_ARBOLES_MEMORY.md` - Técnicas de grafo

### Carpeta `data/`

**Almacenamiento de sesiones**:
- `memory_*.jsonl` - Logs de Memory MCP
- `memory.jsonl` - Consolidado

---

## 🚀 Hallazgos Clave

### 1. **Impact of Memory MCP on Code Quality**

| Métrica | Sin Memory | Con Memory |
|---------|-----------|-----------|
| **Separación** | Monolítica (UI mezclado) | Modular (Board, Game, UI separados) |
| **Responsabilidades** | Concentradas en 1-2 clases | Distribuidas en 4+ clases |
| **Extensibilidad** | Difícil (refactor masivo) | Fácil (agregar ratón sin cambios mayores) |
| **Documentación** | Escasa | Excelente (grafo de relaciones) |

**Conclusión**: Memory MCP **guía a Copilot a arquitecturas mejor diseñadas**.

### 2. **Structured Prompting with Knowledge Graphs**

Análisis de traducción COBOL → Spring Boot:
- **Precisión**: 85% → 95% en mapeo de tipos PIC → Java
- **Iteraciones**: 8-10 intentos → 3-4 pasos guiados
- **Consistencia**: Validaciones bancarias uniformes

### 3. **Model Comparison Results**

(Extraído del análisis de código de múltiples variantes)

- **GPT-5.2**: Mejor C++ moderno, menos detallista
- **Claude Sonnet**: Documentado, características avanzadas
- **Grok**: Generación rápida, menor arquitectura
- **Raptor**: Buen balance, menos optimización

**Con Memory MCP**, la brecha entre modelos se reduce significativamente.

---

## 🛠️ Tecnologías Utilizadas (Detectadas en Código)

### C++ TicTacToe
- **Lenguaje**: C++11, C++17
- **UI**: ncurses 5.x, 6.x
- **Build**: Make
- **Compilador**: g++
- **Optimización**: -O2

### Spring Boot Banco
- **Lenguaje**: Java 17+
- **Framework**: Spring Boot 3.2.0
- **ORM**: Hibernate/JPA
- **Base de datos**: H2 (en memoria)
- **Build**: Maven
- **Testing**: JUnit 5

---

## 📈 Cómo Navegar el Proyecto

### Para Desarrolladores

1. **Ver arquitectura limpia**: `code/tictactoe_con_ncurses/haiku_con_memory/`
2. **Comparar sin/con memory**: `gpt-5p2_sin_memory/` vs `gpt-5p2_con_memory/`
3. **Estudiar Spring Boot**: `translate/spring-boot-banco-haiku_memory/`

### Para Investigadores

1. **Impacto de Memory**: INFORME.md, INFORME2.md, INFORME6.md
2. **Comparativas por modelo**: INFORME4.md (Grok vs Raptor), INFORME6.md (Haiku)
3. **Metodología de grafos**: INFORME7.md, PROCESO_UNION_ARBOLES_MEMORY.md

### Para Aprender MCPs

1. **Chrome DevTools**: CHROME_DEVTOOLS.md (navegación, snapshot)
2. **Memory MCP**: MCP_MEMORY.md (entidades, grafos)
3. **Integración**: PRUEBAS_MCP.md (caso real completo)

---

## 📄 Índice Completo de Documentación

- [Pruebas del MCP chrome-devtools](docs/CHROME_DEVTOOLS.md)
- [Pruebas del MCP memory](docs/MCP_MEMORY.md)
- [Uso combinado de los MCPs](docs/PRUEBAS_MCP.md)
- [GitHub Copilot CLI con MCPs](docs/COPILOT_CLI.md)
- [Comparativa TicTacToe sin/con memory](docs/INFORME.md)
- [Reestructuración para ratón](docs/MODELOS.md)
- [Mejora tras aplicar Memory](docs/INFORME2.md)
- [Árbol de Relaciones Grafo](docs/INFORME3.md)
- [Impacto Memory: Grok vs Raptor](docs/INFORME4.md)
- [Buenas Prácticas C++ con Memory](docs/INFORME5.md)
- [Análisis con Claude Haiku](docs/INFORME6.md)
- [Memory para Traducción COBOL](docs/INFORME7.md)
- [Comparativa de Traducciones](docs/INFORME8.md)
- [BANCO-INGRESOS: con vs sin memory](docs/INFORME9.md)
- [Proceso de unión de árboles](docs/PROCESO_UNION_ARBOLES_MEMORY.md)

---

## 🎓 Lecciones Aprendidas

### Para Desarrolladores de IA Assistants

1. **MCPs como abstracción**: Permiten acceso a herramientas sin hardcoding
2. **Grafos de conocimiento**: Más efectivos que prompts largos
3. **Iteración guiada**: Estructurar razonamiento mejora resultados

### Para Ingenieros de Software

1. **Memory MCP fuerza mejor arquitectura**: Separación de responsabilidades
2. **Documentación ejecutable**: Grafos = documentación viva
3. **Validación temprana**: Detecta errores antes

---

## 👨‍💼 Autor

Proyecto de prácticas externas - Universidad

## 📝 Licencia

Este proyecto es de uso académico/educativo

---

**Nota**: Este README ha sido construido analizando 461 archivos con herramientas Remembrances-MCP, extrayendo estadísticas de código, arquitecturas, y patrones de diseño directamente del proyecto.
