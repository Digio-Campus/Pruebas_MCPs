# INFORME 3: Árbol de Relaciones del Grafo de Conocimiento

## Entidad Raíz: TicTacToe-ncurses

A continuación se presenta el árbol completo de relaciones partiendo de la entidad **TicTacToe-ncurses**, explorando recursivamente todas las conexiones del grafo de conocimiento.

---

## Árbol de Relaciones Completo

### Vista Principal: Estructura Jerárquica

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                          TicTacToe-ncurses 
                              (project)
                                 │
        ┌────────────┬───────────┼───────────┬───────────┬──────────┐
        │            │           │           │           │          │
     NIVEL 1      NIVEL 1     NIVEL 1    NIVEL 1     NIVEL 1    NIVEL 1
   (5 components) (build)   (concepto)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### Nivel 1: Componentes y Conceptos Principales

```
TicTacToe-ncurses (project)
│
├── [1] Board-class (component)
│       └── Gestión: Estado del tablero 3x3
│
├── [2] UI-class (component) ⭐ HUB: 7 conexiones
│       └── Gestión: Interfaz ncurses
│
├── [3] Game-class (component) ⭐ HUB: 6 conexiones
│       └── Gestión: Controlador principal
│
├── [4] Menu-class (component)
│       └── Gestión: Navegación de menús
│
├── [5] Settings-class (component)
│       └── Gestión: Configuración del juego
│
├── [6] makefile-structure (build)
│       └── Gestión: Compilación
│
└── [7] Mouse_Click_Detection (concepto)
        └── Gestión: Detección de clics
```

---

### Nivel 2: Expansión de UI-class (Componente Central)

```
[2] UI-class (component)
    │
    ├── [2.1] ncurses-integration (pattern)
    │         └── Tipo: Patrón de integración
    │         └── Función: Inicialización, colores, eventos
    │
    ├── [2.2] Hit_Testing (concepto) ⭐ HUB: 6 conexiones
    │         └── Tipo: Algoritmo de colisión
    │         └── Función: Verificar punto en área
    │         │
    │         ├── [2.2.1] UI_Layout_Calculation (concepto)
    │         │           └── Función: Cálculo de posiciones dinámicas
    │         │
    │         └── [2.2.2] Mouse_Implementation_Pitfalls (anti_pattern)
    │                     └── Función: Errores comunes a evitar
    │
    ├── [2.3] Coordinate_to_Element_Mapping (concepto)
    │         └── Tipo: Algoritmo de mapeo
    │         └── Función: Convertir (x,y) en elemento
    │         │
    │         ├── [2.3.1] Hit_Testing (concepto) [↑ ver 2.2]
    │         │
    │         └── [2.3.2] Grid_Cell_Selection (algoritmo)
    │                     └── Función: Selección de celda en grilla
    │
    └── [2.4] UI_Layout_Calculation (concepto) [↑ ver 2.2.1]
```

---

### Nivel 2: Expansión de Game-class (Controlador)

```
[3] Game-class (component)
    │
    ├── [3.1] Board-class (component) [↑ ver 1]
    │         └── Relación: manages (gestiona)
    │
    ├── [3.2] UI-class (component) [↑ ver 2]
    │         └── Relación: uses (usa)
    │
    ├── [3.3] Settings-class (component) [↑ ver 5]
    │         └── Relación: uses (usa)
    │
    ├── [3.4] independent-boards-pattern (pattern)
    │         └── Tipo: Patrón de diseño
    │         └── Función: Tableros independientes
    │
    └── [3.5] Event_Driven_Input_Handling (patron)
              └── Tipo: Patrón de eventos
              └── Función: Manejo de entrada teclado/ratón
```

---

### Nivel 2: Expansión de Menu-class

```
[4] Menu-class (component)
    │
    ├── [4.1] UI-class (component) [↑ ver 2]
    │         └── Relación: uses (usa)
    │
    └── [4.2] Settings-class (component) [↑ ver 5]
              └── Relación: modifies (modifica)
```

---

### Nivel 2: Expansión de Mouse_Click_Detection

```
[7] Mouse_Click_Detection (concepto)
    │
    ├── [7.1] Event_Driven_Input_Handling (patron) [↑ ver 3.5]
    │         └── Relación: requiere
    │
    ├── [7.2] Hit_Testing (concepto) [↑ ver 2.2]
    │         └── Relación: utiliza
    │
    ├── [7.3] Mouse_State_Management (best_practice)
    │         └── Tipo: Mejores prácticas
    │         └── Función: Gestión de estado del ratón
    │
    ├── [7.4] Mouse_Implementation_Pitfalls (anti_pattern) [↑ ver 2.2.2]
    │         └── Relación: debe_evitar
    │
    └── [7.5] Mouse_Feature_Documentation (documentation)
              └── Tipo: Documentación
              └── Relación: documenta (inversa)
```

---

### Nivel 3: Expansión de Testing (Complementario)

```
Testing_Interactive_Features (patron)
    │
    ├── [T.1] Mouse_Click_Detection (concepto) [↑ ver 7]
    │         └── Relación: debe_verificar
    │
    ├── [T.2] Coordinate_to_Element_Mapping (concepto) [↑ ver 2.3]
    │         └── Relación: debe_validar
    │
    ├── [T.3] Unit_Test_Mouse_Logic (test_strategy)
    │         └── Tipo: Estrategia de testing
    │         └── Función: Tests unitarios de ratón
    │
    └── [T.4] Integration_Test_Mouse (test_strategy)
              └── Tipo: Estrategia de testing
              └── Función: Tests de integración de ratón
```

---

## Mapa Visual: Forma del Árbol Completo

```
                                  TicTacToe-ncurses
                                        │
         ┌──────────┬──────────┬────────┼────────┬──────────┬──────────┐
         │          │          │        │        │          │          │
      Board-      UI-class   Game-   Menu-   Settings-  makefile-  Mouse_Click_
       class    (7 conexiones) class   class    class    structure   Detection
         │          │          │        │        │          │          │
         │          │          │        │        │          │          │
    [Modelo]   [Vista MVC]  [Control]  │        │       [Build]    [Input]
                   │          │        │        │                      │
         ┌─────────┼──────────┘        │        │                      │
         │         │                   │        │                      │
         │    ┌────┴────┬──────────────┘        │              ┌───────┴──────┐
         │    │         │                       │              │              │
         │  ncurses  Hit_Testing ───────────────┘       Event_Driven    Mouse_State
         │    │         │                              Input_Handling   Management
         │    │    ┌────┴────┐
         │    │    │         │
         │    │  UI_Layout  Mouse_
         │    │  Calculation Pitfalls
         │    │
         │    └─ Coordinate_to_
         │       Element_Mapping
         │            │
         │       Grid_Cell_
         │       Selection
         │
         └─ independent-
            boards-pattern

                [Testing Layer]
                      │
          Testing_Interactive_Features
                      │
          ┌───────────┼───────────┐
          │           │           │
    Unit_Test_   Integration_   [verifica]
    Mouse_Logic  Test_Mouse    Mouse & Coords
```

---

## Guía de Navegación del Árbol

### Profundidad por Rama

| Rama Principal | Profundidad Máxima | Nodos en la Rama |
|----------------|---------------------|------------------|
| **Board-class** | 1 nivel | 1 nodo |
| **UI-class** | 4 niveles | 8 nodos |
| **Game-class** | 2 niveles | 5 nodos |
| **Menu-class** | 2 niveles | 2 nodos |
| **Settings-class** | 1 nivel | 1 nodo |
| **makefile-structure** | 1 nivel | 1 nodo |
| **Mouse_Click_Detection** | 3 niveles | 5 nodos |
| **Testing** (complemento) | 2 niveles | 4 nodos |

### Orden de Exploración Recomendado

**1. Para entender la arquitectura básica:**
```
TicTacToe-ncurses → Game-class → Board-class + UI-class + Settings-class
```

**2. Para entender la interfaz:**
```
UI-class → ncurses-integration → Hit_Testing → UI_Layout_Calculation
```

**3. Para entender la entrada de usuario:**
```
Event_Driven_Input_Handling → Mouse_Click_Detection → Hit_Testing
```

**4. Para entender el testing:**
```
Testing_Interactive_Features → Unit_Test_Mouse_Logic + Integration_Test_Mouse
```

### Índice de Referencias Cruzadas

```
[1] Board-class
    └─ Referenciado por: [3.1]

[2] UI-class
    ├─ Referenciado por: [3.2], [4.1]
    ├─ Contiene: [2.1], [2.2], [2.3], [2.4]
    └─ Hub: 7 conexiones totales

[2.2] Hit_Testing
    ├─ Referenciado por: [2.3.1], [7.2]
    ├─ Contiene: [2.2.1], [2.2.2]
    └─ Hub: 6 conexiones totales

[3] Game-class
    ├─ Contiene: [3.1], [3.2], [3.3], [3.4], [3.5]
    └─ Hub: 6 conexiones totales

[3.5] Event_Driven_Input_Handling
    └─ Referenciado por: [7.1]

[5] Settings-class
    └─ Referenciado por: [3.3], [4.2]

[7] Mouse_Click_Detection
    ├─ Contiene: [7.1], [7.2], [7.3], [7.4], [7.5]
    └─ Referenciado por: [T.1]

[2.3] Coordinate_to_Element_Mapping
    └─ Referenciado por: [T.2]
```

---

## Tabla de Entidades por Nivel

### Nivel 1: Directamente relacionadas con TicTacToe-ncurses

| Entidad | Tipo | Relación | Descripción |
|---------|------|----------|-------------|
| **Board-class** | component | contains | Representa un tablero 3x3 individual con lógica de juego |
| **UI-class** | component | contains | Gestiona toda la interfaz con ncurses |
| **Game-class** | component | contains | Controlador principal del flujo de juego |
| **Menu-class** | component | contains | Sistema de menús navegables |
| **Settings-class** | component | contains | Gestiona configuración del juego |
| **makefile-structure** | build | builds-with | Estructura de compilación del proyecto |
| **Mouse_Click_Detection** | concepto | implements | Detección de clics del ratón |

### Nivel 2: Relacionadas con componentes de Nivel 1

| Entidad | Tipo | Relacionada con | Relación |
|---------|------|-----------------|----------|
| **ncurses-integration** | pattern | UI-class | implements |
| **Hit_Testing** | concepto | UI-class | utiliza |
| **Coordinate_to_Element_Mapping** | concepto | UI-class | implementa |
| **UI_Layout_Calculation** | concepto | UI-class | requiere |
| **independent-boards-pattern** | pattern | Game-class | implements |
| **Event_Driven_Input_Handling** | patron | Game-class | implementa |
| **Mouse_State_Management** | best_practice | Mouse_Click_Detection | debe_implementar |
| **Mouse_Implementation_Pitfalls** | anti_pattern | Mouse_Click_Detection | debe_evitar |
| **Mouse_Feature_Documentation** | documentation | Mouse_Click_Detection | documenta |

### Nivel 3: Relacionadas con entidades de Nivel 2

| Entidad | Tipo | Relacionada con | Relación |
|---------|------|-----------------|----------|
| **Grid_Cell_Selection** | algoritmo | Coordinate_to_Element_Mapping | implementado_por |
| **Testing_Interactive_Features** | patron | Mouse_Click_Detection | debe_verificar |

### Nivel 4: Estrategias de Testing

| Entidad | Tipo | Relacionada con | Relación |
|---------|------|-----------------|----------|
| **Unit_Test_Mouse_Logic** | test_strategy | Testing_Interactive_Features | incluye |
| **Integration_Test_Mouse** | test_strategy | Testing_Interactive_Features | incluye |

---

## Resumen Estadístico

### Nodos del Grafo
- **Nodos de nivel 1**: 7 entidades
- **Nodos de nivel 2**: 9 entidades
- **Nodos de nivel 3**: 2 entidades
- **Nodos de nivel 4**: 2 entidades
- **Total de entidades relacionadas**: 20

### Tipos de Relaciones
| Tipo de Relación | Frecuencia | Ejemplo |
|------------------|------------|---------|
| contains | 5 | TicTacToe-ncurses → Board-class |
| implements / implementa | 5 | UI-class → ncurses-integration |
| uses / utiliza | 4 | Game-class → UI-class |
| requiere | 2 | Mouse_Click_Detection → Event_Driven_Input_Handling |
| manages | 1 | Game-class → Board-class |
| modifies | 1 | Menu-class → Settings-class |
| debe_evitar | 2 | Hit_Testing → Mouse_Implementation_Pitfalls |
| debe_verificar | 1 | Testing_Interactive_Features → Mouse_Click_Detection |
| debe_validar | 1 | Testing_Interactive_Features → Coordinate_to_Element_Mapping |
| documenta | 1 | Mouse_Feature_Documentation → Mouse_Click_Detection |
| usa_datos_de | 1 | Hit_Testing → UI_Layout_Calculation |
| es_parte_de | 1 | Coordinate_to_Element_Mapping → Hit_Testing |
| implementado_por | 1 | Coordinate_to_Element_Mapping → Grid_Cell_Selection |
| builds-with | 1 | TicTacToe-ncurses → makefile-structure |
| incluye | 2 | Testing_Interactive_Features → Unit_Test_Mouse_Logic |
| debe_implementar | 1 | Mouse_Click_Detection → Mouse_State_Management |

---

## Análisis por Categoría

### 1. Componentes de Arquitectura (5)
- **Board-class**: Modelo de datos del tablero 3x3
- **UI-class**: Vista con ncurses
- **Game-class**: Controlador principal
- **Menu-class**: Navegación de menús
- **Settings-class**: Configuración

### 2. Patrones de Diseño (3)
- **ncurses-integration**: Integración con biblioteca ncurses
- **independent-boards-pattern**: Tableros independientes
- **Event_Driven_Input_Handling**: Manejo de eventos

### 3. Conceptos Técnicos (4)
- **Mouse_Click_Detection**: Detección de clics
- **Hit_Testing**: Verificación de colisión punto-área
- **Coordinate_to_Element_Mapping**: Mapeo coordenadas a elementos
- **UI_Layout_Calculation**: Cálculo de layout dinámico

### 4. Algoritmos (1)
- **Grid_Cell_Selection**: Selección de celda en grilla

### 5. Testing (3)
- **Testing_Interactive_Features**: Patrón de testing interactivo
- **Unit_Test_Mouse_Logic**: Tests unitarios de ratón
- **Integration_Test_Mouse**: Tests de integración de ratón

### 6. Mejores Prácticas (1)
- **Mouse_State_Management**: Gestión de estado del ratón

### 7. Anti-Patrones (1)
- **Mouse_Implementation_Pitfalls**: Errores comunes a evitar

### 8. Build (1)
- **makefile-structure**: Sistema de compilación

### 9. Documentación (1)
- **Mouse_Feature_Documentation**: Documentación de funcionalidad de ratón

---

## Entidades Más Conectadas (Hubs)

### UI-class (7 conexiones)
Nodo central del sistema de visualización:
1. Contenida en: TicTacToe-ncurses
2. Usada por: Game-class
3. Usada por: Menu-class
4. Implementa: ncurses-integration
5. Utiliza: Hit_Testing
6. Implementa: Coordinate_to_Element_Mapping
7. Requiere: UI_Layout_Calculation

### Hit_Testing (6 conexiones)
Concepto central para detección de interacción:
1. Utilizado por: UI-class
2. Utilizado por: Mouse_Click_Detection
3. Usa datos de: UI_Layout_Calculation
4. Debe evitar: Mouse_Implementation_Pitfalls
5. Contiene: Coordinate_to_Element_Mapping (es_parte_de)
6. Validado por: Testing_Interactive_Features

### Game-class (6 conexiones)
Controlador principal del proyecto:
1. Contenido en: TicTacToe-ncurses
2. Gestiona: Board-class
3. Usa: UI-class
4. Usa: Settings-class
5. Implementa: independent-boards-pattern
6. Implementa: Event_Driven_Input_Handling

---

## Caminos de Dependencia Más Largos

### Camino 1 (5 niveles)
```
TicTacToe-ncurses 
  → [contains] UI-class 
    → [implementa] Coordinate_to_Element_Mapping 
      → [implementado_por] Grid_Cell_Selection
```

### Camino 2 (5 niveles)
```
TicTacToe-ncurses 
  → [contains] UI-class 
    → [utiliza] Hit_Testing 
      → [usa_datos_de] UI_Layout_Calculation
```

### Camino 3 (5 niveles)
```
TicTacToe-ncurses 
  → [implements] Mouse_Click_Detection 
    → [debe_verificar] ← Testing_Interactive_Features 
      → [incluye] Unit_Test_Mouse_Logic
```

---

## Observaciones del Análisis

### Arquitectura MVC
El proyecto sigue un patrón **Modelo-Vista-Controlador** implícito:
- **Modelo**: Board-class, Settings-class
- **Vista**: UI-class (con ncurses-integration)
- **Controlador**: Game-class, Menu-class

### Estrategia de Testing Completa
El grafo incluye una estrategia de testing bien definida:
- **Patrón general**: Testing_Interactive_Features
- **Tests unitarios**: Unit_Test_Mouse_Logic (lógica pura)
- **Tests de integración**: Integration_Test_Mouse (flujos completos)

### Gestión de Entrada Dual
El proyecto implementa dos modos de entrada:
1. **Teclado**: Event_Driven_Input_Handling
2. **Ratón**: Mouse_Click_Detection + Hit_Testing

### Documentación de Calidad
El grafo captura no solo código, sino también:
- ✅ **Best practices**: Mouse_State_Management
- ❌ **Anti-patterns**: Mouse_Implementation_Pitfalls
- 📖 **Documentation**: Mouse_Feature_Documentation

### Independencia de Tableros
El patrón **independent-boards-pattern** garantiza:
- Cada tablero con su propio estado
- Sin compartir turnos ni estadísticas
- Navegación multiplexada entre tableros

---

## Detalles de Entidades Clave

### TicTacToe-ncurses (Raíz)
**Observaciones del grafo:**
- Juego completo de TicTacToe en C++ con interfaz ncurses
- Ubicación: code/tictactoe/
- Soporta múltiples tableros simultáneos (1-9) completamente independientes
- Cada tablero mantiene su propio turno, estado, victorias y empates
- Alternancia estricta X → O → X → O en cada tablero
- Tres modos: 0 jugadores (auto), 1 jugador (manual), 2 jugadores (vs auto)
- Compilación: `make` (requiere g++, ncurses, make)
- Ejecución: `./bin/tictactoe` o `make run`
- Implementa detección de clicks de ratón
- Utiliza hit testing para mapear coordenadas
- Soporta controles de teclado y ratón

### Board-class
**Observaciones del grafo:**
- Clase que representa un tablero individual 3x3
- Archivos: include/Board.h y src/Board.cpp
- Gestiona estado del juego, turno actual, detección de victoria/empate
- Métodos clave: makeMove(), makeAutoMove(), reset(), checkWin(), checkDraw()
- Cada tablero comienza siempre con X como primer turno
- Mantiene estadísticas individuales: xWins, oWins, draws
- Movimientos aleatorios implementados para modo automático

### UI-class
**Observaciones del grafo:**
- Gestiona toda la interfaz con ncurses
- Archivos: include/UI.h y src/UI.cpp
- Soporta colores, ratón y teclado
- Métodos de dibujo: drawMenu(), drawBoards(), drawSingleBoard(), drawStats(), drawHelp(), drawSettingsMenu()
- Se adapta dinámicamente al tamaño de terminal
- Usa 6 pares de colores para distintos elementos
- Cursor oculto durante ejecución (curs_set(0))
- Implementa método mapClickToCell para convertir clicks en selecciones de celda
- Calcula layout dinámico de tableros para soporte de hit testing preciso
- Gestiona eventos de ratón mediante ncurses (mousemask, KEY_MOUSE)

### Mouse_Click_Detection
**Observaciones del grafo:**
- Los clics del ratón se detectan mediante eventos de entrada en el bucle principal
- Se necesita habilitar el soporte de ratón en la biblioteca de interfaz al inicializar
- Los eventos contienen coordenadas (x, y) en unidades de la pantalla
- Debe distinguirse entre tipos de eventos: clic, doble clic, botón presionado, botón liberado
- La detección requiere comparar coordenadas del evento con áreas clickeables definidas
- En ncurses: habilitar con mousemask(ALL_MOUSE_EVENTS, NULL) y detectar con KEY_MOUSE
- Implementado en el proyecto TicTacToe-ncurses para interacción de usuario
- Requiere cálculo de layout para mapeo preciso de coordenadas
- Usado por UI-class para detectar clicks en celdas del tablero de juego

### Hit_Testing
**Observaciones del grafo:**
- Hit testing es verificar si un punto (x,y) está dentro de un área rectangular
- Condición: x >= left && x < right && y >= top && y < bottom
- Iterar sobre todos los elementos clickeables para encontrar cuál contiene el punto
- Orden de verificación importante: elementos superiores (z-index) primero
- Usar estructuras de datos que almacenen bounds de cada elemento clickeable

### Grid_Cell_Selection (Algoritmo)
**Observaciones del grafo:**
- Para seleccionar celda en grid NxM desde coordenadas (x,y) del clic
- Paso 1: Verificar que (x,y) está dentro del área del grid completo
- Paso 2: Calcular coordenadas relativas: relX = x - gridStartX, relY = y - gridStartY
- Paso 3: Dividir por tamaño de celda: col = relX / cellWidth, row = relY / cellHeight
- Paso 4: Validar que row < numRows && col < numCols antes de usar
- Implementación de ejemplo en C++: int row = (y - gridY) / cellHeight;
- Importante usar división entera, no flotante, para obtener índice de celda
- Considerar casos especiales: grids con diferentes tamaños de celda

---

## Conclusiones

### Completitud del Grafo
El grafo de conocimiento captura:
1. **Arquitectura**: Componentes y sus relaciones
2. **Patrones**: Soluciones de diseño implementadas
3. **Conceptos**: Ideas técnicas fundamentales
4. **Algoritmos**: Implementaciones específicas
5. **Testing**: Estrategias de verificación
6. **Calidad**: Best practices y anti-patterns
7. **Build**: Sistema de compilación

### Estructura Modular
La exploración recursiva revela un diseño altamente modular:
- **Separación de responsabilidades**: Cada componente tiene un propósito claro
- **Bajo acoplamiento**: Los componentes se comunican a través de interfaces bien definidas
- **Alta cohesión**: Cada entidad agrupa funcionalidad relacionada

### Escalabilidad del Conocimiento
El grafo permite:
- **Navegación**: Seguir relaciones entre conceptos
- **Descubrimiento**: Identificar patrones y anti-patrones
- **Validación**: Verificar implementación de best practices
- **Documentación**: Generar reportes automáticos como este

### Aplicaciones Futuras
Este análisis puede usarse para:
1. **Onboarding**: Nuevos desarrolladores pueden entender la arquitectura
2. **Refactoring**: Identificar puntos de mejora en el diseño
3. **Testing**: Guiar la creación de tests basándose en relaciones
4. **Documentación**: Mantener actualizada la arquitectura del proyecto

---

**Fecha de generación**: 2026-02-15  
**Método**: Exploración recursiva con memory-read_graph  
**Entidad raíz**: TicTacToe-ncurses  
**Profundidad máxima**: 5 niveles  
**Total de entidades exploradas**: 20
