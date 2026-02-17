# Proceso de Unión de Árboles en Memory MCP

**Fecha**: 11 de febrero de 2026  
**Objetivo**: Unir los subgrafos "TicTacToe-ncurses" y "Mouse_Click_Detection" mediante relaciones lógicas

---

## 1. Situación Inicial

Según el análisis en `BUSQUEDA_MEMORY.md`, el grafo de conocimiento está fragmentado en **3 subgrafos completamente aislados**:

### Subgrafo 1: TicTacToe-ncurses (9 entidades)
```
TicTacToe-ncurses [project]
├── Board-class [component]
├── UI-class [component]
├── Game-class [component]
├── Menu-class [component]
├── Settings-class [component]
├── makefile-structure [build]
├── ncurses-integration [pattern]
└── independent-boards-pattern [pattern]
```

### Subgrafo 2: Mouse_Click_Detection (12 entidades)
```
Mouse_Click_Detection [concepto]
├── Event_Driven_Input_Handling [patron]
├── Hit_Testing [concepto]
├── UI_Layout_Calculation [concepto]
├── Mouse_State_Management [best_practice]
├── Mouse_Implementation_Pitfalls [anti_pattern]
├── Testing_Interactive_Features [patron]
├── Coordinate_to_Element_Mapping [concepto]
├── Grid_Cell_Selection [algoritmo]
├── Unit_Test_Mouse_Logic [test_strategy]
├── Integration_Test_Mouse [test_strategy]
└── Mouse_Feature_Documentation [documentation]
```

### Subgrafo 3: Ticketmaster (3 entidades)
```
Ticketmaster Contar Eventos Procedimiento [Procedimiento]
└── Murcia Eventos 01-03-2026 a 31-05-2026 [Búsqueda Realizada]
Problemas gpt-4.1 [Problemas] (aislado)
```

---

## 2. Análisis de Conexiones Lógicas

### 2.1 Conexión Principal Propuesta

**Relación más lógica**: `TicTacToe-ncurses` **implementa** `Mouse_Click_Detection`

**Justificación**:
- El proyecto TicTacToe-ncurses utiliza funcionalidad de detección de clicks de ratón
- Según los READMEs, ambos proyectos mencionan "controles: teclado y ratón"
- La clase `UI-class` gestiona la entrada del usuario, incluyendo clicks de ratón
- El concepto `Mouse_Click_Detection` describe exactamente lo que TicTacToe implementa

### 2.2 Conexiones Secundarias

#### **UI-class** utiliza **Hit_Testing**
- `UI-class` debe implementar hit testing para mapear clicks a celdas del tablero
- Conexión directa entre el componente de interfaz y el concepto de prueba de colisión

#### **UI-class** implementa **Coordinate_to_Element_Mapping**
- El proyecto tiene método `mapClickToCell(mx, my, &b, &r, &c)` en raptor_con_memory
- Este método convierte coordenadas de mouse a elementos de la UI (tablero, fila, columna)

#### **UI-class** requiere **UI_Layout_Calculation**
- Ambos proyectos calculan layouts dinámicos
- raptor_con_memory tiene método `computeLayout()` explícito
- Este cálculo es esencial para el hit testing

#### **Game-class** implementa **Event_Driven_Input_Handling**
- El juego responde a eventos de teclado y ratón de forma asíncrona
- Usa el bucle de eventos de ncurses

---

## 3. Proceso de Unión en Memory MCP

### 3.1 Paso 1: Crear la Relación Principal

```json
{
  "from": "TicTacToe-ncurses",
  "to": "Mouse_Click_Detection",
  "relationType": "implements"
}
```

**Comando Memory MCP**:
```
memory-create_relations
[
  {
    "from": "TicTacToe-ncurses",
    "to": "Mouse_Click_Detection",
    "relationType": "implements"
  }
]
```

**Efecto**: Conecta el subgrafo del proyecto con el subgrafo conceptual de detección de ratón.

---

### 3.2 Paso 2: Conectar UI-class con Conceptos de Mouse

```json
[
  {
    "from": "UI-class",
    "to": "Hit_Testing",
    "relationType": "utiliza"
  },
  {
    "from": "UI-class",
    "to": "Coordinate_to_Element_Mapping",
    "relationType": "implementa"
  },
  {
    "from": "UI-class",
    "to": "UI_Layout_Calculation",
    "relationType": "requiere"
  }
]
```

**Comando Memory MCP**:
```
memory-create_relations
[
  {"from": "UI-class", "to": "Hit_Testing", "relationType": "utiliza"},
  {"from": "UI-class", "to": "Coordinate_to_Element_Mapping", "relationType": "implementa"},
  {"from": "UI-class", "to": "UI_Layout_Calculation", "relationType": "requiere"}
]
```

**Efecto**: Especifica cómo la clase de interfaz usa los conceptos de mouse.

---

### 3.3 Paso 3: Conectar Game-class con Patrón de Eventos

```json
{
  "from": "Game-class",
  "to": "Event_Driven_Input_Handling",
  "relationType": "implementa"
}
```

**Comando Memory MCP**:
```
memory-create_relations
[
  {
    "from": "Game-class",
    "to": "Event_Driven_Input_Handling",
    "relationType": "implementa"
  }
]
```

**Efecto**: Documenta que el juego usa arquitectura orientada a eventos.

---

### 3.4 Paso 4: Añadir Observaciones de Contexto

Añadir observaciones a las entidades para documentar la unión:

**A TicTacToe-ncurses**:
```
memory-add_observations
[
  {
    "entityName": "TicTacToe-ncurses",
    "contents": [
      "Implementa detección de clicks de ratón para seleccionar celdas del tablero",
      "Utiliza hit testing para mapear coordenadas de mouse a elementos de la UI",
      "Soporta controles de teclado y ratón según documentación del proyecto"
    ]
  }
]
```

**A UI-class**:
```
memory-add_observations
[
  {
    "entityName": "UI-class",
    "contents": [
      "Implementa método mapClickToCell para convertir clicks en selecciones de celda",
      "Calcula layout dinámico de tableros para soporte de hit testing preciso",
      "Gestiona eventos de ratón mediante ncurses (mousemask, KEY_MOUSE)"
    ]
  }
]
```

**A Mouse_Click_Detection**:
```
memory-add_observations
[
  {
    "entityName": "Mouse_Click_Detection",
    "contents": [
      "Implementado en el proyecto TicTacToe-ncurses para interacción de usuario",
      "Requiere cálculo de layout para mapeo preciso de coordenadas",
      "Usado por UI-class para detectar clicks en celdas del tablero de juego"
    ]
  }
]
```

---

## 4. Resultado de la Unión

### 4.1 Grafo Unificado (21 entidades conectadas)

```
TicTacToe-ncurses [project]
├── [implementa] Mouse_Click_Detection [concepto] ← NUEVA RELACIÓN PRINCIPAL
│   ├── [requiere] Event_Driven_Input_Handling [patron]
│   ├── [utiliza] Hit_Testing [concepto]
│   ├── [debe_implementar] Mouse_State_Management [best_practice]
│   └── ...resto del subgrafo Mouse
│
├── [contains] UI-class [component]
│   ├── [utiliza] Hit_Testing [concepto] ← NUEVA RELACIÓN
│   ├── [implementa] Coordinate_to_Element_Mapping [concepto] ← NUEVA RELACIÓN
│   ├── [requiere] UI_Layout_Calculation [concepto] ← NUEVA RELACIÓN
│   ├── [implements] ncurses-integration [pattern]
│   └── [uses] Game-class
│
├── [contains] Game-class [component]
│   ├── [implementa] Event_Driven_Input_Handling [patron] ← NUEVA RELACIÓN
│   ├── [manages] Board-class
│   └── ...resto de relaciones
│
└── ...resto del subgrafo TicTacToe
```

### 4.2 Estadísticas del Grafo Unificado

**Antes de la unión**:
- 3 subgrafos aislados
- 24 entidades totales
- 0 conexiones entre subgrafos

**Después de la unión**:
- 2 subgrafos (TicTacToe+Mouse unificado, Ticketmaster aislado)
- 24 entidades totales
- 5 nuevas relaciones creadas
- 21 entidades conectadas en el grafo principal

---

## 5. Ventajas de la Unión

### 5.1 Para Búsquedas y Consultas

**Antes**:
```
Query: "¿Qué proyectos implementan detección de mouse?"
Resultado: Ninguno (subgrafos aislados)
```

**Después**:
```
Query: "¿Qué proyectos implementan detección de mouse?"
Resultado: TicTacToe-ncurses implementa Mouse_Click_Detection
```

### 5.2 Para Navegación del Grafo

- Ahora se puede navegar desde conceptos abstractos hasta implementaciones concretas
- Permite descubrir patrones de implementación en proyectos reales
- Facilita auditorías de calidad (¿se implementan las best practices?)

### 5.3 Para Análisis de Impacto

Si se modifica `Mouse_Click_Detection` o sus conceptos relacionados:
- Memory puede identificar que `TicTacToe-ncurses` se verá afectado
- Puede sugerir revisar `UI-class` específicamente
- Ayuda a propagar cambios de diseño a implementaciones

---

## 6. Comando Completo para Ejecutar

### Script de Unión (pseudo-código)

```javascript
// 1. Relación principal
memory.createRelations([
  { from: "TicTacToe-ncurses", to: "Mouse_Click_Detection", relationType: "implements" }
]);

// 2. Relaciones de UI-class
memory.createRelations([
  { from: "UI-class", to: "Hit_Testing", relationType: "utiliza" },
  { from: "UI-class", to: "Coordinate_to_Element_Mapping", relationType: "implementa" },
  { from: "UI-class", to: "UI_Layout_Calculation", relationType: "requiere" }
]);

// 3. Relación de Game-class
memory.createRelations([
  { from: "Game-class", to: "Event_Driven_Input_Handling", relationType: "implementa" }
]);

// 4. Añadir contexto
memory.addObservations([
  {
    entityName: "TicTacToe-ncurses",
    contents: [
      "Implementa detección de clicks de ratón para seleccionar celdas",
      "Soporta controles de teclado y ratón según documentación"
    ]
  },
  {
    entityName: "UI-class",
    contents: [
      "Implementa método mapClickToCell para conversión de clicks a celdas",
      "Gestiona eventos de ratón mediante ncurses"
    ]
  }
]);
```

---

## 7. Verificación de la Unión

### 7.1 Queries de Validación

```
1. search_nodes("Mouse_Click_Detection")
   → Debería mostrar relación con TicTacToe-ncurses

2. search_nodes("TicTacToe-ncurses")
   → Debería mostrar relación con Mouse_Click_Detection

3. search_nodes("UI-class")
   → Debería mostrar relaciones con Hit_Testing, Coordinate_to_Element_Mapping, UI_Layout_Calculation

4. open_nodes(["TicTacToe-ncurses"])
   → Verificar observaciones añadidas sobre mouse
```

### 7.2 Test de Navegación

Desde `Mouse_Click_Detection`, seguir relaciones entrantes:
```
Mouse_Click_Detection
  ← [implements] TicTacToe-ncurses
     ← [contains] UI-class
        ← [utiliza] Hit_Testing (vuelta al subgrafo Mouse)
```

Debería ser posible navegar en ambas direcciones sin encontrar nodos huérfanos.

---

## 8. Consideraciones Adicionales

### 8.1 ¿Conectar también el Subgrafo de Ticketmaster?

**Posible relación**:
- `Ticketmaster Contar Eventos Procedimiento` también implementa `Mouse_Click_Detection`
- Según BUSQUEDA_MEMORY.md: "counts events by city/date range via search, filter dates, apply, wait for load, count, handle cookies"
- El procedimiento incluye clicks en elementos de la UI

**Relación propuesta**:
```json
{
  "from": "Ticketmaster Contar Eventos Procedimiento",
  "to": "Mouse_Click_Detection",
  "relationType": "utiliza"
}
```

### 8.2 Mantener Integridad del Grafo

Al hacer cambios en memoria:
- ✅ Usar relaciones en voz activa
- ✅ Documentar el contexto con observaciones
- ✅ Verificar que las relaciones son bidireccionales cuando sea apropiado
- ✅ Evitar relaciones redundantes o duplicadas

---

## 9. Conclusión

El proceso de unión transforma un grafo fragmentado en un **grafo de conocimiento coherente** donde:

1. Los conceptos abstractos (`Mouse_Click_Detection`) se conectan con implementaciones concretas (`TicTacToe-ncurses`)
2. Los componentes de código (`UI-class`) referencian los patrones que utilizan
3. El sistema de memoria puede responder consultas complejas atravesando subgrafos
4. Se mantiene trazabilidad entre diseño y código

**Próximos pasos**:
- Ejecutar los comandos de Memory MCP descritos
- Verificar la unión con queries de búsqueda
- Documentar el grafo unificado actualizado
- Considerar unir también el subgrafo de Ticketmaster si es relevante

---

**Fin del Documento**
