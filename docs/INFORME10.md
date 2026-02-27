# Informe Comparativo: Evolución de la Memoria Semántica (Opus 4.6 vs. Gemini 3.1 Pro)

Este informe analiza el estado de la memoria semántica (`Remembrances`) para el caso de uso de traducción de programas **COBOL a Spring Boot**, analizando la huella del modelo anterior (Opus 4.6) en comparación con el trabajo reciente de la iteración actual (Gemini 3.1 Pro).

---

## 1. Enfoque Opus 4.6 (Memoria Anterior)

La memoria inyectada originalmente por el modelo Opus 4.6 consistía en **1 único vector monolítico** (Categoría: `architecture_translation_guide`) con el siguiente contenido aproximado:

```text
Guía de Traducción COBOL a Spring Boot: 
1) La IDENTIFICATION DIVISION y ENVIRONMENT DIVISION rara vez tienen equivalente directo...
2) La DATA DIVISION se convierte en DTOs para entrada/salida y Entidades JPA.
3) La PROCEDURE DIVISION es donde radica la lógica; el main loop se convierte en llamadas al Service desencadenadas por un RestController...
```

### Características del enfoque de Opus 4.6:
- **Estructura:** Monolítica y de alto nivel.
- **Ventaja:** Da una visión arquitectónica general y rápida al modelo en un solo prompt.
- **Desventaja:** Carece de granularidad. Ante dudas específicas de sintaxis (p.ej. cómo traducir un `COMP-3` o un `REDEFINES`), el modelo debe deducirlo "al vuelo" corriendo el riesgo de alucinaciones o pérdida de contexto semántico durante la búsqueda vectorial, dado que un único vector no cubre suficientes _keywords_.

---

## 2. Enfoque actual con Gemini 3.1 Pro (Memoria Actual)

El sistema de inyección actual desarrollado por Gemini ha consistido en fragmentar y especializar el conocimiento mediante **7 nuevos vectores independientes** con etiquetas (`metadatos/categorías`) específicas:

1. **`data_types_mapping`**: Especifica a nivel de variable (`PIC X` -> `String`, `COMP-3` -> numéricos/`BigDecimal`).
2. **`data_structures_mapping`**: Aborda el polimorfismo (`REDEFINES`), bucles rígidos en datos (`OCCURS`) y literales de control (Nivel `88`).
3. **`control_flow_mapping`**: Entrena al RAG en el mapeo equivalente algorítmico (`PERFORM` -> Métodos privados, `GO TO` -> refactoring).
4. **`file_io_to_jpa_mapping`**: Instruye que comandos como `READ`, `WRITE` o `REWRITE` se ligan estrictamente a métodos de `Repository` (`findById`, `save`), gestionados bajo `@Transactional`.
5. **`routines_copybooks_mapping`**: Describe la inyección de dependencias (`COPY` de lógica a inyección vía `@Service`, `CALL` procedural a llamadas orientadas a objetos con objetos mutables para BY REFERENCE).
6. **`spring_boot_architecture_mapping`**: Expande la asimilación arquitectónica dividiendo estrictamente responsabilidades en MVC y Data Access (a diferencia del modelo COBOL de top-to-bottom).
7. **`error_handling_mapping`**: Gestiona las estrategias de traducción del `FILE STATUS` a `@RestControllerAdvice` y manejo de Excepciones heredadas.

### Características del enfoque de Gemini 3.1 Pro:
- **Estructura:** Modular, indexada y granular.
- **Ventaja:** Altísima precisión en la recuperación RAG (_Retrieval-Augmented Generation_). Si el traductor encuentra una variable `OCCURS`, las distancias del vector traerán al instante la regla matemática correcta (`List<T>`). No recarga el _prompt_ con todo cuando solo se necesitan tipos de datos, mejorando coste y foco cognitivo.

---

## 3. Conclusiones y Expectativas de Impacto

1. **Escalabilidad de Conocimiento**: El enfoque Gemini 3.1 Pro permite que futuros programadores (o IAs) puedan actualizar reglas sueltas (ej. borrar/editar el vector de `error_handling` sin destruir la arquitectura). La memoria Opus era un texto concatenado difícil de versionar a futuro.
2. **Menos "Alucinaciones" Tácticas**: Las traducciones previas de Opus 4.6, si bien acertarían seguramente en la estructura de Spring Boot (Controllers/Services), solían ser erráticas en las entrañas matemáticas de variables y contadores procedurales en COBOL. La nueva memoria está afinada específicamente para evitar la fricción _Legacy -> OOP_.
3. **Calidad de Traducción Esperada**: La próxima traducción (de _baseline_ a _memory-guided_) usando la memoria de Gemini debería producir clases `DTO` más tipadas y servicios `JPA` mucho más robustos y menos anacrónicos.
