# Informe Comparativo 11: Traducción COBOL a Spring Boot (Baseline vs Memory-Guided)

Este informe detalla los resultados obtenidos tras someter al modelo a una doble traducción de programas COBOL clásicos (como `banco-ingresos` y `banco-transferencia`) a **Spring Boot**.
La primera versión es un *Baseline* (sin asistencia contextual adicional), y la segunda es *Memory-Guided* (respaldada por el ecosistema de memoria semántica de `Remembrances` introducido recientemente).

---

## 1. Implementación Baseline (Sin Memoria Especializada)

### 1.1 Características del Código Generado
El modelo, utilizando su conocimiento general de Java y Spring, produjo una traducción que **funciona lógicamente**, pero adolece de múltiples antipatrones cuando se mira desde el prisma de una aplicación de nivel empresarial moderna.

* **Estado mutable incrustado en los Servicios:** El traductor copió el concepto procedural estricto de la `WORKING-STORAGE SECTION`. Variables como `sumaTotal`, `contador` o `transferValida` se declararon genéricamente como atributos de estado de la clase `@Service`. Esto es un antipatrón masivo en Spring, ya que **los beans `@Service` son `Singletons` por defecto**; si dos usuarios hicieran transferencias simultáneas, se pisarían las variables y habría colisiones graves de datos (*Race conditions*).
* **Ausencia de un diseño real de Base de Datos:** Los ficheros o pseudo-bases de datos simuladas en COBOL se tradujeron como campos quemados en el código (`saldoOrigen = new BigDecimal("25000.00")`), simulando el estado en memoria.
* **Flujos de Control emulados de COBOL:** Validaciones críticas se devolvían usando flag booleanos (`boolean transferValida = false`) y cadenas de condicionales idénticas a la topología `IF-ELSE` de la `PROCEDURE DIVISION`, en vez de lanzar excepciones.
* **Sin tipado avanzado de DTOs:** Los endpoints recibían todos los campos granulares vía `@RequestParam` (ej. `@RequestParam String origen, @RequestParam String destino...`), en lugar de serializar los modelos de negocio correctamente.

---

## 2. Implementación Memory-Guided (Con Ecosistema Remembrances)

Al inyectar y recuperar los diferentes fragmentos de la memoria semántica (`data_structures_mapping`, `error_handling_mapping`, `file_io_to_jpa_mapping`), la salida del modelo experimentó un salto cualitativo pasando de una *"Traducción Lexicográfica"* a un *"Refactoring Arquitectónico"*.

### 2.1 Mejoras Arquitectónicas Clave (Efecto de la Memoria)
1. **Verdadera separación MVC y Persistencia (JPA):**
   * Gracias al vector `file_io_to_jpa_mapping`, el modelo entendió que no debía simular las cuentas en memoria. Dedujo y escribió autónomamente una Entity (`CuentaEntity`) y su contrato `Spring Data JPA` (`CuentaRepository`).
   * La inyección de los repositorios desplazó la manipulación de saldos hacia abstracciones `findById` y `save`, haciendo el código verdaderamente resiliente.
2. **Transformación de Estructuras Complejas (OCCURS):**
   * El vector de `data_structures_mapping` actuó forzando una interpretación orientada a objetos. El bloque COBOL estático `05 WS-INGRESO-ENTRY OCCURS 100 TIMES` no se mapeó a una clase genérica anidada, sino a un robusto `DTO` (`IngresosBatchRequestDTO`) con una sintaxis correcta de `List<IngresoItem>`.
3. **Manejo Excepcional Moderno de Errores (AOP):**
   * Evitando la traducción literal de variables de estado (como el *FILE STATUS* / Flags booleanos), construimos un mecanismo global de gestión vía `@RestControllerAdvice` (`GlobalExceptionHandler`).
   * Cualquier error de negocio (*Cuentas idénticas*, *Importe = 0*, o un balance insuficiente) dejó de ser un `IF-ELSE` anidado y pasó a representarse mediante interrupciones seguras del hilo (`throw new IllegalArgumentException()`, `throw new IllegalStateException()`).
4. **Protección Transaccional de Spring:**
   * La anotación `@Transactional` en los servicios garantizó de golpe que, si una rutina fallaba en la mitad, la base de datos haría Rollback. Esto cerró la brecha de control de concurrencia que COBOL dejaba muy ambigua.

---

## 3. Conclusión Final: El Impacto de "Remembrances"

El experimento *dual* confirma firmemente la hipótesis de que **traducir COBOL a modern frameworks no debe tratarse nunca como un procesado NLP de Lenguaje A a Lenguaje B**. Sin memoria, un LLM actúa como un compilador cruzado: escupe Java, pero que sigue oliendo a COBOL procedural (Singletons manteniendo estado mutable).

Al nutrir la Memoria Semántica (`Remembrances`), pasamos a un entorno en donde la IA no traduce, **rediseña**. Utilizar fragmentación vectorial hiper-especializada de `Gemini 3.1 Pro` permite que el RAG inyecte in-situ las reglas maestras (usar DTOs para OCCURS, usar EntityNotFoundException para fallos de STATUS) transformando el código heredado en código *Cloud Native* puro.
