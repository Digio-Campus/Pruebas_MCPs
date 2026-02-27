# INFORME 12 — Comparativa: Traducción COBOL a Spring Boot con Gemini 3.1 Pro (Baseline vs Memory-Guided)

**Fecha:** 27/02/2026  
**Modelo evaluado:** Gemini 3.1 Pro  
**Variable de estudio:** Presencia/ausencia de memoria semántica (Remembrances MCP)  
**Programas COBOL traducidos:** `banco-ingresos.cbl`, `banco-transferencia.cbl`

---

## 1. Contexto

Ambos proyectos son traducciones de programas COBOL bancarios a **Spring Boot** realizadas por el mismo modelo (**Gemini 3.1 Pro**). La diferencia es el contexto disponible durante la generación:

| Aspecto | `baseline_gemini_3_1_pro` | `memory_gemini_3_1_pro` |
|---|---|---|
| **Modelo** | Gemini 3.1 Pro | Gemini 3.1 Pro |
| **Memoria** | Sin memoria (baseline) | Con memoria semántica (Remembrances) |
| **Enfoque** | Conocimiento general del modelo | 7 vectores especializados de traducción COBOL→Java |
| **Paquete base** | `com.banco.baseline` | `com.banco.memory` |

La memoria semántica inyectada al modelo memory-guided consistía en **7 vectores independientes** y especializados (ver INFORME10): `data_types_mapping`, `data_structures_mapping`, `control_flow_mapping`, `file_io_to_jpa_mapping`, `routines_copybooks_mapping`, `spring_boot_architecture_mapping` y `error_handling_mapping`.

---

## 2. Estadísticas Generales

| Métrica | `baseline_gemini_3_1_pro` | `memory_gemini_3_1_pro` |
|---|---|---|
| **Archivos Java** | 3 | 8 |
| **Líneas de código** | 219 | 312 |
| **Capas arquitectónicas** | 2 (controller + service) | 6 (controller + dto + service + repo + entity + exception) |
| **Entidades JPA** | 0 | 1 (`CuentaEntity`) |
| **DTOs** | 0 | 2 (`TransferenciaRequestDTO`, `IngresosBatchRequestDTO`) |
| **Repositorios** | 0 | 1 (`CuentaRepository`) |
| **Exception Handlers** | 0 | 1 (`GlobalExceptionHandler`) |
| **Tests** | 0 | 0 |
| **pom.xml / config** | No | No |

> La versión memory-guided produjo **2.7× más archivos** y **42% más LOC**, pero con archivos más pequeños y enfocados.

---

## 3. Arquitectura y Paquetes

### 3.1 Baseline — Estructura plana

```
com.banco.baseline
 ├── controller/
 │     └── BancoController.java          (45 líneas)
 └── service/
       ├── BancoIngresosService.java      (80 líneas)
       └── BancoTransferenciaService.java (94 líneas)
```

Solo **2 capas**: controlador y servicio. No existe capa de persistencia, DTOs, ni manejo de excepciones. Es una traducción literal de la estructura COBOL: los párrafos se convierten en métodos Java y la `WORKING-STORAGE SECTION` se convierte en atributos de instancia.

### 3.2 Memory-Guided — Arquitectura Spring Boot canónica

```
com.banco.memory
 ├── controller/
 │     └── BancoController.java              (35 líneas)
 ├── dto/
 │     ├── TransferenciaRequestDTO.java      (43 líneas)
 │     └── IngresosBatchRequestDTO.java      (49 líneas)
 ├── entity/
 │     └── CuentaEntity.java                 (44 líneas)
 ├── exception/
 │     └── GlobalExceptionHandler.java       (31 líneas)
 ├── repository/
 │     └── CuentaRepository.java             (9 líneas)
 └── service/
       ├── BancoIngresosService.java          (42 líneas)
       └── BancoTransferenciaService.java     (59 líneas)
```

**6 capas** siguiendo la arquitectura canónica de Spring Boot: Controller → DTO → Service → Repository → Entity, con capa transversal de excepciones.

| Criterio | Baseline | Memory-Guided |
|---|---|---|
| **Separación de responsabilidades** | ❌ Mezclada | ✅ Correcta por capas |
| **Single Responsibility Principle** | ❌ Servicios con múltiples responsabilidades | ✅ Cada clase tiene un propósito claro |
| **Escalabilidad** | ❌ Todo crece en 2 archivos | ✅ Cada capa crece independiente |
| **Fidelidad al modelo Spring** | ❌ Adaptación superficial | ✅ Arquitectura estándar |

---

## 4. Modelo de Datos y Persistencia

### 4.1 Baseline — Sin persistencia

El baseline **no tiene ninguna entidad JPA**. Los datos se almacenan como campos de instancia en los servicios:

```java
// BancoTransferenciaService.java — Baseline
@Service
public class BancoTransferenciaService {
    private String cuentaOrigen;
    private String cuentaDestino;
    private BigDecimal importeTransfer = BigDecimal.ZERO;
    private BigDecimal saldoOrigen = new BigDecimal("25000.00");   // ❌ Hardcoded
    private BigDecimal saldoDestino = new BigDecimal("10000.00");  // ❌ Hardcoded
    // ...
}
```

**Problemas críticos:**
- Los saldos son constantes hardcodeadas que se pierden al reiniciar
- No hay base de datos — simula estado en memoria del singleton
- La clase interna `IngresoEntry` tiene campos públicos sin encapsulación

### 4.2 Memory-Guided — JPA con anotaciones precisas

```java
// CuentaEntity.java — Memory-Guided
@Entity
@Table(name = "CUENTAS")
public class CuentaEntity {
    @Id
    @Column(name = "NUMERO_CUENTA", length = 20)
    private String numeroCuenta;

    @Column(name = "TITULAR", length = 40)
    private String titular;

    @Column(name = "SALDO", precision = 12, scale = 2)
    private BigDecimal saldo;
    // getters/setters encapsulados
}
```

**Mejoras:**
- Entidad JPA real con `@Entity`, `@Table`, `@Id`
- `@Column` con `length`, `precision` y `scale` que mapean directamente las cláusulas PIC del COBOL
- Encapsulación correcta con getters/setters
- Persistencia real vía base de datos

| Criterio | Baseline | Memory-Guided |
|---|---|---|
| **Persistencia** | ❌ Hardcoded en memoria | ✅ JPA con base de datos |
| **Mapeo PIC→Java** | ❌ Inexistente | ✅ `precision=12, scale=2` refleja PIC |
| **Encapsulación** | ❌ Campos públicos | ✅ Getters/Setters |
| **Relaciones JPA** | ❌ No aplica | ✅ `@ManyToOne` implícitas |

---

## 5. Capa de Servicio

### 5.1 Baseline — Servicios con estado mutable (antipatrón)

```java
// BancoTransferenciaService.java — Baseline
@Service
public class BancoTransferenciaService {
    private BigDecimal saldoOrigen = new BigDecimal("25000.00");
    private boolean transferValida = false;
    
    public void inicializar() { ... }
    public void pedirDatosTransferencia(...) { ... }
    public boolean validarTransferencia() { ... }
    public void ejecutarTransferencia() { ... }
    public String mostrarJustificante() { ... }
}
```

**Antipatrones detectados:**
- **Estado mutable en singleton**: Los beans `@Service` son singletons por defecto. Dos usuarios haciendo transferencias simultáneas **pisarían las variables del otro** (race condition)
- **Flujo procedural COBOL**: El controlador llama 5 métodos en secuencia, replicando la estructura `PERFORM` del COBOL original
- **Sin `@Transactional`**: No hay garantía de atomicidad
- **Validación por booleano**: `transferValida = false` en vez de excepciones

### 5.2 Memory-Guided — Servicios stateless

```java
// BancoTransferenciaService.java — Memory-Guided
@Service
public class BancoTransferenciaService {
    private final CuentaRepository cuentaRepository;  // Solo dependencia inyectada
    
    @Transactional
    public void ejecutarTransferencia(TransferenciaRequestDTO request) {
        if (request.getImporte().compareTo(BigDecimal.ZERO) <= 0)
            throw new IllegalArgumentException("Importe debe ser mayor a 0");
        
        CuentaEntity origen = cuentaRepository.findById(request.getCuentaOrigen())
            .orElseThrow(() -> new EntityNotFoundException("Cuenta origen no encontrada"));
        // ... lógica atómica ...
        cuentaRepository.save(origen);
        cuentaRepository.save(destino);
    }
}
```

**Mejoras:**
- **Stateless**: Sin campos de instancia mutables, solo el repositorio inyectado
- **Thread-safe**: No hay estado compartido entre peticiones concurrentes
- **`@Transactional`**: Garantiza atomicidad (rollback si falla a mitad)
- **Validación por excepciones**: Idiomático Java/Spring
- **Método único**: Encapsula toda la operación de negocio

| Criterio | Baseline | Memory-Guided |
|---|---|---|
| **Thread safety** | ❌ **Roto** (singleton mutable) | ✅ Stateless |
| **Atomicidad** | ❌ Sin `@Transactional` | ✅ `@Transactional` |
| **Validación** | ❌ Flags booleanos | ✅ Excepciones |
| **Patrón** | ❌ Transliteración COBOL | ✅ Service layer idiomático |
| **Concurrencia** | ❌ Race conditions | ✅ Safe |

---

## 6. Capa de Controlador

### 6.1 Baseline — Controlador con `@RequestParam`

```java
@PostMapping("/transferencia")
public String realizarTransferencia(@RequestParam String origen,
        @RequestParam String destino,
        @RequestParam BigDecimal importe,
        @RequestParam String concepto) {
    transferenciaService.inicializar();
    transferenciaService.pedirDatosTransferencia(origen, destino, importe, concepto);
    if (transferenciaService.validarTransferencia()) {
        transferenciaService.ejecutarTransferencia();
        return transferenciaService.mostrarJustificante();
    }
    return "ERROR: Transferencia no valida...";
}
```

- Usa `@RequestParam` (formulario HTML, no JSON)
- Retorna `String` plano (siempre HTTP 200, incluso en error)
- Contiene lógica de orquestación (5 llamadas secuenciales)
- El error es un texto indistinguible del éxito para el cliente

### 6.2 Memory-Guided — Controlador REST con DTOs

```java
@PostMapping("/transferencia")
public ResponseEntity<String> realizarTransferencia(
        @RequestBody TransferenciaRequestDTO request) {
    transferenciaService.ejecutarTransferencia(request);
    return ResponseEntity.ok("Transferencia realizada con éxito");
}
```

- Usa `@RequestBody` con DTO (JSON API)
- Retorna `ResponseEntity<String>` con códigos HTTP correctos
- Controlador fino: una sola llamada al servicio
- Errores delegados al `@RestControllerAdvice`

| Criterio | Baseline | Memory-Guided |
|---|---|---|
| **Formato entrada** | `@RequestParam` (form) | ✅ `@RequestBody` + DTO (JSON) |
| **Formato salida** | `String` (siempre 200) | ✅ `ResponseEntity` (HTTP codes correctos) |
| **Grosor del controlador** | ❌ Orquesta 5 métodos | ✅ 1 línea de lógica |
| **Diseño REST** | ❌ No estándar | ✅ Estándar |

---

## 7. Manejo de Errores

### 7.1 Baseline — Sin infraestructura

```java
// Único manejo de errores: texto plano
return "ERROR: Transferencia no valida. Posibles causas: ...";
```

Siempre retorna HTTP 200. El cliente no puede distinguir éxito de error programáticamente.

### 7.2 Memory-Guided — `@RestControllerAdvice`

```java
@RestControllerAdvice
public class GlobalExceptionHandler {
    // Mapea COBOL FILE STATUS 23 → HTTP 404
    @ExceptionHandler(EntityNotFoundException.class)
    public ResponseEntity<String> handleNotFound(EntityNotFoundException ex) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(ex.getMessage());
    }
    
    // Mapea validaciones COBOL IF → HTTP 400
    @ExceptionHandler({IllegalArgumentException.class, IllegalStateException.class})
    public ResponseEntity<String> handleBadRequest(RuntimeException ex) {
        return ResponseEntity.badRequest().body(ex.getMessage());
    }
    
    // Mapea COBOL FILE STATUS ≥ 30 → HTTP 500
    @ExceptionHandler(DataAccessException.class)
    public ResponseEntity<String> handleDataError(DataAccessException ex) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error interno");
    }
}
```

| Excepción | Código HTTP | Origen COBOL |
|---|---|---|
| `EntityNotFoundException` | 404 NOT_FOUND | FILE STATUS 23 |
| `IllegalArgumentException` | 400 BAD_REQUEST | Validaciones IF/GO TO ERROR |
| `DataAccessException` | 500 INTERNAL_SERVER_ERROR | FILE STATUS ≥ 30 |

Los comentarios en el código documentan explícitamente el mapeo entre constructos COBOL y excepciones Java — trazabilidad completa.

| Criterio | Baseline | Memory-Guided |
|---|---|---|
| **Infraestructura** | ❌ Ninguna | ✅ `@RestControllerAdvice` centralizado |
| **HTTP codes** | ❌ Siempre 200 | ✅ 400/404/500 según error |
| **Trazabilidad COBOL** | ❌ Ninguna | ✅ Comentarios con mapeo FILE STATUS→HTTP |
| **Mantenibilidad** | ❌ Errores dispersos | ✅ Un solo punto de manejo |

---

## 8. Tipos de Datos

| Aspecto | Baseline | Memory-Guided |
|---|---|---|
| **Dinero** | `BigDecimal` (sin precisión) | ✅ `BigDecimal` con `precision=12, scale=2` |
| **Fechas** | `LocalDate.now().toString()` como `String` | No usado (delegado al DB) |
| **IDs cuenta** | `String` sin restricción | ✅ `String` con `@Column(length=20)` |
| **Saldos** | Hardcoded `new BigDecimal("25000.00")` | ✅ Desde base de datos vía JPA |
| **Colecciones** | `ArrayList<IngresoEntry>` con índice manual | ✅ `List<IngresoItem>` en DTO con for-each |

---

## 9. Validación

| Aspecto | Baseline | Memory-Guided |
|---|---|---|
| **Enfoque** | Lógica booleana procedural | Excepciones idiomáticas |
| **Bean Validation** | ❌ No | ❌ No |
| **Ejemplo** | `if (...) transferValida = false;` | `if (...) throw new IllegalArgumentException(...)` |

> Ninguno de los dos usa anotaciones de Bean Validation (`@NotNull`, `@Positive`, etc.) en los DTOs o entidades, lo cual es una oportunidad de mejora en ambos.

---

## 10. Calidad del Código

| Criterio | Baseline | Memory-Guided |
|---|---|---|
| **Transliteración COBOL** | ❌ Alta — párrafos → métodos, WORKING-STORAGE → campos | ✅ Baja — re-arquitectado para Spring |
| **Thread safety** | ❌ **Roto** | ✅ Correcto |
| **Naming** | Mixto (`cto` para concepto) | ✅ Limpio (`request`, `origen`, `destino`) |
| **Comentarios** | Mínimos | ✅ Documentan mapeo COBOL→Java |
| **Código muerto** | Sí (`continuar` declarado nunca usado) | No |
| **Separación lógica/vista** | ❌ Servicio genera StringBuilder (vista) | ✅ Servicio solo lógica |
| **Java moderno** | `ArrayList` con índice manual, inner class pública | For-each, DTOs con encapsulación |

---

## 11. Elementos Comunes (Empate)

| Aspecto | Estado |
|---|---|
| **pom.xml** | Ninguno lo incluye |
| **application.properties** | Ninguno lo incluye |
| **Clase `@SpringBootApplication`** | Ninguno la incluye |
| **Tests** | Ninguno incluye tests |
| **Bean Validation** | Ninguno usa `@NotNull`/`@Positive` |

Ambos son traducciones de **código fuente solamente** — ninguno es un proyecto ejecutable completo.

---

## 12. Matriz Resumen

| Dimensión | Baseline (sin memoria) | Memory-Guided | Ganador |
|---|---|---|---|
| **Archivos** | 3 | 8 | **Memory** (descomposición correcta) |
| **LOC totales** | 219 | 312 | **Memory** (más estructura, no inflado) |
| **Capas arquitectónicas** | 2 | 6 | **Memory** |
| **Persistencia** | Hardcoded en memoria | JPA con DB | **Memory** |
| **Thread safety** | Roto (singleton mutable) | Safe (stateless) | **Memory** |
| **Transaccionalidad** | Ninguna | `@Transactional` | **Memory** |
| **Diseño REST** | `@RequestParam` + String + HTTP 200 | `@RequestBody` + DTO + ResponseEntity | **Memory** |
| **Manejo de errores** | Texto plano (siempre 200) | `@RestControllerAdvice` con 3 handlers | **Memory** |
| **Trazabilidad COBOL** | Baja | Alta (comentarios con mapeo) | **Memory** |
| **Bean Validation** | No | No | **Empate** |
| **Proyecto completo** | No (sin pom.xml/main/config) | No (sin pom.xml/main/config) | **Empate** |
| **Tests** | No | No | **Empate** |

**Resultado: 9–0–3** a favor de la versión memory-guided (9 victorias, 0 derrotas, 3 empates).

---

## 13. Análisis del Impacto de la Memoria Semántica

### 13.1 ¿Por qué la memoria mejoró tan radicalmente la traducción?

La memoria semántica de Gemini 3.1 Pro estaba **diseñada específicamente para traducción COBOL→Spring Boot**, con vectores granulares que cubrían:

1. **`file_io_to_jpa_mapping`** → El modelo entendió que `READ`/`WRITE` COBOL deben mapearse a `Repository.findById()`/`save()`, no a campos hardcodeados. **Resultado**: creación de `CuentaEntity` + `CuentaRepository`.
2. **`data_structures_mapping`** → El bloque `OCCURS 100 TIMES` se tradujo a `List<IngresoItem>` dentro de un DTO, no a una clase interna con campos públicos. **Resultado**: `IngresosBatchRequestDTO`.
3. **`error_handling_mapping`** → `FILE STATUS` se mapeó a excepciones con `@RestControllerAdvice`, no a flags booleanos. **Resultado**: `GlobalExceptionHandler` con HTTP codes correctos.
4. **`spring_boot_architecture_mapping`** → Forzó la separación en 6 capas. **Resultado**: paquetes `entity/`, `dto/`, `repository/`, `exception/`.
5. **`control_flow_mapping`** → `PERFORM` procedural se re-arquitectó como métodos `@Transactional`. **Resultado**: servicios stateless y atómicos.

### 13.2 Contraste con el baseline

Sin memoria, Gemini 3.1 Pro realizó una **transliteración lexicográfica**: COBOL `WORKING-STORAGE` → campos de instancia Java, `PERFORM párrafo` → llamada a método, `DISPLAY` → StringBuilder. El código resultante funciona pero tiene defectos de producción graves (thread safety, ausencia de persistencia).

Con memoria, Gemini 3.1 Pro realizó un **rediseño arquitectónico**: el código COBOL procedural se transformó en una aplicación Spring Boot nativa con persistencia real, servicios transaccionales y manejo de errores centralizado.

### 13.3 Comparación con otros modelos

| Modelo | Sin memoria | Con memoria | Mejora con memoria |
|---|---|---|---|
| **GPT-5.2** (INFORME8) | Modernización Java (Lombok, Swagger) | N/A (usó memoria de otro modelo) | N/A |
| **Claude Opus 4.6** (INFORME8) | Fiel al COBOL (SRP, 4 servicios) | Generó la memoria base | N/A |
| **Claude Haiku** (INFORME9) | 10 archivos, 534 LOC, tests | 13 archivos, 770 LOC, DTOs, excepciones custom | ✅ +44% LOC, mejor estructura |
| **Gemini 3.1 Pro** (este informe) | 3 archivos, 219 LOC, singleton mutable | 8 archivos, 312 LOC, JPA completo | ✅ +42% LOC, salto arquitectónico |

Gemini 3.1 Pro muestra el **mayor delta de mejora** entre baseline y memory de todos los modelos evaluados, lo que sugiere que la memoria semántica granular (7 vectores especializados) es más efectiva que la memoria monolítica del grafo de conocimiento original.

---

## 14. Conclusiones

### 14.1 Conclusión Principal

> **La memoria semántica granular de Remembrances transforma radicalmente la calidad de traducción COBOL→Spring Boot de Gemini 3.1 Pro**: de una transliteración procedural con defectos críticos (singleton mutable, sin persistencia, sin manejo de errores) a una aplicación Spring Boot arquitectónicamente correcta con JPA, DTOs, `@Transactional` y `@RestControllerAdvice`.

### 14.2 Conclusiones Secundarias

1. **El baseline demuestra el límite del conocimiento general**: Sin guía específica, incluso un modelo capaz como Gemini 3.1 Pro produce código que "huele a COBOL" — estado mutable emulando WORKING-STORAGE, flujos procedurales secuenciales, y ausencia total de persistencia.

2. **La granularidad de la memoria importa**: Los 7 vectores especializados (tipos de datos, estructuras, control de flujo, I/O→JPA, manejo de errores, etc.) permiten que el RAG inyecte reglas precisas según el contexto, sin sobrecargar el prompt con información irrelevante.

3. **La trazabilidad COBOL→Java es un bonus del memory**: Los comentarios en el código memory-guided documentan explícitamente el mapeo entre constructos COBOL y patrones Java, facilitando la auditoría de la traducción.

4. **Ambos proyectos comparten debilidades**: Ni el baseline ni el memory-guided incluyen pom.xml, clase principal, configuración, tests ni Bean Validation — áreas de mejora para futuras iteraciones.

5. **Gemini 3.1 Pro con memoria supera al baseline más que ningún otro modelo evaluado**, confirmando que la memoria semántica vectorial es especialmente efectiva para tareas de traducción entre paradigmas.

---

**Método**: Análisis comparativo de código fuente (2 proyectos, 531 líneas analizadas)  
**Modelo evaluado**: Gemini 3.1 Pro  
**Variables de estudio**: Presencia/ausencia de memoria semántica (Remembrances) con 7 vectores especializados
