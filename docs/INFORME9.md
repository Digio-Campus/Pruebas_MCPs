# INFORME 9 — Comparativa: `banco-ingresos-spring` vs `spring-boot-banco-ingresos`

## 1. Contexto

Ambos proyectos son traducciones del **mismo programa COBOL** (`BANCO-INGRESOS`) a una API REST con Spring Boot. La diferencia principal radica en la **sesión de generación** y la **profundidad de la traducción**:

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| **Generado por** | GitHub Copilot | GitHub Copilot (con Memory MCP) |
| **Enfoque** | Conversión directa a REST | Traducción con trazabilidad COBOL |
| **Spring Boot** | 3.2.0 | 3.2.2 |
| **Paquete base** | `com.banco.ingresos` | `com.banco` |

---

## 2. Estadísticas generales

| Métrica | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Archivos `.java` | 10 | 13 |
| Líneas de código Java | 534 | 770 |
| DTOs | 2 (`IngresoDTO`, `ResumenIngresoDTO`) | 4 (`IngresoPedidoDTO`, `IngresoResponseDTO`, `CuentaBancariaRequestDTO`, `CuentaBancariaResponseDTO`) |
| Excepciones custom | 0 (usa `RuntimeException`) | 2 (`CuentaNoEncontradaException`, `OperacionInvalidaException`) |
| Tests | 4 tests de integración (103 líneas) | 0 |
| Documentación `.md` | 7 archivos | 9 archivos |
| Otros recursos | — | `messages.properties`, `postman_collection.json` |

> `spring-boot-banco-ingresos` tiene **~44 % más líneas** de código Java, debido a DTOs adicionales, excepciones personalizadas y logging más detallado.

---

## 3. Dependencias (`pom.xml`)

| Dependencia | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| spring-boot-starter-web | ✅ | ✅ |
| spring-boot-starter-data-jpa | ✅ | ✅ |
| spring-boot-starter-validation | ✅ | ✅ |
| H2 Database | ✅ | ✅ |
| Lombok | ✅ | ✅ |
| spring-boot-starter-test | ✅ | ✅ |
| **jakarta.validation-api** (explícita) | ❌ | ✅ |
| **jakarta.persistence-api** (explícita) | ❌ | ✅ |

**Análisis:** Las dependencias son prácticamente idénticas. `spring-boot-banco-ingresos` añade explícitamente las APIs de Jakarta (redundante ya que Spring Boot Starters las incluyen transitivamente), lo cual sugiere un enfoque más defensivo.

---

## 4. Configuración

### `banco-ingresos-spring` — `application.properties`

```properties
server.port=8080
server.servlet.context-path=/
spring.jpa.show-sql=true
logging.level.com.banco.ingresos=DEBUG
logging.level.org.hibernate.SQL=DEBUG
```

### `spring-boot-banco-ingresos` — `application.properties`

```properties
server.port=8080
server.servlet.context-path=/banco-ingresos
spring.jpa.show-sql=false
logging.level.com.banco=DEBUG
logging.pattern.console=%d{yyyy-MM-dd HH:mm:ss} - %msg%n
spring.messages.basename=messages
```

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Context path | `/` (raíz) | `/banco-ingresos` |
| SQL en consola | ✅ Activado | ❌ Desactivado |
| Logging Hibernate | DEBUG + TRACE | Solo app |
| Patrón de log | Por defecto (Spring) | Personalizado (`yyyy-MM-dd HH:mm:ss`) |
| Messages i18n | ❌ | ✅ (`messages.properties`) |
| Comentarios COBOL en config | ❌ | ✅ (con referencias a secciones COBOL) |

**Análisis:** `spring-boot-banco-ingresos` tiene una configuración más orientada a producción (sin SQL verbose, con context-path explícito y patrón de log limpio). También incluye mensajes de validación internacionalizables.

---

## 5. Modelo de datos

### 5.1. `CuentaBancaria`

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Lombok | `@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder` | `@Data @NoArgsConstructor @AllArgsConstructor @Builder` |
| Campo `saldo` | ❌ No existe | ✅ `BigDecimal saldo` con `@Builder.Default = ZERO` |
| Campo `fechaCreacion` | ❌ | ✅ `String fechaCreacion` |
| `@Column(name=...)` explícito | ❌ | ✅ En todos los campos |
| `@Column(unique=true)` en numeroCuenta | ❌ | ✅ |
| Validaciones en entidad | ❌ | ✅ (`@NotBlank`, `@Size`) |
| Precisión del saldo | N/A | `precision=12, scale=2` |
| Método `agregarIngreso()` | ❌ | ✅ (actualiza saldo y vincula relación bidireccional) |
| Método `calcularSumaTotal()` / `calcularTotalIngresos()` | ✅ | ✅ |
| Referencia `mappedBy` | `"cuenta"` | `"cuentaBancaria"` |
| `@Builder.Default` en listas | ❌ | ✅ |

**Análisis:** `spring-boot-banco-ingresos` tiene un modelo más completo: incluye `saldo` como campo persistido (que se actualiza al agregar ingresos), validaciones a nivel de entidad, nombres de columna explícitos y `unique=true` en `numeroCuenta`. El campo `saldo` en `spring-boot-banco-ingresos` refleja fielmente la variable `WS-SALDO` del COBOL original.

### 5.2. `Ingreso`

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Lombok | `@Getter @Setter` | `@Data` |
| Relación ManyToOne | `@JoinColumn(name="cuenta_id")` | `@JoinColumn(name="cuenta_bancaria_id")` |
| `FetchType.LAZY` | ✅ | ✅ |
| `optional = false` | ❌ | ✅ |
| Campo `descripcion` | ❌ | ✅ (campo adicional) |
| Timestamp | `@PrePersist` callback | `@Builder.Default = LocalDateTime.now()` |
| Validaciones | `@NotNull`, `@DecimalMin("0.01")`, `@NotBlank` | `@Positive`, `@NotBlank`, `@Size` |
| `@Column(name=...)` explícito | Solo `fecha_ingreso` | En todos los campos |
| `@Column(precision, scale)` | ❌ | ✅ (`precision=10, scale=2`) |

**Análisis:** Ambos usan `LocalDateTime` para fechas (correctamente). `spring-boot-banco-ingresos` es más completo con nombres de columna explícitos, precision/scale para BigDecimal y campo `descripcion` adicional. La estrategia de timestamp difiere: `@PrePersist` (más robusto) vs `@Builder.Default` (más simple pero puede no funcionar en todos los escenarios JPA).

---

## 6. DTOs

### `banco-ingresos-spring` — 2 DTOs

| DTO | Propósito | Campos |
|---|---|---|
| `IngresoDTO` | Request de ingreso | `importe`, `concepto` |
| `ResumenIngresoDTO` | Respuesta con resumen | `numeroCuenta`, `titular`, `numeroIngresos`, `sumaTotal`, `detalleIngresos` (inner class `DetalleIngresoDTO`) |

### `spring-boot-banco-ingresos` — 4 DTOs

| DTO | Propósito | Campos |
|---|---|---|
| `IngresoPedidoDTO` | Request de ingreso | `importe`, `concepto`, `descripcion` |
| `IngresoResponseDTO` | Respuesta individual | `id`, `importe`, `concepto`, `fechaIngreso`, `descripcion` |
| `CuentaBancariaRequestDTO` | Request crear cuenta | `numeroCuenta`, `titular` |
| `CuentaBancariaResponseDTO` | Respuesta completa | `id`, `numeroCuenta`, `titular`, `saldo`, `numeroIngresos`, `sumaTotalIngresos`, `ingresos` |

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Separación Request/Response | Parcial (solo `IngresoDTO` para ambos) | ✅ Completa (Request ≠ Response) |
| Inner classes | ✅ `DetalleIngresoDTO` dentro de `ResumenIngresoDTO` | ❌ Clases separadas |
| `@JsonProperty` explícito | ✅ En todos los campos | ❌ (usa convención de nombres) |
| Validaciones en DTOs | ✅ | ✅ |
| Nombrado | Inglés (`IngresoDTO`) | Español (`IngresoPedidoDTO`, `IngresoResponseDTO`) |

**Análisis:** `spring-boot-banco-ingresos` aplica correctamente el patrón **CQRS** (Command/Query) con DTOs de request y response separados. `banco-ingresos-spring` usa un DTO mixto pero compensa con `@JsonProperty` explícito para control de serialización.

---

## 7. Servicio

### `banco-ingresos-spring` — `IngresoService` (121 líneas)

- Métodos: `crearCuenta`, `registrarIngreso`, `obtenerResumen`, `obtenerCuenta`, `obtenerSumaTotal`, `eliminarCuenta`
- Excepciones: `RuntimeException` genérico para cuenta no encontrada
- Logging: No incluye
- Patrón: Cada operación es un método independiente

### `spring-boot-banco-ingresos` — `IngresosService` (269 líneas)

- Métodos: `procesarIngresos` (orquestador), `inicializar`, `obtenerOCrearCuenta`, `registrarIngresos`, `calcularTotal`, `mostrarResumen`, `finalizar`, `obtenerCuenta`, `obtenerCuentaPorNumero`, `agregarIngreso`, `construirResponse`
- Excepciones: `CuentaNoEncontradaException`, `OperacionInvalidaException`
- Logging: Extensivo con `@Slf4j` (simula DISPLAY del COBOL)
- Patrón: **Reproduce el flujo completo del párrafo 0000-PRINCIPAL del COBOL**

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Líneas | 121 | 269 |
| Fidelidad al COBOL | ⚠️ Parcial | ✅ Alta (reproduce párrafos) |
| Método orquestador | ❌ | ✅ `procesarIngresos()` (= 0000-PRINCIPAL) |
| Logging COBOL (DISPLAY) | ❌ | ✅ Detallado (`log.info` por cada acción) |
| Inyección de dependencias | `@Autowired` en 2 repos | `@Autowired` en 1 repo |
| `IngresoRepository` dedicado | ✅ | ❌ (cascade desde `CuentaBancaria`) |
| Búsqueda por nº cuenta | ❌ (solo por ID) | ✅ `obtenerCuentaPorNumero` |
| Validación manual de importe | ❌ (delega en Bean Validation) | ✅ (doble validación: Bean + manual) |
| Excepciones tipadas | ❌ (`RuntimeException`) | ✅ (custom exceptions) |
| Método `eliminarCuenta` | ✅ | ❌ |
| Obtener/Crear cuenta automático | ❌ (debe crear antes) | ✅ `obtenerOCrearCuenta` |

**Análisis:** Los dos servicios representan enfoques muy diferentes:
- `banco-ingresos-spring` es un servicio **REST-first**: operaciones CRUD independientes, sin flujo de negocio.
- `spring-boot-banco-ingresos` es una **traducción literal** del flujo COBOL: un método orquestador que llama a los párrafos (1000-INICIALIZAR, 3000-REGISTRAR-INGRESOS, etc.) en secuencia, con logging que replica los DISPLAY del COBOL.

---

## 8. Controlador

### `banco-ingresos-spring` — `IngresoController`

```
POST   /api/cuentas                        → crearCuenta
POST   /api/cuentas/{id}/ingresos          → registrarIngreso
GET    /api/cuentas/{id}/resumen            → obtenerResumen
GET    /api/cuentas/{id}                    → obtenerCuenta
DELETE /api/cuentas/{id}                    → eliminarCuenta
GET    /api/cuentas/{id}/suma-total         → obtenerSumaTotal
```

### `spring-boot-banco-ingresos` — `IngresosController`

```
POST   /api/ingresos/procesar                          → procesarIngresos
GET    /api/ingresos/cuentas/{id}                      → obtenerCuenta
GET    /api/ingresos/cuentas/numero/{numeroCuenta}     → obtenerCuentaPorNumero
POST   /api/ingresos/cuentas/{cuentaId}/agregar-ingreso → agregarIngreso
```

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Base path | `/api/cuentas` | `/api/ingresos` |
| Diseño REST | ✅ Recursos/verbos HTTP estándar | ⚠️ Orientado a acciones (verbos en URLs) |
| Endpoints | 6 | 4 |
| `@CrossOrigin` | ✅ | ❌ |
| Crear cuenta (separado) | ✅ `POST /api/cuentas` | ❌ (integrado en `procesarIngresos`) |
| Eliminar cuenta | ✅ `DELETE /{id}` | ❌ |
| Consulta por nº cuenta | ❌ | ✅ `GET /cuentas/numero/{nro}` |
| Ingreso batch | ❌ (uno a uno) | ✅ `procesarIngresos` (lista de ingresos) |
| Logging en controlador | ❌ | ✅ (`@Slf4j`) |
| Retorno | `ResponseEntity<T>` | `ResponseEntity<T>` |
| Crear cuenta como parámetro | `@RequestParam` | `@RequestParam` (en `procesar`) |

**Análisis:** `banco-ingresos-spring` sigue el estilo **RESTful** convencional (recursos como sustantivos, verbos HTTP como acciones). `spring-boot-banco-ingresos` adopta un estilo más **RPC** que refleja el flujo del programa COBOL: un endpoint `/procesar` que ejecuta todo el programa de punta a punta.

---

## 9. Manejo de excepciones

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Excepciones custom | ❌ (`RuntimeException`) | ✅ 2 clases propias |
| `GlobalExceptionHandler` | ❌ | ✅ con 3 handlers |
| Respuesta de error | Java por defecto (stack trace en dev) | `Map<String, Object>` con `timestamp`, `estado`, `error`, `mensaje` |
| Catch-all (`Exception.class`) | ❌ | ✅ |
| `@RestControllerAdvice` | ❌ | ✅ |
| Logging de errores | ❌ | ✅ (`log.error`) |

**Análisis:** `spring-boot-banco-ingresos` tiene un manejo de excepciones profesional y completo. `banco-ingresos-spring` lanza `RuntimeException` sin interceptor, lo que expone stack traces potencialmente sensibles al cliente.

---

## 10. Repositorios

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| `CuentaBancariaRepository` | ✅ con `findByNumeroCuenta` | ✅ con `findByNumeroCuenta` |
| `IngresoRepository` | ✅ con `findByCuentaId` | ❌ (no necesario, cascade desde cuenta) |

**Análisis:** `banco-ingresos-spring` tiene un repo dedicado para `Ingreso`, permitiendo queries directas. `spring-boot-banco-ingresos` gestiona los ingresos exclusivamente a través del cascade de `CuentaBancaria`, lo cual simplifica el código pero limita la consulta independiente de ingresos.

---

## 11. Testing

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Clase de test | `BancoIngresosApplicationTests` | No tiene |
| Tipo | `@SpringBootTest` (integración) | — |
| Tests | 4 | 0 |
| Cobertura | Crear cuenta, registrar ingreso, múltiples ingresos, obtener resumen | — |
| Assertions | `assertNotNull`, `assertEquals` con valores exactos | — |

### Tests de `banco-ingresos-spring`:

1. **`testCrearCuenta`** — Crea cuenta y verifica numeroCuenta/titular
2. **`testRegistrarIngreso`** — Registra 1 ingreso y verifica resumen
3. **`testRegistrarMultiplesIngresos`** — Registra 3 ingresos y verifica suma (1750.75)
4. **`testObtenerResumen`** — Obtiene resumen y verifica todos los campos

**Análisis:** `banco-ingresos-spring` tiene tests de integración reales que verifican la lógica de negocio completa. `spring-boot-banco-ingresos` no incluye ningún test, lo cual es una carencia significativa considerando su mayor complejidad.

---

## 12. Trazabilidad COBOL

| Aspecto | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Javadoc con referencia COBOL | ⚠️ Básica ("Equivalente a...") | ✅ Detallada ("Párrafo X, Mapeo de...") |
| Comentarios en config | ❌ | ✅ ("MAPEO: FILE-CONTROL →...") |
| Referencia a `memory` | ❌ | ✅ ("Referencia memory: BLOQUE-...") |
| Logging tipo DISPLAY | ❌ | ✅ (reproduce DISPLAY del COBOL) |
| Flujo orquestador | ❌ | ✅ (0000-PRINCIPAL → procesarIngresos) |
| Nombres de párrafos en métodos | ⚠️ Genéricos | ✅ (`inicializar`, `finalizar`, `mostrarResumen`) |

**Análisis:** `spring-boot-banco-ingresos` fue generado con Memory MCP, por lo que mantiene referencias explícitas a los bloques del knowledge graph y reproduce fielmente la estructura del programa COBOL. `banco-ingresos-spring` tiene referencias COBOL mínimas.

---

## 13. Documentación del proyecto

| Documento | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| README.md | ✅ | ✅ |
| Arquitectura | ARCHITECTURE.md | ARQUITECTURA.md |
| Mapeo COBOL | MAPEO_DETALLADO.md | MAPEO_DETALLADO.md |
| Guía de ejecución | GUIA_EJECUCION.md | GUIA_EJECUCION.md |
| Ejemplos de prueba | EJEMPLOS_PRUEBA.md | EJEMPLOS_PRUEBA.md |
| Índice | INDEX.md | INDICE_DOCUMENTACION.md |
| Resumen conversión | RESUMEN_CONVERSION.md | RESUMEN_EJECUCION.md + SUMARIO_TRADUCCION.md |
| Inicio rápido | ❌ | ✅ INICIO_RAPIDO.md |
| Colección Postman | ❌ | ✅ `postman_collection.json` |
| `.gitignore` | ✅ | ✅ |

**Análisis:** `spring-boot-banco-ingresos` tiene documentación más extensa (9 archivos + Postman collection). Destaca la inclusión de una colección Postman lista para importar, lo que facilita la prueba inmediata de la API.

---

## 14. Tabla resumen de buenas prácticas

| Buena práctica | `banco-ingresos-spring` | `spring-boot-banco-ingresos` |
|---|---|---|
| Excepciones custom | ❌ | ✅ |
| `GlobalExceptionHandler` | ❌ | ✅ |
| Separación Request/Response DTOs | ❌ | ✅ |
| Tests | ✅ (4 tests) | ❌ |
| Diseño RESTful | ✅ | ⚠️ (orientado a acciones) |
| Validaciones en entidad | ❌ | ✅ |
| Validación en DTO + servicio | Parcial | ✅ (doble) |
| `@Column(name=...)` explícito | ❌ | ✅ |
| `unique=true` en numeroCuenta | ❌ | ✅ |
| `precision/scale` en BigDecimal | ❌ | ✅ |
| `optional=false` en @ManyToOne | ❌ | ✅ |
| `@Builder.Default` | ❌ | ✅ |
| Logging (`@Slf4j`) | ❌ | ✅ |
| Mensajes i18n | ❌ | ✅ |
| Colección Postman | ❌ | ✅ |
| `@CrossOrigin` | ✅ | ❌ |
| Endpoint DELETE | ✅ | ❌ |
| Campo saldo persistido | ❌ | ✅ |
| Fidelidad al COBOL | ⚠️ Baja | ✅ Alta |

---

## 15. Conclusiones

### `banco-ingresos-spring` (Copilot sin Memory)

**Fortalezas:**
- Diseño REST **más limpio y convencional** (recursos como sustantivos).
- **4 tests de integración** que cubren la lógica principal.
- Código más conciso y fácil de leer (534 líneas vs 770).
- `@CrossOrigin` para uso directo desde frontend.
- Endpoint DELETE para eliminar cuentas.
- `@PrePersist` para timestamp (más robusto con JPA).

**Debilidades:**
- Sin manejo de excepciones personalizado (`RuntimeException`).
- Sin logging.
- Sin campo `saldo` (no refleja `WS-SALDO` del COBOL).
- Sin validaciones a nivel de entidad.
- Poca trazabilidad hacia el programa COBOL original.
- `numeroCuenta` sin restricción `unique`.

### `spring-boot-banco-ingresos` (Copilot con Memory MCP)

**Fortalezas:**
- **Traducción fiel** del flujo del programa COBOL (párrafos como métodos).
- Manejo de excepciones profesional con `@RestControllerAdvice`.
- DTOs bien separados (Request ≠ Response).
- Logging extensivo que replica los DISPLAY del COBOL.
- Modelo más robusto (`saldo`, `unique`, `precision/scale`, `optional=false`).
- Mensajes de validación internacionalizables.
- Colección Postman incluida.
- Documentación más completa y con trazabilidad COBOL→Java.

**Debilidades:**
- **Sin tests** (carencia significativa).
- Endpoints con estilo RPC (verbos en URLs como `/procesar`, `/agregar-ingreso`).
- Más código para mantener (770 líneas, 13 archivos).
- Dependencias explícitas Jakarta redundantes.
- Sin `@CrossOrigin`.

### Valoración global

| Criterio | Ventaja |
|---|---|
| Diseño REST estándar | `banco-ingresos-spring` |
| Fidelidad al COBOL | `spring-boot-banco-ingresos` |
| Testing | `banco-ingresos-spring` |
| Manejo de errores | `spring-boot-banco-ingresos` |
| Robustez del modelo | `spring-boot-banco-ingresos` |
| Concisión del código | `banco-ingresos-spring` |
| Documentación y trazabilidad | `spring-boot-banco-ingresos` |
| Logging/Observabilidad | `spring-boot-banco-ingresos` |

### Impacto del Memory MCP

La diferencia más notable es la **trazabilidad**: `spring-boot-banco-ingresos`, generado con Memory MCP, mantiene un mapeo explícito entre cada párrafo COBOL y su equivalente Java, reproduciendo el flujo del programa original. Esto lo convierte en una traducción más fiel y auditable, aunque sacrifica convenciones REST puras en favor de preservar la estructura lógica del COBOL. El proyecto **ideal** combinaría los tests de `banco-ingresos-spring` con la robustez, excepciones y trazabilidad de `spring-boot-banco-ingresos`.

---

*Informe generado comparando el código fuente completo de ambos proyectos.*
