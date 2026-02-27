# INFORME 8 — Comparativa: `spring-boot-banco-opus` vs `spring-boot-gpt5_2`

## 1. Contexto

Ambos proyectos son traducciones de los mismos programas COBOL bancarios (BANCO-INGRESOS, BANCO-CONSULTA-SALDO, BANCO-TRANSFERENCIA, BANCO-EXTRACTO y HELLO-WORLD) a una API REST con Spring Boot. En este caso opus realizo la tarea sin memoria y genero recuerdos en la memoria, para que posteriormente el modelo gpt5.2 utilizase la información que ya existia en la memoria. La diferencia principal radica en **quién realizó la traducción**:

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| **Generado por** | GitHub Copilot (con Memory MCP) | GPT-5 (modelo directo) |
| **Enfoque** | Traducción fiel al COBOL original | Traducción con modernización Java |
| **artifactId** | `banco-cobol-traducido` | `spring-boot-gpt5_2` |
| **Versión** | `1.0.0` | `0.0.1-SNAPSHOT` |

---

## 2. Estadísticas generales

| Métrica | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Archivos `.java` | 24 | 25 |
| Líneas de código Java | 938 | 700 |
| Puerto | 8085 | 8080 |
| Formato de configuración | `application.properties` | `application.yml` |
| Tests | No | Sí (`contextLoads`) |
| UI | `index.html` (panel interactivo) | Swagger UI (SpringDoc) |

> `spring-boot-gpt5_2` consigue la misma funcionalidad con **~25 % menos líneas** gracias al uso de records y Lombok.

---

## 3. Dependencias (`pom.xml`)

| Dependencia | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| spring-boot-starter-web | ✅ | ✅ |
| spring-boot-starter-data-jpa | ✅ | ✅ |
| spring-boot-starter-validation | ✅ | ✅ |
| H2 Database | ✅ | ✅ |
| **Lombok** | ❌ | ✅ |
| **SpringDoc OpenAPI (Swagger)** | ❌ | ✅ (v2.3.0) |
| spring-boot-starter-test | ✅ | ✅ |

**Análisis:** `spring-boot-gpt5_2` añade Lombok para reducir boilerplate y SpringDoc para documentación automática de la API. `spring-boot-banco-opus` opta por no incorporar dependencias adicionales más allá de lo estrictamente necesario.

---

## 4. Arquitectura de servicios

### 4.1. `spring-boot-banco-opus` — 4 servicios separados

```
IngresoService           → registrar ingresos (batch)
ConsultaSaldoService     → consultar saldo
TransferenciaService     → realizar transferencia
ExtractoService          → obtener extracto
```

Cada servicio tiene responsabilidad única sobre una operación COBOL. Esta separación refleja fidedignamente la estructura original de programas COBOL independientes.

### 4.2. `spring-boot-gpt5_2` — 1 servicio unificado

```
BancoService → ingresar, consultarSaldo, transferir, extracto
```

Centraliza toda la lógica en un único servicio. Simplifica la inyección de dependencias pero mezcla responsabilidades.

### Valoración

| Criterio | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Principio de responsabilidad única (SRP) | ✅ Mejor separación | ⚠️ Clase con múltiples responsabilidades |
| Simplicidad | ⚠️ Más archivos | ✅ Un solo punto de entrada |
| Escalabilidad | ✅ Cada servicio crece independiente | ⚠️ El servicio crece sin límite |
| Fidelidad al COBOL | ✅ 1 programa = 1 servicio | ❌ Todo unificado |

---

## 5. Modelo de datos

### 5.1. `CuentaBancaria`

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| `@Column(nullable=false)` | No explícito | ✅ En todos los campos |
| Constructor JPA | `public` (por defecto) | `protected` (buena práctica JPA) |
| `getSaldoTotal()` | Método normal (no marcado) | `@Transient` (correctamente excluido de persistencia) |
| Getters/Setters | Manuales | Manuales (Lombok no se aplica a entidades) |

### 5.2. `Movimiento`

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Campo `fecha` | `String` | `LocalDateTime` ✅ |
| Campo `tipo` | `String` ("I"/"G") | `TipoMovimiento` enum (`I`, `G`) ✅ |
| `@ManyToOne` fetch | Por defecto (`EAGER`) | `FetchType.LAZY` ✅ |
| `optional` en relación | No especificado | `optional = false` ✅ |
| Mutabilidad | Setters en todos los campos | **Sin setters** (inmutable tras construcción) ✅ |
| Constructor JPA | `public` | `protected` ✅ |

**Análisis:** El modelo de `spring-boot-gpt5_2` aplica mejores prácticas JPA de forma consistente: tipado fuerte (enum + `LocalDateTime`), fetch lazy, inmutabilidad y constructores `protected`.

---

## 6. DTOs y Respuestas

### Enfoque radicalmente diferente

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Tipo de DTO | Clases Java (beans) | **Java Records** ✅ |
| Boilerplate | Getters/setters manuales (~30 líneas/DTO) | 1 línea por record |
| Inmutabilidad | Mutable (setters expuestos) | Inmutable por diseño ✅ |
| Inner classes | `IngresoDetalle`, `MovimientoDetalle` como inner | Sin inner classes |

### Ejemplo comparativo — `SaldoResponse`

**`spring-boot-banco-opus`** (~50 líneas):
```java
public class SaldoResponse {
    private String numeroCuenta;
    private String titular;
    // ... campos + getters + setters
}
```

**`spring-boot-gpt5_2`** (1 línea):
```java
public record SaldoResponse(String numeroCuenta, String titular,
        String tipoCuenta, BigDecimal saldoDisponible,
        BigDecimal saldoRetenido, BigDecimal saldoTotal) {}
```

---

## 7. Controladores

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Retorno | `ResponseEntity<T>` | DTO directo (sin wrapping) |
| Servicio inyectado | 1 servicio por controlador | Todos usan `BancoService` |
| Ruta Hello | `/hello` | `/api/hello` |
| Ingreso | Batch (`List<IngresoRequest>`) | Individual (`IngresoRequest`) |
| Extracto | Parámetro `saldoInicial` (BigDecimal) | Parámetro `limit` (int, default 50) |
| `@Valid` | ✅ | ✅ |

### Diferencias funcionales relevantes

1. **Ingreso batch vs individual:** `spring-boot-banco-opus` acepta una lista de ingresos en una sola petición (más fiel al COBOL que procesa lotes). `spring-boot-gpt5_2` acepta un solo ingreso por petición (estilo REST más estándar).

2. **Extracto:** `spring-boot-banco-opus` recibe un `saldoInicial` para calcular el balance. `spring-boot-gpt5_2` limita el número de movimientos (`limit`) y calcula el saldo inicial restando del saldo actual.

3. **ResponseEntity:** `spring-boot-banco-opus` envuelve todas las respuestas en `ResponseEntity`, siguiendo el patrón más común en APIs profesionales (permite control sobre headers y status). `spring-boot-gpt5_2` retorna directamente el DTO, lo cual es más conciso.

---

## 8. Repositorios

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Nombre | `CuentaRepository` | `CuentaBancariaRepository` |
| Anotación `@Repository` | ✅ Explícita | ❌ (innecesaria con Spring Data) |
| Query movimientos | `findByCuentaOrderByFechaAsc` | `findTop50ByCuentaNumeroCuentaOrderByFechaDesc` |
| Parámetro de búsqueda | Objeto `CuentaBancaria` | `String numeroCuenta` |
| Ordenación | Ascendente (cronológico) | Descendente (más reciente primero) |
| Límite de resultados | Sin límite | Top 50 |

**Análisis:** `spring-boot-gpt5_2` realiza la consulta directamente por `numeroCuenta` (String), evitando cargar la entidad previamente. Además, implementa paginación implicit con `Top50`. `spring-boot-banco-opus` requiere cargar la entidad `CuentaBancaria` primero para consultar sus movimientos.

---

## 9. Manejo de excepciones

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Handler global | `GlobalExceptionHandler` | `ApiExceptionHandler` |
| Handlers separados | 3 (uno por excepción) | 2 (agrupados) + 1 validation |
| Validación `@Valid` | No captura `MethodArgumentNotValidException` | ✅ Captura y formatea |
| `SaldoInsuficienteException` | `new SaldoInsuficienteException("mensaje")` | `new SaldoInsuficiente(cuenta, requerido, disponible)` |
| Tipo de retorno | `Map<String, String>` | `Map<String, Object>` |

**Análisis:** `spring-boot-gpt5_2` tiene un manejo de excepciones más robusto:
- Agrupa excepciones relacionadas en un solo handler.
- Captura errores de validación de beans (`@Valid`), lo que `spring-boot-banco-opus` no hace.
- La excepción `SaldoInsuficienteException` es estructurada (3 parámetros) vs un simple mensaje de texto.

---

## 10. Configuración y datos iniciales

### 10.1. Formato de configuración

**`spring-boot-banco-opus`** — `application.properties`:
```properties
server.port=8085
spring.jpa.hibernate.ddl-auto=create-drop
spring.sql.init.mode=always
```

**`spring-boot-gpt5_2`** — `application.yml`:
```yaml
server:
  port: 8080
spring:
  jpa:
    hibernate:
      ddl-auto: update
    open-in-view: false
```

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| ddl-auto | `create-drop` | `update` |
| `open-in-view` | No configurado (true por defecto) | `false` ✅ (evita lazy-loading accidental) |
| `DB_CLOSE_DELAY` | No configurado | `-1` (mantiene DB entre conexiones) |

### 10.2. Datos iniciales (seed data)

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Mecanismo | `data.sql` | `DataInitializer.java` (CommandLineRunner) |
| Idempotente | ❌ Siempre ejecuta | ✅ Verifica `count > 0` |
| Tipo de fecha | Strings (`'2024-01-15'`) | `LocalDate.parse().atStartOfDay()` |
| Cuentas insertadas | 3 | 3 (mismos datos) |
| Movimientos insertados | 8 | 8 (mismos datos) |

**Análisis:** `DataInitializer` de `spring-boot-gpt5_2` es más robusto: verifica si ya existen datos antes de insertar, y usa tipos Java correctos. `data.sql` es más simple pero se ejecuta cada vez que arranca la aplicación (compatible con `create-drop`).

---

## 11. Comisión en transferencias

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Configurable | ✅ `@Value("${...}")` | ❌ Hardcoded |
| Umbral | 3000 (inyectado) | 3000 (literal) |
| Porcentaje | 0.005 (inyectado) | 0.005 (literal) |
| Redondeo | No especificado | `setScale(2, HALF_UP)` ✅ |

**Análisis:** `spring-boot-banco-opus` permite configurar la comisión externamente sin recompilar (12-factor app). `spring-boot-gpt5_2` aplica redondeo correcto pero requiere cambio de código para modificar los valores.

---

## 12. Interfaz de usuario

### `spring-boot-banco-opus` — Panel HTML interactivo

Incluye un archivo `index.html` de 157 líneas con:
- 5 tarjetas interactivas (una por endpoint)
- Formularios con campos de entrada
- Botones de acción con fetch a la API
- Estilo oscuro personalizado
- Visualización JSON de respuestas

### `spring-boot-gpt5_2` — Swagger UI

Aprovecha SpringDoc OpenAPI para generar documentación interactiva automática.
- Ruta: `/swagger-ui.html`
- Sin código adicional (generado automáticamente)
- Permite probar todos los endpoints desde el navegador

### Valoración

| Criterio | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Esfuerzo de creación | Alto (HTML/CSS/JS manual) | Nulo (generado) |
| Personalización | ✅ Total | ⚠️ Limitada |
| UX para usuario final | ✅ Amigable | ⚠️ Técnica |
| Documentación de API | ❌ | ✅ Automática (OpenAPI 3.0) |
| Mantenimiento | ⚠️ Requiere actualizar HTML | ✅ Se actualiza sola |

---

## 13. Testing

| Aspecto | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Clase de test | No existe | `SpringBootGpt52ApplicationTests` |
| Tests de contexto | ❌ | ✅ `contextLoads()` |
| Tests unitarios | ❌ | ❌ |
| Tests de integración | ❌ | ❌ |

Ninguno de los dos proyectos tiene tests de lógica de negocio. `spring-boot-gpt5_2` al menos verifica que el contexto de Spring carga correctamente.

---

## 14. Tabla resumen de buenas prácticas

| Buena práctica | `spring-boot-banco-opus` | `spring-boot-gpt5_2` |
|---|---|---|
| Constructor `protected` en entidades | ❌ | ✅ |
| `@Transient` en campos calculados | ❌ | ✅ |
| `FetchType.LAZY` en relaciones | ❌ | ✅ |
| `nullable = false` en columnas | ❌ | ✅ |
| Entidades inmutables donde aplica | ❌ | ✅ |
| Java Records para DTOs | ❌ | ✅ |
| Enum para tipos fijos | ❌ (String) | ✅ (TipoMovimiento) |
| `LocalDateTime` para fechas | ❌ (String) | ✅ |
| Validación de `@Valid` capturada | ❌ | ✅ |
| `open-in-view: false` | ❌ | ✅ |
| Comisión configurable | ✅ | ❌ |
| SRP en servicios | ✅ | ❌ |
| Fidelidad al COBOL | ✅ | ⚠️ |
| Panel de usuario | ✅ | ❌ |
| Documentación API (Swagger) | ❌ | ✅ |
| Tests | ❌ | ⚠️ (mínimos) |
| Redondeo financiero | ❌ | ✅ |
| Seed data idempotente | ❌ | ✅ |

---

## 15. Conclusiones

### `spring-boot-banco-opus` (Copilot + Memory MCP)

**Fortalezas:**
- Traducción más fiel a la estructura original COBOL (1 programa → 1 servicio).
- Separación de responsabilidades superior (SRP).
- Comisión externalizada y configurable.
- Incluye panel HTML interactivo para demostración.
- Código autoexplicativo con Javadoc y Logger.

**Debilidades:**
- No aplica buenas prácticas JPA modernas (`@Transient`, `protected`, `LAZY`).
- DTOs verbosos con getters/setters manuales.
- Tipos débiles (`String` para fechas y tipos).
- Sin tests ni documentación automática de API.
- Seed data no idempotente.

### `spring-boot-gpt5_2` (GPT-5)

**Fortalezas:**
- Código más moderno e idiomático (records, enums, `LocalDateTime`).
- Mejores prácticas JPA consistentes en todo el proyecto.
- Menos código (700 vs 938 líneas) para la misma funcionalidad.
- Swagger integrado para documentación automática.
- Manejo de excepciones más robusto.
- Seed data idempotente.
- Redondeo financiero correcto.

**Debilidades:**
- Servicio monolítico que viola SRP.
- Se aleja de la estructura original COBOL.
- Comisión hardcoded (no configurable).
- Sin interfaz de usuario personalizada.
- Tests mínimos (solo `contextLoads`).

### Valoración global

| Criterio | Ventaja |
|---|---|
| Calidad técnica del código | `spring-boot-gpt5_2` |
| Fidelidad a la traducción COBOL | `spring-boot-banco-opus` |
| Arquitectura de servicios | `spring-boot-banco-opus` |
| Prácticas JPA modernas | `spring-boot-gpt5_2` |
| Concisión y mantenibilidad | `spring-boot-gpt5_2` |
| Experiencia de usuario | Empate (HTML vs Swagger) |
| Configurabilidad | `spring-boot-banco-opus` |

Un proyecto **ideal** combinaría la **arquitectura de servicios separados** de `spring-boot-banco-opus` con las **prácticas modernas de Java** de `spring-boot-gpt5_2`: records, enums, `LocalDateTime`, `@Transient`, `FetchType.LAZY`, constructor `protected`, Swagger, y seed data idempotente, manteniendo la comisión configurable y la fidelidad al flujo original COBOL.

---

*Informe generado comparando el código fuente completo de ambos proyectos.*
