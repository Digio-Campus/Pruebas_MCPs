# Banco Ingresos - Traducción COBOL → Spring Boot

## 📋 Descripción General

Este proyecto es la traducción completa del programa COBOL **BANCO-INGRESOS** a una aplicación **Spring Boot 3.2** moderna.

### Programa Original (COBOL)
- **PROGRAMA**: BANCO-INGRESOS
- **DESCRIPCIÓN**: Simula el ingreso de dinero en una cuenta bancaria. Permite registrar N ingresos y muestra la suma total.
- **AUTOR**: Proyecto MCPs
- **FECHA**: 2026-02-18

### Aplicación Traducida (Spring Boot)
- **Nombre**: banco-ingresos-service
- **Versión**: 1.0.0
- **Java**: 17+
- **Framework**: Spring Boot 3.2.2
- **Base de Datos**: H2 (en memoria)

---

## 📚 Estructura del Proyecto

```
spring-boot-banco-ingresos/
├── src/
│   ├── main/
│   │   ├── java/com/banco/
│   │   │   ├── BancoIngresosApplication.java         # Entrada principal (IDENTIFICATION DIVISION)
│   │   │   ├── controller/
│   │   │   │   └── IngresosController.java           # REST API (PROCEDURE DIVISION → endpoints)
│   │   │   ├── service/
│   │   │   │   └── IngresosService.java              # Lógica de negocio (párrafos COBOL)
│   │   │   ├── model/
│   │   │   │   ├── CuentaBancaria.java               # @Entity (DATA DIVISION nivel 01)
│   │   │   │   └── Ingreso.java                      # @Entity (DATA DIVISION nivel 05/10)
│   │   │   ├── dto/
│   │   │   │   ├── CuentaBancariaRequestDTO.java     # Entrada de cuenta
│   │   │   │   ├── CuentaBancariaResponseDTO.java    # Salida de cuenta (DISPLAY)
│   │   │   │   ├── IngresoPedidoDTO.java             # Entrada de ingreso (ACCEPT)
│   │   │   │   └── IngresoResponseDTO.java           # Salida de ingreso
│   │   │   ├── repository/
│   │   │   │   └── CuentaBancariaRepository.java     # JPA Repository (FILE-CONTROL)
│   │   │   └── exception/
│   │   │       ├── CuentaNoEncontradaException.java
│   │   │       ├── OperacionInvalidaException.java
│   │   │       └── GlobalExceptionHandler.java       # Manejo global de excepciones
│   │   └── resources/
│   │       ├── application.properties                 # Configuración (ENVIRONMENT DIVISION)
│   │       └── messages.properties                    # Mensajes de validación
│   └── test/
│       └── java/com/banco/
├── pom.xml                                             # Dependencias Maven
└── README.md                                           # Este archivo
```

---

## 🔄 Mapeo COBOL → Spring Boot

### 1. IDENTIFICATION DIVISION → Clase Principal

**COBOL:**
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. BANCO-INGRESOS.
AUTHOR. PROYECTO-MCPS.
```

**Spring Boot:**
```java
@SpringBootApplication
public class BancoIngresosApplication {
    public static void main(String[] args) {
        SpringApplication.run(BancoIngresosApplication.class, args);
    }
}
```

### 2. DATA DIVISION → Entidades y DTOs

#### Variables de Nivel 01 → Clases @Entity
| COBOL | Java | Descripción |
|-------|------|-----------|
| `WS-NUMERO-CUENTA PIC X(20)` | `String numeroCuenta` | Identificador único de la cuenta |
| `WS-TITULAR PIC X(40)` | `String titular` | Nombre del titular |
| `WS-TABLA-INGRESOS OCCURS 100` | `List<Ingreso> ingresos` | Tabla dinamica de ingresos |

#### Variables de Nivel 05/10 → Campos de @Entity
| COBOL | Java | Validación |
|-------|------|-----------|
| `WS-ING-IMPORTE PIC 9(8)V99` | `BigDecimal importe` | @Positive |
| `WS-ING-CONCEPTO PIC X(30)` | `String concepto` | @NotBlank, @Size(max=30) |

### 3. PROCEDURE DIVISION → Service + Controller

#### Párrafos COBOL → Métodos del Service

| Párrafo COBOL | Método Java | Tipo | Descripción |
|---------------|-------------|------|-----------|
| `0000-PRINCIPAL` | `procesarIngresos()` | Service | Orquestación principal |
| `1000-INICIALIZAR` | `inicializar()` | Service Private | Inicializa variables |
| `2000-PEDIR-DATOS-CUENTA` | `obtenerOCrearCuenta()` | Service Private | ACCEPT → @RequestParam |
| `3000-REGISTRAR-INGRESOS` | `registrarIngresos()` | Service Private | PERFORM UNTIL → for loop |
| `4000-CALCULAR-TOTAL` | `calcularTotal()` | Service Private | PERFORM VARYING → Stream |
| `5000-MOSTRAR-RESUMEN` | `mostrarResumen()` | Service Private | DISPLAY → Logger.info() |
| `9000-FINALIZAR` | `finalizar()` | Service Private | STOP RUN → return |

#### DISPLAY → Logger

**COBOL:**
```cobol
DISPLAY "=========================================="
DISPLAY "   SISTEMA DE INGRESOS BANCARIOS"
DISPLAY "   Fecha: " WS-FECHA-ACTUAL
DISPLAY "=========================================="
```

**Spring Boot:**
```java
log.info("==========================================");
log.info("   SISTEMA DE INGRESOS BANCARIOS");
log.info("   Fecha: {}", LocalDateTime.now());
log.info("==========================================");
```

#### ACCEPT → @RequestBody / @RequestParam

**COBOL:**
```cobol
ACCEPT WS-NUMERO-CUENTA
ACCEPT WS-TITULAR
```

**Spring Boot (Controller):**
```java
@PostMapping("/procesar")
public ResponseEntity<CuentaBancariaResponseDTO> procesarIngresos(
    @RequestParam String numeroCuenta,
    @RequestParam String titular,
    @Valid @RequestBody List<IngresoPedidoDTO> ingresos)
```

#### PERFORM UNTIL → for/while loops

**COBOL:**
```cobol
PERFORM UNTIL WS-CONTINUAR = 'N'
    ADD 1 TO WS-CONTADOR
    ACCEPT WS-IMPORTE-INGRESO
    ...
END-PERFORM
```

**Spring Boot:**
```java
for (IngresoPedidoDTO pedido : ingresosDTO) {
    // Procesar cada ingreso
    Ingreso ingreso = Ingreso.builder()
        .importe(pedido.getImporte())
        .concepto(pedido.getConcepto())
        .build();
    cuenta.agregarIngreso(ingreso);
}
```

#### PERFORM VARYING → Java Streams

**COBOL:**
```cobol
PERFORM VARYING WS-CONTADOR FROM 1 BY 1
    UNTIL WS-CONTADOR > WS-NUM-INGRESOS
    ADD WS-ING-IMPORTE(WS-CONTADOR) TO WS-SUMA-TOTAL
END-PERFORM
```

**Spring Boot:**
```java
BigDecimal total = cuenta.getIngresos().stream()
    .map(Ingreso::getImporte)
    .reduce(BigDecimal.ZERO, BigDecimal::add);
```

### 4. ENVIRONMENT DIVISION → Configuración

**COBOL (FILE-CONTROL, CONFIGURATION SECTION):**
```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
```

**Spring Boot (application.properties):**
```properties
spring.datasource.url=jdbc:h2:mem:bancoingresosdb
spring.jpa.hibernate.ddl-auto=create-drop
spring.jpa.database-platform=org.hibernate.dialect.H2Dialect
```

---

## 🛠️ Guía de Instalación y Ejecución

### Requisitos
- **Java 17+**
- **Maven 3.8+**

### 1. Compilación
```bash
cd spring-boot-banco-ingresos
mvn clean install
```

### 2. Ejecución
```bash
mvn spring-boot:run
```

La aplicación estará disponible en: `http://localhost:8080/banco-ingresos`

### 3. Consola H2
```
URL: http://localhost:8080/banco-ingresos/h2-console
Usuario: sa
Contraseña: (dejar en blanco)
JDBC URL: jdbc:h2:mem:bancoingresosdb
```

---

## 🌐 API REST - Endpoints

### 1. Procesar Ingresos (Flujo Principal)

**POST** `/api/ingresos/procesar`

**Parámetros de Query:**
- `numeroCuenta` (String, obligatorio): Número de la cuenta
- `titular` (String, obligatorio): Nombre del titular

**Body (JSON):**
```json
[
  {
    "importe": 1000.50,
    "concepto": "Salario",
    "descripcion": "Pago de nómina"
  },
  {
    "importe": 500.00,
    "concepto": "Bono",
    "descripcion": "Bono especial"
  }
]
```

**Respuesta (201 Created):**
```json
{
  "id": 1,
  "numeroCuenta": "ES9121000418450200051332",
  "titular": "Juan Pérez García",
  "saldo": 1500.50,
  "numeroIngresos": 2,
  "sumaTotalIngresos": 1500.50,
  "ingresos": [
    {
      "id": 1,
      "importe": 1000.50,
      "concepto": "Salario",
      "fechaIngreso": "2026-02-20T14:30:00",
      "descripcion": "Pago de nómina"
    },
    {
      "id": 2,
      "importe": 500.00,
      "concepto": "Bono",
      "fechaIngreso": "2026-02-20T14:30:00",
      "descripcion": "Bono especial"
    }
  ]
}
```

### 2. Obtener Cuenta por ID

**GET** `/api/ingresos/cuentas/{id}`

```bash
curl http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1
```

### 3. Obtener Cuenta por Número

**GET** `/api/ingresos/cuentas/numero/{numeroCuenta}`

```bash
curl http://localhost:8080/banco-ingresos/api/ingresos/cuentas/numero/ES9121000418450200051332
```

### 4. Agregar Ingreso a Cuenta Existente

**POST** `/api/ingresos/cuentas/{cuentaId}/agregar-ingreso`

**Body:**
```json
{
  "importe": 250.75,
  "concepto": "Transferencia",
  "descripcion": "Transferencia de amigo"
}
```

---

## ✅ Validaciones (Bean Validation - JSR-380)

Las validaciones se aplican automáticamente según las reglas COBOL:

### Validaciones de Cuenta
- `numeroCuenta`: @NotBlank, @Size(max=20)
- `titular`: @NotBlank, @Size(max=40)

### Validaciones de Ingreso
- `importe`: @Positive (debe ser > 0)
- `concepto`: @NotBlank, @Size(max=30)

### Manejo de Errores

**Error de validación (400 Bad Request):**
```json
{
  "timestamp": "2026-02-20T14:30:00",
  "estado": 400,
  "error": "Operación inválida",
  "mensaje": "El importe debe ser positivo"
}
```

**Cuenta no encontrada (404 Not Found):**
```json
{
  "timestamp": "2026-02-20T14:30:00",
  "estado": 404,
  "error": "Cuenta no encontrada",
  "mensaje": "Cuenta no encontrada con ID: 999"
}
```

---

## 🎯 Reglas de Conversión Aplicadas

### 1. Tipos de Datos (SEPARADOR-REGLAS-CONVERSION)

| COBOL | Java | Anotación JPA |
|-------|------|-----------|
| `PIC X(n)` | `String` | `@Column(length=n)` |
| `PIC 9(n)` | `Integer` / `Long` | - |
| `PIC 9(n)V99` | `BigDecimal` | `@Column(precision=n+2, scale=2)` |
| `PIC S9(n)V99` | `BigDecimal` | (soporta negativos) |
| `OCCURS n` | `List<T>` | `@OneToMany` |

### 2. Patrones Bancarios (SEPARADOR-PATRONES-BANCARIOS)

✅ Operaciones monetarias con `BigDecimal` (nunca `double`/`float`)
✅ Validaciones de datos → Bean Validation decorators
✅ Excepciones custom → `CuentaNoEncontradaException`, `OperacionInvalidaException`
✅ Transaccionalidad → `@Transactional` en métodos que modifican datos

### 3. Validaciones (SEPARADOR-VALIDACIONES)

✅ Campos vacíos → `@NotBlank`
✅ Valores positivos → `@Positive`
✅ Tamaño máximo → `@Size(max=n)`
✅ Validaciones custom → Interface `@Constraint` si es necesario

---

## 📖 Referencia de Memory (Knowledge Graph)

Esta traducción sigue las reglas definidas en el archivo `memory.jsonl`:

```
COBOL-SpringBoot-Traduccion
├── BLOQUE-IDENTIFICATION-DIVISION → BancoIngresosApplication
├── BLOQUE-DATA-DIVISION → CuentaBancaria, Ingreso, DTOs
├── BLOQUE-PROCEDURE-DIVISION → IngresosService, IngresosController
├── BLOQUE-ENVIRONMENT-DIVISION → application.properties
├── SEPARADOR-PATRONES-BANCARIOS → BigDecimal, validaciones, @Transactional
├── SEPARADOR-ESTRUCTURA-SPRINGBOOT → estructura de carpetas y paquetes
├── SEPARADOR-REGLAS-CONVERSION → mapeo de tipos PIC COBOL → Java
└── SEPARADOR-VALIDACIONES → Bean Validation JSR-380
```

---

## 🧪 Ejemplos de Uso

### Ejemplo 1: Procesar ingresos en una nueva cuenta

```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=ES9121000418450200051332&titular=Juan%20Pérez" \
  -H "Content-Type: application/json" \
  -d '[
    {"importe": 1000.00, "concepto": "Salario"},
    {"importe": 500.00, "concepto": "Bono"}
  ]'
```

### Ejemplo 2: Obtener cuenta completa

```bash
curl "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/numero/ES9121000418450200051332"
```

### Ejemplo 3: Agregar nuevo ingreso

```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1/agregar-ingreso" \
  -H "Content-Type: application/json" \
  -d '{"importe": 250.75, "concepto": "Transferencia", "descripcion": "De amigo"}'
```

---

## 📝 Notas de Traducción

### Diferencias Principales entre COBOL y Spring Boot

| Aspecto | COBOL | Spring Boot |
|--------|-------|-----------|
| Entrada de datos | `ACCEPT` (interactivo) | REST API (`@RequestBody`, `@RequestParam`) |
| Salida de datos | `DISPLAY` (console) | `Logger.info()` + JSON response |
| Base de datos | FILE-CONTROL, ASSIGN | JPA Repository, `@Entity` |
| Validación | IF statements manuales | Anotaciones Bean Validation |
| Persistencia manual | WRITE, REWRITE | JPA automático con `@Transactional` |
| Bucles | PERFORM UNTIL/VARYING | for/while loops, Streams |

### Ventajas de la Traducción

✅ Código más modular y testeable
✅ Validación automática integrada
✅ API REST escalable
✅ Persistencia robusta con JPA
✅ Logging centralizado
✅ Mejor manejo de errores
✅ Fácil de integrar con otros servicios

---

## 📞 Soporte

Para consultas sobre la traducción COBOL → Spring Boot, revisar:
- 📄 [memory.jsonl](../data/memory.jsonl) - Knowledge Graph con reglas de traducción
- 📄 [PROCESO_UNION_ARBOLES_MEMORY.md](../docs/PROCESO_UNION_ARBOLES_MEMORY.md) - Proceso de integración

---

## 📅 Historial de Cambios

**v1.0.0 (2026-02-20)**
- ✅ Traducción completa del programa COBOL BANCO-INGRESOS
- ✅ Implementación de API REST con 4 endpoints
- ✅ Validaciones con Bean Validation (JSR-380)
- ✅ Manejo de excepciones custom
- ✅ Base de datos H2 in-memory
- ✅ Logging con SLF4J
- ✅ Documentación completa

---

**Traducido por**: GitHub Copilot | **Fecha**: 20 de febrero de 2026
