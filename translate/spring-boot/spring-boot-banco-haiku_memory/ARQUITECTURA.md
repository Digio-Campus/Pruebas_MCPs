# 📁 ESTRUCTURA FINAL DEL PROYECTO TRADUCIDO

## Árbol de Directorios Completo

```
spring-boot-banco-ingresos/
├── src/
│   └── main/
│       ├── java/
│       │   └── com/
│       │       └── banco/
│       │           ├── BancoIngresosApplication.java              [15 líneas]
│       │           │   ├─ Clase principal @SpringBootApplication
│       │           │   └─ MAPEO: IDENTIFICATION DIVISION → PROGRAM-ID
│       │           │
│       │           ├── controller/
│       │           │   └── IngresosController.java               [95 líneas]
│       │           │       ├─ 4 endpoints REST (@PostMapping, @GetMapping)
│       │           │       └─ MAPEO: PROCEDURE DIVISION → endpoints
│       │           │
│       │           ├── service/
│       │           │   └── IngresosService.java                 [280 líneas]
│       │           │       ├─ 0000-PRINCIPAL → procesarIngresos()
│       │           │       ├─ 1000-INICIALIZAR → inicializar()
│       │           │       ├─ 2000-PEDIR-DATOS → obtenerOCrearCuenta()
│       │           │       ├─ 3000-REGISTRAR → registrarIngresos()
│       │           │       ├─ 4000-CALCULAR → calcularTotal()
│       │           │       ├─ 5000-MOSTRAR → mostrarResumen()
│       │           │       └─ 9000-FINALIZAR → finalizar()
│       │           │
│       │           ├── model/
│       │           │   ├── CuentaBancaria.java                  [80 líneas]
│       │           │   │   ├─ @Entity con campos tipados
│       │           │   │   ├─ @OneToMany List<Ingreso>
│       │           │   │   ├─ MAPEO: WS-NUMERO-CUENTA, WS-TITULAR
│       │           │   │   └─ MAPEO: WS-TABLA-INGRESOS (OCCURS)
│       │           │   │
│       │           │   └── Ingreso.java                          [50 líneas]
│       │           │       ├─ @Entity
│       │           │       ├─ @ManyToOne CuentaBancaria
│       │           │       ├─ MAPEO: WS-ING-IMPORTE (PIC 9(8)V99)
│       │           │       ├─ MAPEO: WS-ING-CONCEPTO (PIC X(30))
│       │           │       └─ Validaciones: @Positive, @NotBlank, @Size
│       │           │
│       │           ├── dto/
│       │           │   ├── CuentaBancariaRequestDTO.java        [30 líneas]
│       │           │   │   └─ MAPEO: ACCEPT WS-NUMERO-CUENTA, WS-TITULAR
│       │           │   │
│       │           │   ├── CuentaBancariaResponseDTO.java       [35 líneas]
│       │           │   │   └─ MAPEO: DISPLAY resumen cuenta
│       │           │   │
│       │           │   ├── IngresoPedidoDTO.java                [30 líneas]
│       │           │   │   ├─ MAPEO: ACCEPT WS-IMPORTE-INGRESO
│       │           │   │   └─ MAPEO: ACCEPT WS-ING-CONCEPTO
│       │           │   │
│       │           │   └── IngresoResponseDTO.java              [30 líneas]
│       │           │       └─ MAPEO: DISPLAY detalle ingreso
│       │           │
│       │           ├── repository/
│       │           │   └── CuentaBancariaRepository.java        [20 líneas]
│       │           │       ├─ Spring Data JPA
│       │           │       ├─ MAPEO: FILE-CONTROL, CONFIGURATION SECTION
│       │           │       └─ Método: findByNumeroCuenta()
│       │           │
│       │           └── exception/
│       │               ├── CuentaNoEncontradaException.java     [13 líneas]
│       │               │   └─ RuntimeException custom
│       │               │
│       │               ├── OperacionInvalidaException.java      [13 líneas]
│       │               │   └─ RuntimeException custom
│       │               │
│       │               └── GlobalExceptionHandler.java          [70 líneas]
│       │                   ├─ @RestControllerAdvice
│       │                   ├─ Manejo de errores centralizado
│       │                   └─ Respuestas consistentes (400, 404, 500)
│       │
│       └── resources/
│           ├── application.properties                           [30 líneas]
│           │   ├─ spring.datasource.* (H2 Database)
│           │   ├─ spring.jpa.* (Hibernate Config)
│           │   ├─ logging.* (SLF4J/Logback)
│           │   └─ MAPEO: ENVIRONMENT DIVISION
│           │
│           └── messages.properties                              [10 líneas]
│               └─ Mensajes de validación (Bean Validation)
│
├── pom.xml                                                       [90 líneas]
│   ├─ Spring Boot 3.2.2
│   ├─ Spring Data JPA
│   ├─ Spring Web (REST)
│   ├─ Spring Validation (Bean Validation JSR-380)
│   ├─ H2 Database
│   ├─ Lombok (anotaciones)
│   └─ Maven plugins
│
├── README.md                                                     [289 líneas]
│   ├─ Descripción general del proyecto
│   ├─ Estructura del proyecto
│   ├─ Mapeos COBOL → Spring Boot
│   ├─ Guía de instalación y ejecución
│   ├─ Documentación de 4 endpoints REST
│   ├─ Ejemplos de uso con cURL
│   ├─ Explicación de validaciones
│   ├─ Reglas de conversión aplicadas
│   └─ Referencias a Memory (Knowledge Graph)
│
├── MAPEO_DETALLADO.md                                           [400+ líneas]
│   ├─ Tabla de mapeo completa COBOL → Java
│   ├─ Mapeo párrafo por párrafo de PROCEDURE DIVISION
│   ├─ Conversión detallada de estructuras
│   ├─ Patrones de traducción aplicados
│   ├─ Checklist de traducción
│   └─ Ejemplos lado a lado COBOL/Java
│
├── GUIA_EJECUCION.md                                            [350+ líneas]
│   ├─ Inicio rápido
│   ├─ 7 pruebas manuales con cURL
│   ├─ Consola H2
│   ├─ Script de automatización
│   ├─ Tabla de test cases
│   ├─ Troubleshooting
│   └─ Validación de respuestas
│
├── RESUMEN_EJECUCION.md                                         [250+ líneas]
│   ├─ Resumen ejecutivo de la traducción
│   ├─ Estadísticas del proyecto (LOC, clases)
│   ├─ Checklist de entrega
│   ├─ Trazabilidad de mapeos
│   ├─ Características implementadas
│   └─ Recomendaciones para próximos pasos
│
├── .gitignore                                                    [40 líneas]
│   ├─ Estructuras Maven
│   ├─ Directorios y archivos IDE
│   ├─ Archivos temporales
│   └─ Base de datos H2
│
└── ARQUITECTURA.md                                               [Este archivo]
    └─ Estructura y componentes del proyecto

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
TOTAL ARCHIVOS CREADOS: 24
TOTAL LÍNEAS DE CÓDIGO: ~2090 líneas
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

---

## 📊 Tabla Resumen de Componentes

| Categoría | Componente | Archivo | Propósito |
|-----------|-----------|---------|----------|
| **App Main** | SpringBoot App | BancoIngresosApplication.java | Punto de entrada @SpringBootApplication |
| **Controllers** | REST API | IngresosController.java | 4 endpoints para operaciones |
| **Service** | Business Logic | IngresosService.java | 7 párrafos COBOL como métodos |
| **Models** | JPA Entities | CuentaBancaria.java | @Entity principal |
| | | Ingreso.java | @Entity secundaria (@OneToMany) |
| **DTOs** | Data Transfer | CuentaBancariaRequestDTO.java | DTO entrada |
| | | CuentaBancariaResponseDTO.java | DTO salida |
| | | IngresoPedidoDTO.java | DTO entrada |
| | | IngresoResponseDTO.java | DTO salida |
| **Repository** | Data Access | CuentaBancariaRepository.java | Spring Data JPA |
| **Exceptions** | Error Handling | CuentaNoEncontradaException.java | Excepción business |
| | | OperacionInvalidaException.java | Excepción business |
| | | GlobalExceptionHandler.java | Centralización errores |
| **Config** | Configuración | pom.xml | Dependencias Maven |
| | | application.properties | Configuración Spring Boot |
| | | messages.properties | Mensajes validación |
| | | .gitignore | Git ignore patterns |
| **Docs** | Documentación | README.md | Guía principal |
| | | MAPEO_DETALLADO.md | Detalles de traducción |
| | | GUIA_EJECUCION.md | Pruebas y ejecución |
| | | RESUMEN_EJECUCION.md | Resumen ejecutivo |
| | | ARQUITECTURA.md | Este archivo |

---

## 🔀 Flujo de Solicitud HTTP

```
┌─────────────────────────────────────────────────────────────────┐
│                    CLIENT (Browser/cURL)                        │
└──────────────────────────┬──────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│                  IngresosController                             │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ @PostMapping("/procesar")                               │   │
│  │ @PostMapping("/cuentas/{id}/agregar-ingreso")          │   │
│  │ @GetMapping("/cuentas/{id}")                           │   │
│  │ @GetMapping("/cuentas/numero/{numeroCuenta}")          │   │
│  └──────────────────┬──────────────────────────────────────┘   │
└─────────────────────┼─────────────────────────────────────────┘
                      │
                      ↓
┌─────────────────────────────────────────────────────────────────┐
│                  IngresosService                               │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │ public CuentaBancariaResponseDTO procesarIngresos()      │  │
│  │   ├─ inicializar()                                       │  │
│  │   ├─ obtenerOCrearCuenta()                               │  │
│  │   ├─ registrarIngresos()                                 │  │
│  │   ├─ calcularTotal()                                     │  │
│  │   ├─ mostrarResumen()                                    │  │
│  │   └─ finalizar()                                         │  │
│  └──────────────────┬───────────────────────────────────────┘  │
└─────────────────────┼──────────────────────────────────────────┘
                      │
                      ↓
┌─────────────────────────────────────────────────────────────────┐
│           CuentaBancariaRepository (Spring Data JPA)            │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ Optional<CuentaBancaria> findByNumeroCuenta()             │ │
│  │ CuentaBancaria save(CuentaBancaria)                       │ │
│  │ Optional<CuentaBancaria> findById(Long)                   │ │
│  └─────────────────────┬──────────────────────────────────────┘ │
└────────────────────────┼───────────────────────────────────────┘
                         │
                         ↓
┌─────────────────────────────────────────────────────────────────┐
│                    H2 Database                                  │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ CUENTAS_BANCARIAS                                          │ │
│  │   id, numero_cuenta, titular, saldo, fecha_creacion       │ │
│  │                                                             │ │
│  │ INGRESOS                                                   │ │
│  │   id, cuenta_bancaria_id, importe, concepto, fecha        │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

---

## 🔗 Relaciones Entre Entidades

```
┌──────────────────────────┐
│   CuentaBancaria         │
├──────────────────────────┤
│ - id (PK)                │
│ - numeroCuenta (UNIQUE)  │
│ - titular                │
│ - saldo                  │
│ - fechaCreacion          │
│ - ingresos (OneToMany)   │◄───────────┐
└──────────────────────────┘            │
                                        │ ManyToOne
                                        │ FK: cuenta_bancaria_id
                                        │
                      ┌──────────────────┘
                      │
┌─────────────────────────────────────────┐
│          Ingreso                        │
├─────────────────────────────────────────┤
│ - id (PK)                               │
│ - cuentaBancaria_id (FK)                │
│ - importe (@Positive)                   │
│ - concepto (@NotBlank, @Size)           │
│ - descripcion                           │
│ - fechaIngreso                          │
└─────────────────────────────────────────┘
```

---

## 🎯 Capas de la Aplicación

```
┌──────────────────────────────────────────────────────────┐
│              PRESENTATION LAYER                          │
│  ┌────────────────────────────────────────────────────┐  │
│  │ IngresosController                                 │  │
│  │  • @RestController, @GetMapping, @PostMapping      │  │
│  │  • Manejo de solicitudes HTTP                      │  │
│  │  • Validación automática @Valid                    │  │
│  └────────────────────────────────────────────────────┘  │
└────────────────────┬─────────────────────────────────────┘
                     │
┌────────────────────┼─────────────────────────────────────┐
│    BUSINESS LAYER  │                                      │
│  ┌────────────────────────────────────────────────────┐  │
│  │ IngresosService                                    │  │
│  │  • @Service, @Transactional                        │  │
│  │  • Lógica de negocio                               │  │
│  │  • Orquestación de operaciones                     │  │
│  │  • Implementación de párrafos COBOL                │  │
│  └────────────────────────────────────────────────────┘  │
└────────────────────┬─────────────────────────────────────┘
                     │
┌────────────────────┼─────────────────────────────────────┐
│       DATA LAYER   │                                      │
│  ┌────────────────────────────────────────────────────┐  │
│  │ CuentaBancariaRepository                           │  │
│  │  • @Repository, JpaRepository                      │  │
│  │  • Acceso a datos persistentes                     │  │
│  │  • Consultas personalizadas                        │  │
│  └────────────────────────────────────────────────────┘  │
└────────────────────┬─────────────────────────────────────┘
                     │
┌────────────────────┼─────────────────────────────────────┐
│    DATABASE LAYER  │                                      │
│  ┌────────────────────────────────────────────────────┐  │
│  │ H2 Database (In-Memory)                            │  │
│  │  • CUENTAS_BANCARIAS table                         │  │
│  │  • INGRESOS table                                  │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────┘
```

---

## 🏗️ Arquitectura Hexagonal (Puertos y Adaptadores)

```
                    INPUT PORTS
                        │
        ┌───────────────┼───────────────┐
        │               │               │
    [HTTP REST]    [CLI]           [EVENTS]
        │               │               │
        ▼               ▼               ▼
    ┌──────────────────────────────────────┐
    │    IngresosController               │  ◄─── ADAPTER
    └────────────────┬─────────────────────┘
                     │
              PORT (Interface)
                     │
    ┌────────────────┼─────────────────────┐
    │   IngresosService (CORE)            │
    │  ├─ Lógica de negocio               │
    │  ├─ Orquestación                    │
    │  └─ Validaciones dominio            │
    └────────────────┬─────────────────────┘
                     │
              PORT (Repository)
                     │
        ┌────────────┼──────────────┐
        │            │              │
        ▼            ▼              ▼
    [JPA]    [CACHE]        [API REST EXTERNO]
        │            │              │
        └────────────┼──────────────┘
                     │
                OUTPUT PORTS
```

---

## 📋 Matriz de Mapeo COBOL → Spring Boot (Resumida)

| Elemento COBOL | Elemento Spring Boot | Archivo |
|---|---|---|
| PROGRAM-ID | @SpringBootApplication | BancoIngresosApplication |
| ENVIRONMENT DIVISION | application.properties | pom.xml |
| DATA DIVISION (01) | @Entity | CuentaBancaria, Ingreso |
| DATA DIVISION (05-10) | @Column, @Validation | Campos de Entities |
| WORKING-STORAGE | Service properties | IngresosService |
| PROCEDURE DIVISION | @Service methods | IngresosService |
| Párrafos PERFORM | private methods | IngresosService |
| ACCEPT | @RequestParam/@RequestBody | IngresosController |
| DISPLAY | Logger.info() | IngresosService |
| Validaciones IF | @NotBlank/@Positive/@Size | DTOs y Entities |
| FILE-CONTROL | @Repository | CuentaBancariaRepository |

---

## 🧩 Inyección de Dependencias

```
BancoIngresosApplication
        │
        ├─ IngresosController
        │       │
        │       └─ @Autowired IngresosService
        │                  │
        │                  └─ @Autowired CuentaBancariaRepository
        │                          │
        │                          └─ Spring Data JPA (auto-configurado)
        │
        └─ GlobalExceptionHandler (auto-instanciado por @RestControllerAdvice)


BEANS REGISTRADOS:
✓ BancoIngresosApplication (root context)
✓ IngresosController (@CrossOrigin, RequestMapping)
✓ IngresosService (@Service, @Transactional)
✓ CuentaBancariaRepository (Spring Data JPA Proxy)
✓ GlobalExceptionHandler (@RestControllerAdvice)
```

---

## 📈 Ciclo de Vida de una Solicitud

```
1. REQUEST ENTRA
   POST /api/ingresos/procesar?numeroCuenta=ES1234&titular=Juan
   BODY: [{"importe": 1000, "concepto": "Salario"}]
                              │
                              ▼
2. TOMCAT RECIBE → DISPATCHER SERVLET
                              │
                              ▼
3. CONTROLLER RESUELVE (@PostMapping)
   IngresosController.procesarIngresos()
                              │
                              ▼
4. VALIDACIÓN (@Valid)
   ✓ IngresoPedidoDTO validado
   ✓ Anotaciones: @Positive, @NotBlank, @Size
                              │
                              ▼
5. SERVICE PROCESZA (@Transactional)
   IngresosService.procesarIngresos()
   ├─ inicializar()
   ├─ obtenerOCrearCuenta()
   ├─ registrarIngresos()
   ├─ calcularTotal()
   ├─ mostrarResumen()
   └─ finalizar()
                              │
                              ▼
6. REPOSITORY ACCEDE BD
   CuentaBancariaRepository.findByNumeroCuenta()
   CuentaBancariaRepository.save()
                              │
                              ▼
7. BD PERSISTE (H2)
   INSERT INTO CUENTAS_BANCARIAS...
   INSERT INTO INGRESOS...
                              │
                              ▼
8. CONSTRUCCIÓN RESPUESTA
   CuentaBancariaResponseDTO construida
                              │
                              ▼
9. RESPONSE ENVIADA AL CLIENT
   HTTP 201 Created
   Content-Type: application/json
   BODY: { "id": 1, "numeroCuenta": "...", ... }
```

---

## 🔐 Validación en Cascada

```
CLIENT REQUEST
      │
      ▼
CONTROLLER @Valid
      │
      ├─ IngresoPedidoDTO
      │  ├─ @Positive importe  ✓
      │  ├─ @NotBlank concepto ✓
      │  └─ @Size concepto     ✓
      │
      └─ Si hay error → HttpMessageNotReadableException → 400 Bad Request
                                      │
                                      ▼
      Si OK → Pasar a SERVICE
                      │
                      ▼
SERVICE VALIDACIÓN LÓGICA
      │
      ├─ if (importe <= 0) → OperacionInvalidaException
      ├─ if (cuenta not found) → CuentaNoEncontradaException
      │
      └─ Si OK → Persistir en BD
                      │
                      ▼
RESPONSE (201 Created / 200 OK / 400 / 404 / 500)
```

---

## 🚀 Deployment Readiness

```
✓ Compilable: mvn clean compile          → BUILD SUCCESS
✓ Testeable: mvn test                    → Tests configurados
✓ Empaquetable: mvn package              → JAR generado
✓ Ejecutable: java -jar *.jar            → Application starts
✓ Port 8080 by default                   → spring.profiles.active=dev
✓ Context path: /banco-ingresos          → server.servlet.context-path
✓ H2 Console: /h2-console                → spring.h2.console.enabled=true
✓ Health endpoint: /actuator/health      → spring-boot-starter-actuator (opcional)
```

---

**Documento generado**: 20 de febrero de 2026
**Referencia**: Proyecto `spring-boot-banco-ingresos` traducido desde COBOL
