# MAPEO DETALLADO: BANCO-INGRESOS COBOL → Spring Boot

## 📊 Tabla de Mapeo Completa

### IDENTIFICATION DIVISION / IDENTIFICATION BLOCK

```
COBOL PROGRAM-ID           Java Class                  Tipo
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
BANCO-INGRESOS      →      BancoIngresosApplication   @SpringBootApplication
```

**Archivo**: `BancoIngresosApplication.java`

---

### DATA DIVISION / WORKING-STORAGE SECTION

#### Tabla de Mapeo de Variables

```
COBOL Variable                PIC Type        Java Equivalent              Anotación JPA
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
WS-NUMERO-CUENTA              X(20)           String numeroCuenta          @Column(length=20)
WS-TITULAR                    X(40)           String titular               @Column(length=40)
WS-NUM-INGRESOS               9(3)            Integer numeroIngresos       -
WS-CONTADOR                   9(3)            int contador                 (variable local)
WS-IMPORTE-INGRESO            9(8)V99         BigDecimal importe           @Column(precision=10, scale=2)
WS-SUMA-TOTAL                 9(10)V99        BigDecimal sumaTotalIngresos @Column(precision=12, scale=2)
WS-CONTINUAR                  X               char continuar              (variable local)
WS-FECHA-ACTUAL               X(10)           LocalDateTime fechaActual   (variable local)
WS-INGRESO-ENTRY              -               List<Ingreso> ingresos      @OneToMany(mappedBy="cuenta")
  └─ WS-ING-IMPORTE          9(8)V99         BigDecimal importe          @Column(precision=10, scale=2)
  └─ WS-ING-CONCEPTO         X(30)           String concepto             @Column(length=30)
```

#### Conversión Nivel por Nivel

**Nivel 01** (Registros principales → @Entity):
- `01 WS-NUMERO-CUENTA PIC X(20)` → `@Entity CuentaBancaria` con `@Column String numeroCuenta`
- `01 WS-TITULAR PIC X(40)` → Campo `String titular` en `CuentaBancaria`
- `01 WS-TABLA-INGRESOS (OCCURS 100)` → `@OneToMany List<Ingreso> ingresos`

**Nivel 05-10** (Subniveles → Campos de @Entity):
- `05 WS-ING-IMPORTE PIC 9(8)V99` → `@Entity Ingreso` con `BigDecimal importe`
- `10 WS-ING-CONCEPTO PIC X(30)` → Campo `String concepto` en `Ingreso`

---

### PROCEDURE DIVISION / Párrafos COBOL

#### Párrafo Principal: 0000-PRINCIPAL

```cobol
0000-PRINCIPAL.
    PERFORM 1000-INICIALIZAR
    PERFORM 2000-PEDIR-DATOS-CUENTA
    PERFORM 3000-REGISTRAR-INGRESOS
    PERFORM 4000-CALCULAR-TOTAL
    PERFORM 5000-MOSTRAR-RESUMEN
    PERFORM 9000-FINALIZAR
    STOP RUN.
```

↓ **Mapeo a Spring Boot** ↓

```java
// IngresosController.java
@PostMapping("/procesar")
public ResponseEntity<CuentaBancariaResponseDTO> procesarIngresos(
    @RequestParam String numeroCuenta,
    @RequestParam String titular,
    @Valid @RequestBody List<IngresoPedidoDTO> ingresos) {
    // Llamadas a métodos del Service que implementan cada párrafo
    return ResponseEntity.status(HttpStatus.CREATED)
        .body(ingresosService.procesarIngresos(numeroCuenta, titular, ingresos));
}

// IngresosService.java
public CuentaBancariaResponseDTO procesarIngresos(
        String numeroCuenta,
        String titular,
        List<IngresoPedidoDTO> ingresosDTO) {
    
    inicializar(numeroCuenta, titular);                    // 1000-INICIALIZAR
    CuentaBancaria cuenta = obtenerOCrearCuenta(...);     // 2000-PEDIR-DATOS-CUENTA
    registrarIngresos(cuenta, ingresosDTO);               // 3000-REGISTRAR-INGRESOS
    BigDecimal total = calcularTotal(cuenta);             // 4000-CALCULAR-TOTAL
    mostrarResumen(cuenta);                               // 5000-MOSTRAR-RESUMEN
    finalizar();                                           // 9000-FINALIZAR
    return construirResponse(cuenta);
}
```

---

#### Párrafo 1000: Inicializar

**COBOL:**
```cobol
1000-INICIALIZAR.
    INITIALIZE WS-NUMERO-CUENTA
    INITIALIZE WS-TITULAR
    MOVE 0 TO WS-NUM-INGRESOS
    MOVE 0 TO WS-SUMA-TOTAL
    MOVE 0 TO WS-CONTADOR
    ACCEPT WS-FECHA-ACTUAL FROM DATE YYYYMMDD
    DISPLAY "=========================================="
    DISPLAY "   SISTEMA DE INGRESOS BANCARIOS"
    DISPLAY "   Fecha: " WS-FECHA-ACTUAL
    DISPLAY "==========================================".
```

**Spring Boot:**
```java
private void inicializar(String numeroCuenta, String titular) {
    log.info("==========================================");
    log.info("   SISTEMA DE INGRESOS BANCARIOS");
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    log.info("   Fecha: {}", LocalDateTime.now().format(formatter));
    log.info("==========================================");
}
```

**Mapeos de instrucciones:**
| COBOL | Spring Boot | Equivalencia |
|-------|------------|-----------|
| `INITIALIZE` | No needed | Objetos java inicializados por defecto |
| `MOVE 0 TO` | Variable = 0 | Asignación directa |
| `ACCEPT...FROM DATE` | `LocalDateTime.now()` | API de fecha moderna |
| `DISPLAY` | `log.info()` | Logging con SLF4J |

---

#### Párrafo 2000: Pedir Datos de Cuenta

**COBOL:**
```cobol
2000-PEDIR-DATOS-CUENTA.
    DISPLAY " "
    DISPLAY "Introduzca el numero de cuenta: "
    ACCEPT WS-NUMERO-CUENTA
    DISPLAY "Introduzca el nombre del titular: "
    ACCEPT WS-TITULAR.
```

**Spring Boot - Request Handler:**
```java
// Los datos llegan a través de @RequestParam en el Controller
@PostMapping("/procesar")
public ResponseEntity<CuentaBancariaResponseDTO> procesarIngresos(
    @RequestParam String numeroCuenta,        // → ACCEPT WS-NUMERO-CUENTA
    @RequestParam String titular,             // → ACCEPT WS-TITULAR
    @Valid @RequestBody List<IngresoPedidoDTO> ingresos) { ... }

// En el Service:
private CuentaBancaria obtenerOCrearCuenta(String numeroCuenta, String titular) {
    Optional<CuentaBancaria> cuentaExistente = 
        cuentaBancariaRepository.findByNumeroCuenta(numeroCuenta);
    
    if (cuentaExistente.isPresent()) {
        return cuentaExistente.get();
    }
    
    CuentaBancaria nuevaCuenta = CuentaBancaria.builder()
        .numeroCuenta(numeroCuenta)
        .titular(titular)
        .saldo(BigDecimal.ZERO)
        .fechaCreacion(LocalDateTime.now().toString())
        .build();
    
    return cuentaBancariaRepository.save(nuevaCuenta);
}
```

---

#### Párrafo 3000: Registrar Ingresos

**COBOL:**
```cobol
3000-REGISTRAR-INGRESOS.
    MOVE 'S' TO WS-CONTINUAR
    PERFORM UNTIL WS-CONTINUAR = 'N'              ← PERFORM UNTIL
       ADD 1 TO WS-CONTADOR
       DISPLAY " "
       DISPLAY "--- Ingreso #" WS-CONTADOR " ---"
       DISPLAY "Importe del ingreso: "
       ACCEPT WS-IMPORTE-INGRESO                 ← ACCEPT
       DISPLAY "Concepto del ingreso: "
       ACCEPT WS-ING-CONCEPTO(WS-CONTADOR)       ← ACCEPT array
       MOVE WS-IMPORTE-INGRESO
          TO WS-ING-IMPORTE(WS-CONTADOR)
       ADD 1 TO WS-NUM-INGRESOS
       DISPLAY "Desea registrar otro ingreso? (S/N): "
       ACCEPT WS-CONTINUAR                       ← ACCEPT booleano implícito
    END-PERFORM.
```

**Spring Boot:**
```java
private void registrarIngresos(CuentaBancaria cuenta, List<IngresoPedidoDTO> ingresosDTO) {
    int contador = 1;
    
    // PERFORM UNTIL → for loop
    for (IngresoPedidoDTO pedido : ingresosDTO) {
        log.info("--- Ingreso #{} ---", contador);
        log.info("Importe del ingreso: {}", pedido.getImporte());
        log.info("Concepto del ingreso: {}", pedido.getConcepto());
        
        // Validación: importe positivo
        if (pedido.getImporte() == null || 
            pedido.getImporte().compareTo(BigDecimal.ZERO) <= 0) {
            throw new OperacionInvalidaException(
                "El importe debe ser un valor positivo");
        }
        
        // CREATE new Ingreso (como MOVE...TO)
        Ingreso ingreso = Ingreso.builder()
            .importe(pedido.getImporte())        // WS-ING-IMPORTE
            .concepto(pedido.getConcepto())      // WS-ING-CONCEPTO
            .descripcion(pedido.getDescripcion())
            .fechaIngreso(LocalDateTime.now())
            .build();
        
        // ADD 1 TO WS-NUM-INGRESOS (via agregación)
        cuenta.agregarIngreso(ingreso);
        contador++;
    }
    
    // Persistence
    cuentaBancariaRepository.save(cuenta);
}
```

**Mapeos:**
| COBOL | Spring Boot | Concepto |
|-------|------------|----------|
| `PERFORM UNTIL` | `for` loop | Iteración con condición |
| `ADD 1 TO WS-CONTADOR` | `contador++` | Incremento |
| `ACCEPT WS-IMPORTE` | `@RequestBody IngresoPedidoDTO` | Entrada de datos |
| `MOVE X TO Y` | `Ingreso.builder().importe(...).build()` | Asignación con creación |
| `IF validacion` | `@Positive`, `@NotBlank` | Validación declarativa |

---

#### Párrafo 4000: Calcular Total

**COBOL:**
```cobol
4000-CALCULAR-TOTAL.
    MOVE 0 TO WS-SUMA-TOTAL
    PERFORM VARYING WS-CONTADOR FROM 1 BY 1              ← PERFORM VARYING
       UNTIL WS-CONTADOR > WS-NUM-INGRESOS
       ADD WS-ING-IMPORTE(WS-CONTADOR)
          TO WS-SUMA-TOTAL
    END-PERFORM.
```

**Spring Boot:**
```java
private BigDecimal calcularTotal(CuentaBancaria cuenta) {
    // PERFORM VARYING → Java Streams (más funcional)
    BigDecimal total = cuenta.getIngresos().stream()      // PERFORM VARYING
        .map(Ingreso::getImporte)                         // WS-ING-IMPORTE
        .reduce(BigDecimal.ZERO, BigDecimal::add);        // ADD...TO
    
    log.info("Total de ingresos calculado: {}", total);
    return total;
}
```

**Alternativa con método auxiliar en @Entity:**
```java
// En CuentaBancaria.java
public BigDecimal calcularTotalIngresos() {
    return this.ingresos.stream()
        .map(Ingreso::getImporte)
        .reduce(BigDecimal.ZERO, BigDecimal::add);
}
```

---

#### Párrafo 5000: Mostrar Resumen

**COBOL:**
```cobol
5000-MOSTRAR-RESUMEN.
    DISPLAY " "
    DISPLAY "=========================================="
    DISPLAY "   RESUMEN DE INGRESOS"
    DISPLAY "=========================================="
    DISPLAY "Cuenta:  " WS-NUMERO-CUENTA
    DISPLAY "Titular: " WS-TITULAR
    DISPLAY "------------------------------------------"
    PERFORM VARYING WS-CONTADOR FROM 1 BY 1
       UNTIL WS-CONTADOR > WS-NUM-INGRESOS
       DISPLAY "  Ingreso #" WS-CONTADOR
          ": " WS-ING-IMPORTE(WS-CONTADOR)
          " - " WS-ING-CONCEPTO(WS-CONTADOR)
    END-PERFORM
    DISPLAY "------------------------------------------"
    DISPLAY "Numero de ingresos: " WS-NUM-INGRESOS
    DISPLAY "SUMA TOTAL:         " WS-SUMA-TOTAL
    DISPLAY "==========================================".
```

**Spring Boot:**
```java
private void mostrarResumen(CuentaBancaria cuenta) {
    log.info(" ");
    log.info("==========================================");
    log.info("   RESUMEN DE INGRESOS");
    log.info("==========================================");
    log.info("Cuenta:  {}", cuenta.getNumeroCuenta());
    log.info("Titular: {}", cuenta.getTitular());
    log.info("------------------------------------------");
    
    int contador = 1;
    for (Ingreso ingreso : cuenta.getIngresos()) {
        log.info("  Ingreso #{}: {} - {}",
            contador,
            ingreso.getImporte(),
            ingreso.getConcepto());
        contador++;
    }
    
    log.info("------------------------------------------");
    log.info("Numero de ingresos: {}", cuenta.obtenerNumeroIngresos());
    log.info("SUMA TOTAL:         {}", cuenta.calcularTotalIngresos());
    log.info("==========================================");
}
```

**Nota**: El resumen también se devuelve como JSON Response en el endpoint REST.

---

#### Párrafo 9000: Finalizar

**COBOL:**
```cobol
9000-FINALIZAR.
    DISPLAY " "
    DISPLAY "Operacion finalizada correctamente."
    DISPLAY "Gracias por usar el sistema bancario."
```

**Spring Boot:**
```java
private void finalizar() {
    log.info(" ");
    log.info("Operacion finalizada correctamente.");
    log.info("Gracias por usar el sistema bancario.");
}
```

---

### ENVIRONMENT DIVISION → Configuration

**COBOL:**
```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
(definiciones de archivos físicos)
```

**Spring Boot - application.properties:**
```properties
# DATABASE
spring.datasource.url=jdbc:h2:mem:bancoingresosdb
spring.datasource.driverClassName=org.h2.Driver
spring.jpa.database-platform=org.hibernate.dialect.H2Dialect
spring.jpa.hibernate.ddl-auto=create-drop

# LOGGING (equivalente a DISPLAY)
logging.level.com.banco=DEBUG
logging.pattern.console=%d{yyyy-MM-dd HH:mm:ss} - %msg%n

# VALIDATION
spring.mvc.throw-exception-if-no-handler-found=true
```

---

## 🎯 Resumen de Patrones Aplicados

### 1. Patrón Data Division → Entity + DTOs

```
COBOL DATA DIVISION (Nivel 01)
        ↓
JAVA @Entity (para persistencia)
        ↓
JAVA DTO (para entrada/salida REST)
```

### 2. Patrón PERFORM → Service Methods

```
COBOL PERFORM párrafo
        ↓
JAVA private method en Service
        ↓
JAVA método público en Controller endpoint
```

### 3. Patrón ACCEPT/DISPLAY → REST API

```
COBOL ACCEPT (entrada interactiva)
        ↓
JAVA @RequestBody, @RequestParam, @PathVariable

COBOL DISPLAY (salida console)
        ↓
JAVA Logger.info(), ResponseEntity<DTO>
```

### 4. Patrón Validación → Bean Validation

```
COBOL IF statements dispersos
        ↓
JAVA @NotBlank, @Positive, @Size, etc. (JSR-380)
```

---

## 📋 Checklist de Traducción Completada

- ✅ IDENTIFICATION DIVISION → BancoIngresosApplication
- ✅ DATA DIVISION → CuentaBancaria, Ingreso (Entities) + DTOs
- ✅ PROCEDURE DIVISION → Service methods + Controller endpoints
- ✅ ENVIRONMENT DIVISION → application.properties
- ✅ Variables PIC → Java types con anotaciones JPA
- ✅ ACCEPT → @RequestParam, @RequestBody
- ✅ DISPLAY → Logger.info() + JSON Responses
- ✅ PERFORM → Service method calls
- ✅ PERFORM UNTIL → for loops
- ✅ PERFORM VARYING → Java Streams
- ✅ IF statements → Bean Validation annotations
- ✅ Excepciones custom para dominio bancario
- ✅ Manejo centralizado de errores
- ✅ Documentación completa
- ✅ README con ejemplos de uso

---

**Documento generado**: 20 de febrero de 2026
**Referencia Memory**: COBOL-SpringBoot-Traduccion (Knowledge Graph)
