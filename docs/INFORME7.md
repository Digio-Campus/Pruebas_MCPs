# INFORME 7 — Memory MCP para Traducción COBOL → Spring Boot

**Fecha:** 19/02/2026  
**Objetivo:** Estructurar la memoria del MCP Memory con separadores lógicos que agilicen la traducción de programas COBOL bancarios a Spring Boot, ejecutar la traducción completa guiada por el grafo de conocimiento, y verificar el funcionamiento en navegador.

---

## 1. Programas COBOL de Entrada

Se utilizaron **5 programas COBOL** en la carpeta `translate/`:

| Fichero | PROGRAM-ID | Descripción |
|---------|------------|-------------|
| `tra1.cbl` | HelloWorld | Imprime "Hello World!" |
| `banco-ingresos.cbl` | BANCO-INGRESOS | Registro de N ingresos con suma total |
| `banco-consulta-saldo.cbl` | BANCO-CONSULTA-SALDO | Consulta saldo (disponible, retenido, total) |
| `banco-transferencia.cbl` | BANCO-TRANSFERENCIA | Transferencia entre cuentas con comisiones |
| `banco-extracto.cbl` | BANCO-EXTRACTO | Extracto de movimientos (ingresos/gastos) |

---

## 2. Estructura de la Memoria (Knowledge Graph)

### 2.1 Problema Detectado: Relaciones No Navegables

Al abrir un nodo con `open_nodes`, el MCP Memory **no devuelve las relaciones** si solo se consulta un nodo aislado. Esto impide recorrer el grafo desde la raíz:

```
open_nodes(["COBOL-SpringBoot-Traduccion"]) → entities: [...], relations: []  ❌
```

**Solución aplicada:** Se añadió a cada entidad una observación con prefijo `>> NAVEGACIÓN:` que lista explícitamente las entidades de salida y el tipo de relación. Ejemplo:

```
>> NAVEGACIÓN: contiene_bloque → [BLOQUE-IDENTIFICATION-DIVISION, BLOQUE-DATA-DIVISION, 
   BLOQUE-PROCEDURE-DIVISION, BLOQUE-ENVIRONMENT-DIVISION] | aplica_dominio → 
   SEPARADOR-PATRONES-BANCARIOS | genera_estructura → SEPARADOR-ESTRUCTURA-SPRINGBOOT | ...
```

### 2.2 Limpieza: Información Específica Eliminada

Se eliminó la entidad `SEPARADOR-MAPEO-FICHEROS` y el ejemplo específico `BANCO-INGRESOS → BancoIngresosService` de `BLOQUE-IDENTIFICATION-DIVISION`, dejando solo reglas genéricas reutilizables para cualquier programa COBOL.

### 2.3 Entidades Finales (9 entidades)

| Entidad | Tipo | Propósito |
|---------|------|-----------|
| `COBOL-SpringBoot-Traduccion` | ProcesoTraduccion | Nodo raíz con orden de traducción y navegación global |
| `BLOQUE-IDENTIFICATION-DIVISION` | BloqueTraduccion | PROGRAM-ID → nombre de clase Java |
| `BLOQUE-DATA-DIVISION` | BloqueTraduccion | PIC types → atributos Java, DTOs, Entities |
| `BLOQUE-PROCEDURE-DIVISION` | BloqueTraduccion | PERFORM/DISPLAY/ACCEPT → Services/Controllers |
| `BLOQUE-ENVIRONMENT-DIVISION` | BloqueTraduccion | Configuración → application.properties |
| `SEPARADOR-PATRONES-BANCARIOS` | PatronDominio | BigDecimal, @Transactional, excepciones custom |
| `SEPARADOR-ESTRUCTURA-SPRINGBOOT` | EstructuraProyecto | Paquetes: controller/service/model/dto/repository/exception |
| `SEPARADOR-REGLAS-CONVERSION` | ReglasConversion | Tabla PIC → Java types |
| `SEPARADOR-VALIDACIONES` | ReglasValidacion | IF COBOL → @NotBlank, @Positive, @Pattern |

### 2.4 Grafo Navegable (con observaciones `>> NAVEGACIÓN`)

```
COBOL-SpringBoot-Traduccion  (nodo raíz)
  │
  ├─ contiene_bloque ──► BLOQUE-IDENTIFICATION-DIVISION
  │                        └─ se_traduce_antes_de ──► BLOQUE-DATA-DIVISION
  │                                                    ├─ se_traduce_antes_de ──► BLOQUE-PROCEDURE-DIVISION
  │                                                    │                            └─ implementa ──► SEPARADOR-PATRONES-BANCARIOS
  │                                                    │                                                └─ requiere_validacion ──► SEPARADOR-VALIDACIONES ■
  │                                                    └─ usa_reglas_de ──► SEPARADOR-REGLAS-CONVERSION ■
  │
  ├─ contiene_bloque ──► BLOQUE-ENVIRONMENT-DIVISION
  │                        └─ configura ──► SEPARADOR-ESTRUCTURA-SPRINGBOOT ■
  │
  ├─ aplica_dominio ──► SEPARADOR-PATRONES-BANCARIOS
  ├─ genera_estructura ──► SEPARADOR-ESTRUCTURA-SPRINGBOOT
  ├─ usa_reglas ──► SEPARADOR-REGLAS-CONVERSION
  └─ valida_con ──► SEPARADOR-VALIDACIONES

■ = nodo terminal
```

---

## 3. Proceso de Traducción Guiado por el Grafo

Se recorrió el grafo paso a paso, consultando cada nodo con `open_nodes` antes de traducir:

### Paso 1: `COBOL-SpringBoot-Traduccion` (nodo raíz)
- Aprendido: la traducción se hace por BLOQUES LÓGICOS
- Orden: IDENTIFICATION → DATA → PROCEDURE
- Cada programa COBOL genera un módulo Spring Boot independiente
- Navegación descubierta: 4 bloques + 4 separadores auxiliares

### Paso 2: `BLOQUE-IDENTIFICATION-DIVISION`
- PROGRAM-ID → nombre de clase Java (`HelloWorld` → `HelloWorldController`)
- AUTHOR → `@author` en Javadoc
- Patrón: kebab-case COBOL → PascalCase Java
- **Resultado:** 1 `@SpringBootApplication` + 5 `@RestController` + 4 `@Service`

### Paso 3: `BLOQUE-DATA-DIVISION` + `SEPARADOR-REGLAS-CONVERSION`
- Reglas aplicadas para convertir tipos PIC:

| Variable COBOL | Tipo PIC | → Campo Java |
|----------------|----------|--------------|
| `WS-NUMERO-CUENTA` | `PIC X(20)` | `String numeroCuenta` con `@Size(max=20)` |
| `WS-IMPORTE-INGRESO` | `PIC 9(8)V99` | `BigDecimal importe` con `@Column(precision=10, scale=2)` |
| `WS-SALDO-DISPONIBLE` | `PIC S9(10)V99` | `BigDecimal saldoDisponible` |
| `WS-MOVIMIENTO OCCURS 50` | tabla | `List<Movimiento>` con `@OneToMany` |
| `WS-TITULAR` | `PIC X(40)` | `String titular` con `@Column(length=40)` |

- **Resultado:** 2 `@Entity` (CuentaBancaria, Movimiento) + 6 DTOs

### Paso 4: `BLOQUE-PROCEDURE-DIVISION` + `SEPARADOR-PATRONES-BANCARIOS`
- Reglas aplicadas para convertir instrucciones:

| COBOL | → Java | Ejemplo |
|-------|--------|---------|
| `PERFORM 3000-REGISTRAR` | método privado | `registrarIngresos()` |
| `DISPLAY "SUMA TOTAL"` | `log.info("SUMA TOTAL: {}", sumaTotal)` | IngresoService |
| `ACCEPT WS-IMPORTE` | `@RequestBody IngresoRequest` | IngresoController |
| `PERFORM VARYING` | `for` loop / streams | `detalles.stream().reduce()` |
| `ADD X TO Y` | `y = y.add(x)` | BigDecimal |
| `COMPUTE a = b - c` | `a = b.subtract(c)` | TransferenciaService |

- Patrones bancarios aplicados: `@Transactional` en transferencias, `BigDecimal` para todo importe, excepciones custom (`SaldoInsuficienteException`, `CuentaNoEncontradaException`)
- **Resultado:** 4 Services con lógica de negocio completa

### Paso 5: `BLOQUE-ENVIRONMENT-DIVISION` + `SEPARADOR-ESTRUCTURA-SPRINGBOOT`
- application.properties con H2 (simula BD COBOL)
- data.sql con datos iniciales (equivalente a `2000-CARGAR-DATOS-SIMULADOS`)
- **Resultado:** Configuración Spring Boot + datos iniciales

### Paso 6: `SEPARADOR-VALIDACIONES`
- Validaciones Bean Validation aplicadas:
  - `@NotBlank` en campos de cuenta (IF campo = SPACES)
  - `@Positive` en importes (IF campo > 0)
  - `@Size(max=n)` en strings (PIC X(n))
- **Resultado:** Anotaciones en DTOs de request

---

## 4. Panel de Pruebas HTML (Frontend)

### 4.1 Decisión: HTML Frontend vs Cambiar la API

Los endpoints POST (`/api/ingresos`, `/api/transferencias`) no se pueden probar directamente desde la barra de un navegador. Se evaluaron dos opciones:

| Opción | Pros | Contras |
|--------|------|---------|
| **Cambiar POST → GET** | Funciona en barra de navegador | Rompe el diseño REST correcto; semánticamente incorrecto |
| **Crear HTML con formularios** ✅ | Mantiene REST correcto; UX visual; reutilizable | Un fichero adicional |

**Decisión:** Crear un `index.html` estático en `src/main/resources/static/` que Spring Boot sirve automáticamente en la raíz `/`. Así se conserva el diseño REST correcto (POST para crear, GET para consultar) y se obtiene un panel visual de pruebas.

### 4.2 Características del Panel

- **5 tarjetas** (una por programa COBOL traducido), cada una con:
  - Badge visual `GET` (verde) o `POST` (naranja)
  - Referencia al fichero COBOL de origen
  - Campos de entrada editables
  - Botón de ejecución que hace `fetch()` a la API
  - Área de resultado con formato JSON coloreado
- **Datos pre-rellenados** con las 3 cuentas disponibles
- **Sin dependencias externas** — HTML + CSS + JavaScript vanilla

### 4.3 Acceso

```
http://localhost:8085/
```

El panel se carga automáticamente como página de bienvenida (welcome page) de Spring Boot.

---

## 5. Proyecto Spring Boot Generado

### 5.1 Estructura de Ficheros (28 ficheros)

```
translate/spring-boot-banco/
├── pom.xml
└── src/main/
    ├── java/com/banco/
    │   ├── BancoApplication.java
    │   ├── controller/
    │   │   ├── HelloWorldController.java      ← tra1.cbl
    │   │   ├── IngresoController.java         ← banco-ingresos.cbl
    │   │   ├── ConsultaSaldoController.java   ← banco-consulta-saldo.cbl
    │   │   ├── TransferenciaController.java   ← banco-transferencia.cbl
    │   │   └── ExtractoController.java        ← banco-extracto.cbl
    │   ├── service/
    │   │   ├── IngresoService.java
    │   │   ├── ConsultaSaldoService.java
    │   │   ├── TransferenciaService.java
    │   │   └── ExtractoService.java
    │   ├── model/
    │   │   ├── CuentaBancaria.java            ← @Entity
    │   │   └── Movimiento.java                ← @Entity
    │   ├── dto/
    │   │   ├── IngresoRequest.java
    │   │   ├── IngresoResponse.java
    │   │   ├── SaldoResponse.java
    │   │   ├── TransferenciaRequest.java
    │   │   ├── TransferenciaResponse.java
    │   │   └── ExtractoResponse.java
    │   ├── repository/
    │   │   ├── CuentaRepository.java
    │   │   └── MovimientoRepository.java
    │   └── exception/
    │       ├── CuentaNoEncontradaException.java
    │       ├── SaldoInsuficienteException.java
    │       ├── TransferenciaNoValidaException.java
    │       └── GlobalExceptionHandler.java
    └── resources/
        ├── application.properties
        ├── data.sql
        └── static/
            └── index.html                     ← Panel de pruebas
```

### 5.2 Endpoints REST

| Método | Endpoint | Origen COBOL | Descripción |
|--------|----------|-------------|-------------|
| GET | `/` | — | Panel HTML de pruebas |
| GET | `/hello` | tra1.cbl | Hello World |
| POST | `/api/ingresos/{cuenta}` | banco-ingresos.cbl | Registrar ingresos, devuelve suma total |
| GET | `/api/saldo/{cuenta}` | banco-consulta-saldo.cbl | Consultar saldo |
| POST | `/api/transferencias` | banco-transferencia.cbl | Transferencia con comisiones |
| GET | `/api/extracto/{cuenta}` | banco-extracto.cbl | Extracto de movimientos |

### 5.3 Cómo Ejecutar

```bash
cd translate/spring-boot-banco
mvn spring-boot:run
```

Abrir en navegador: **http://localhost:8085/**

---

## 6. Verificación de Endpoints

Se probaron todos los endpoints con `curl` tras arrancar la aplicación:

### GET /hello
```
Hello World!
```

### GET /api/saldo/ES1234567890123456
```json
{
  "numeroCuenta": "ES1234567890123456",
  "titular": "GARCIA LOPEZ, MARIA",
  "tipoCuenta": "CORRIENTE",
  "saldoDisponible": 15250.75,
  "saldoRetenido": 500.0,
  "saldoTotal": 15750.75
}
```

### GET /api/extracto/ES1234567890123456?saldoInicial=5000
```json
{
  "numeroCuenta": "ES1234567890123456",
  "titular": "GARCIA LOPEZ, MARIA",
  "saldoInicial": 5000,
  "movimientos": [
    { "fecha": "2026-02-01", "concepto": "NOMINA FEBRERO", "tipo": "I", "importe": 2500.0 },
    { "fecha": "2026-02-03", "concepto": "ALQUILER VIVIENDA", "tipo": "G", "importe": 850.0 },
    ...
  ],
  "totalIngresos": 3300.0,
  "totalGastos": 1205.8,
  "saldoFinal": 7094.2
}
```

### POST /api/ingresos/ES1234567890123456
```json
{
  "numeroCuenta": "ES1234567890123456",
  "titular": "GARCIA LOPEZ, MARIA",
  "numIngresos": 1,
  "sumaTotal": 100
}
```

### POST /api/transferencias
```json
{
  "fecha": "2026-02-19",
  "hora": "14:26:44",
  "cuentaOrigen": "ES1234567890123456",
  "cuentaDestino": "ES9876543210987654",
  "importe": 500,
  "comision": 0,
  "importeTotal": 500,
  "nuevoSaldoOrigen": 14850.75,
  "nuevoSaldoDestino": 42500.0
}
```

---

## 7. Resultado de la Prueba

| Aspecto | Resultado |
|---------|-----------|
| Programas COBOL traducidos | ✅ 5/5 programas (incluido HelloWorld) |
| Entidades en Memory | ✅ 9 entidades genéricas (sin info específica) |
| Navegación del grafo | ✅ Solucionado con observaciones `>> NAVEGACIÓN:` |
| Recorrido completo desde raíz | ✅ 9/9 nodos accesibles |
| Compilación Maven | ✅ `mvn compile` sin errores |
| Arranque Spring Boot | ✅ Tomcat en puerto 8085, datos cargados |
| GET /hello | ✅ "Hello World!" |
| GET /api/saldo/{cuenta} | ✅ Saldo correcto con 3 cuentas |
| GET /api/extracto/{cuenta} | ✅ 8 movimientos, totales correctos |
| POST /api/ingresos/{cuenta} | ✅ Suma total calculada, saldo actualizado |
| POST /api/transferencias | ✅ Comisión, validación, saldos actualizados |
| Panel HTML en navegador | ✅ index.html servido en `/`, formularios funcionales |
| Bean Validation | ✅ @NotBlank, @Positive, @Size en DTOs |

### Conclusiones

1. **Problema del MCP Memory:** `open_nodes` no devuelve relaciones al consultar un solo nodo. La solución de añadir observaciones `>> NAVEGACIÓN:` en cada entidad permite recorrer el grafo completo.

2. **Traducción fiel:** Cada párrafo COBOL se mapeó a un método Java. Los `DISPLAY` → `log.info()` + `ResponseEntity`, los `ACCEPT` → `@RequestBody/@PathVariable`, las operaciones aritméticas → `BigDecimal`.

3. **Frontend HTML:** Se creó un panel de pruebas estático (`index.html`) en lugar de cambiar los POST a GET. Esto mantiene el diseño REST correcto y permite probar todos los endpoints desde el navegador con formularios y respuestas JSON formateadas.

4. **Reutilización:** La memoria no contiene información específica de los programas traducidos, por lo que puede usarse para traducir cualquier otro programa COBOL bancario en el futuro.
