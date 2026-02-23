# GUÍA DE EJECUCIÓN Y PRUEBAS

## 🚀 Inicio Rápido

### 1. Compilación

```bash
cd spring-boot-banco-ingresos
mvn clean compile
```

**Resultado esperado:**
```
[INFO] BUILD SUCCESS
[INFO] Total time: XX.XXXs
```

### 2. Pruebas

```bash
mvn test
```

### 3. Empaquetamiento

```bash
mvn package
```

Esto generará: `target/banco-ingresos-1.0.0.jar`

### 4. Ejecución

**Opción A: Con Maven**
```bash
mvn spring-boot:run
```

**Opción B: Con Java (desde target/)**
```bash
java -jar target/banco-ingresos-1.0.0.jar
```

**Salida esperada:**
```
2026-02-20 14:30:00 - Started BancoIngresosApplication in X seconds
2026-02-20 14:30:00 - Tomcat started on port 8080 with context path '/banco-ingresos'
```

---

## 🧪 Pruebas Manuales (cURL)

### Configuración Previa

```bash
# URL base
BASE_URL="http://localhost:8080/banco-ingresos"

# Crear archivo de salida para respuestas
mkdir -p logs
```

### Test 1: Procesar Ingresos (Flujo Principal)

**Crear archivo `request-procesar.json`:**
```json
[
  {
    "importe": 1500.50,
    "concepto": "Salario febrero 2026",
    "descripcion": "Pago de nómina mensual"
  },
  {
    "importe": 300.00,
    "concepto": "Bono desempeño",
    "descripcion": "Bono especial por proyecto completado"
  },
  {
    "importe": 50.25,
    "concepto": "Reembolso gastos",
    "descripcion": "Gastos de representación"
  }
]
```

**Ejecutar request:**
```bash
curl -X POST \
  "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=ES9121000418450200051332&titular=Juan%20Pérez%20García" \
  -H "Content-Type: application/json" \
  -d @request-procesar.json \
  -w "\nStatus: %{http_code}\n" \
  | jq .
```

**Respuesta esperada (201 Created):**
```json
{
  "id": 1,
  "numeroCuenta": "ES9121000418450200051332",
  "titular": "Juan Pérez García",
  "saldo": 1850.75,
  "numeroIngresos": 3,
  "sumaTotalIngresos": 1850.75,
  "ingresos": [
    {
      "id": 1,
      "importe": 1500.50,
      "concepto": "Salario febrero 2026",
      "fechaIngreso": "2026-02-20T14:30:00",
      "descripcion": "Pago de nómina mensual"
    },
    {
      "id": 2,
      "importe": 300.00,
      "concepto": "Bono desempeño",
      "fechaIngreso": "2026-02-20T14:30:00",
      "descripcion": "Bono especial por proyecto completado"
    },
    {
      "id": 3,
      "importe": 50.25,
      "concepto": "Reembolso gastos",
      "fechaIngreso": "2026-02-20T14:30:00",
      "descripcion": "Gastos de representación"
    }
  ]
}
```

**Verificaciones:**
- ✅ Status code: 201 Created
- ✅ Campo `id` > 0 (cuenta creada)
- ✅ Saldo = suma de ingresos (1850.75)
- ✅ numeroIngresos = 3
- ✅ Todos los campos de ingresos presentes

---

### Test 2: Obtener Cuenta por ID

```bash
curl -X GET \
  "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1" \
  -H "Content-Type: application/json" \
  -w "\nStatus: %{http_code}\n" \
  | jq .
```

**Respuesta esperada (200 OK):**
Same as Test 1

---

### Test 3: Obtener Cuenta por Número

```bash
curl -X GET \
  "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/numero/ES9121000418450200051332" \
  -H "Content-Type: application/json" \
  -w "\nStatus: %{http_code}\n" \
  | jq .
```

---

### Test 4: Agregar Ingreso a Cuenta Existente

**Crear archivo `request-ingreso.json`:**
```json
{
  "importe": 500.00,
  "concepto": "Transferencia de amigo",
  "descripcion": "Transferencia recibida"
}
```

**Ejecutar request:**
```bash
curl -X POST \
  "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1/agregar-ingreso" \
  -H "Content-Type: application/json" \
  -d @request-ingreso.json \
  -w "\nStatus: %{http_code}\n" \
  | jq .
```

**Respuesta esperada (200 OK):**
```json
{
  "id": 1,
  "numeroCuenta": "ES9121000418450200051332",
  "titular": "Juan Pérez García",
  "saldo": 2350.75,
  "numeroIngresos": 4,
  "sumaTotalIngresos": 2350.75,
  "ingresos": [
    // ... anteriores
    {
      "id": 4,
      "importe": 500.00,
      "concepto": "Transferencia de amigo",
      "fechaIngreso": "2026-02-20T14:35:00",
      "descripcion": "Transferencia recibida"
    }
  ]
}
```

**Verificaciones:**
- ✅ Status code: 200 OK
- ✅ numeroIngresos ahora = 4
- ✅ saldo actualizado: 2350.75

---

## ❌ Pruebas de Validación y Errores

### Test 5: Error - Importe Negativo

**Crear archivo `request-error-importe.json`:**
```json
[
  {
    "importe": -1000.00,
    "concepto": "Ingreso negativo",
    "descripcion": "Esto debería fallar"
  }
]
```

**Ejecutar:**
```bash
curl -X POST \
  "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=ES1234&titular=Test" \
  -H "Content-Type: application/json" \
  -d @request-error-importe.json \
  -w "\nStatus: %{http_code}\n"
```

**Respuesta esperada (400 Bad Request):**
```json
{
  "timestamp": "2026-02-20T14:40:00",
  "estado": 400,
  "error": "Operación inválida",
  "mensaje": "El importe debe ser un valor positivo"
}
```

---

### Test 6: Error - Cuenta No Encontrada

```bash
curl -X GET \
  "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/9999" \
  -H "Content-Type: application/json" \
  -w "\nStatus: %{http_code}\n"
```

**Respuesta esperada (404 Not Found):**
```json
{
  "timestamp": "2026-02-20T14:42:00",
  "estado": 404,
  "error": "Cuenta no encontrada",
  "mensaje": "Cuenta no encontrada con ID: 9999"
}
```

---

### Test 7: Error - Validación Bean Validation

**Crear archivo `request-validacion.json`:**
```json
[
  {
    "importe": 1000.00,
    "concepto": "",
    "descripcion": "Concepto vacío - debería fallar"
  }
]
```

**Ejecutar:**
```bash
curl -X POST \
  "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=&titular=Test" \
  -H "Content-Type: application/json" \
  -d @request-validacion.json \
  -w "\nStatus: %{http_code}\n"
```

**Respuesta esperada (400 Bad Request):**
Se validarán automáticamente:
- numeroCuenta vacío
- concepto vacío

---

## 📊 Pruebas en la Consola H2

### Acceder a la consola

1. Abrir navegador: `http://localhost:8080/banco-ingresos/h2-console`
2. Configuración:
   - **Driver Class**: `org.h2.Driver`
   - **JDBC URL**: `jdbc:h2:mem:bancoingresosdb`
   - **Username**: `sa`
   - **Password**: (dejar vacío)

### Queries útiles

**Ver todas las cuentas:**
```sql
SELECT * FROM cuentas_bancarias;
```

**Ver todos los ingresos:**
```sql
SELECT * FROM ingresos;
```

**Suma total por cuenta:**
```sql
SELECT cb.numero_cuenta, cb.titular, SUM(i.importe) as total
FROM ingresos i
JOIN cuentas_bancarias cb ON i.cuenta_bancaria_id = cb.id
GROUP BY cb.id, cb.numero_cuenta, cb.titular;
```

**Detalles de ingresos con cuenta:**
```sql
SELECT 
    cb.numero_cuenta,
    cb.titular,
    i.importe,
    i.concepto,
    i.fecha_ingreso
FROM ingresos i
JOIN cuentas_bancarias cb ON i.cuenta_bancaria_id = cb.id
ORDER BY i.fecha_ingreso DESC;
```

---

## 📝 Automatización con Script Bash

**Crear archivo `test-completo.sh`:**

```bash
#!/bin/bash

# Colores
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}=== PRUEBAS COMPLETAS DE BANCO-INGRESOS ===${NC}\n"

BASE_URL="http://localhost:8080/banco-ingresos"
NUMERO_CUENTA="ES9121000418450200051332"
TITULAR="Juan%20Pérez%20García"

# Test 1: Procesar ingresos
echo -e "${YELLOW}Test 1: Procesar ingresos${NC}"
curl -X POST \
  "$BASE_URL/api/ingresos/procesar?numeroCuenta=$NUMERO_CUENTA&titular=$TITULAR" \
  -H "Content-Type: application/json" \
  -d '[
    {"importe": 1500.50, "concepto": "Salario", "descripcion": "Pago de nómina"},
    {"importe": 300.00, "concepto": "Bono", "descripcion": "Bono especial"}
  ]' \
  -w "\nStatus: %{http_code}\n\n"

# Test 2: Obtener cuenta
echo -e "${YELLOW}Test 2: Obtener cuenta por número${NC}"
curl -X GET \
  "$BASE_URL/api/ingresos/cuentas/numero/$NUMERO_CUENTA" \
  -H "Content-Type: application/json" \
  -w "\nStatus: %{http_code}\n\n"

# Test 3: Agregar ingreso
echo -e "${YELLOW}Test 3: Agregar ingreso a cuenta existente${NC}"
curl -X POST \
  "$BASE_URL/api/ingresos/cuentas/1/agregar-ingreso" \
  -H "Content-Type: application/json" \
  -d '{"importe": 500.00, "concepto": "Transferencia"}' \
  -w "\nStatus: %{http_code}\n\n"

echo -e "${GREEN}=== PRUEBAS COMPLETADAS ===${NC}"
```

**Ejecutar:**
```bash
chmod +x test-completo.sh
./test-completo.sh
```

---

## 🔍 Validación de Respuestas

### Checklist de Validación

Para cada request, verificar:

- [ ] **Status Code correcto**
  - 200 OK (GET)
  - 201 Created (POST crear)
  - 200 OK (POST modificar)
  - 400 Bad Request (validación)
  - 404 Not Found (recurso no existe)

- [ ] **Content-Type:** `application/json`

- [ ] **Body válido:**
  ```bash
  jq . respuesta.json  # Válida que sea JSON
  ```

- [ ] **Headers esperados:**
  ```bash
  curl -i ...  # Ver headers
  ```

---

## 📋 Tabla de Test Cases

| # | Test Case | Request | Expected Code | Validación |
|---|-----------|---------|---|---|
| 1 | Procesar ingresos nuevos | POST /procesar | 201 | saldo = suma ingresos |
| 2 | Obtener cuenta por ID | GET /cuentas/{id} | 200 | datos completos |
| 3 | Obtener por número cuenta | GET /cuentas/numero/{num} | 200 | datos incluyen ingresos |
| 4 | Agregar ingreso | POST /cuentas/{id}/agregar | 200 | numeroIngresos += 1 |
| 5 | Importe negativo | POST con importe < 0 | 400 | mensaje validación |
| 6 | Cuenta no existe | GET /cuentas/9999 | 404 | error 404 |
| 7 | Datos incompletos | POST con concepto vacío | 400 | @NotBlank validado |
| 8 | Número cuenta duplicado | POST mismo número 2x | 200 | reutiliza cuenta |
| 9 | Concurrent requests | Múltiples POST simultáneos | 201 | sin race conditions |
| 10 | Base de datos reinicia | Reiniciar app | - | datos se pierden (H2 memory) |

---

## 🐛 Troubleshooting

### Error: Port 8080 already in use

```bash
# Linux/Mac: Encontrar proceso en puerto 8080
lsof -i :8080

# Matar proceso
kill -9 <PID>

# O cambiar puerto en application.properties:
# server.port=8081
```

### Error: Connection refused

```bash
# Verificar que la app está corriendo
curl http://localhost:8080/banco-ingresos

# Si no responde, revisar logs de Maven/Spring
# Spring Boot tarda ~5 segundos en iniciar
```

### Error: H2 Database locked

```bash
# H2 in-memory se reinicia cada vez que se reinicia la app
# Esto es normal. Ver logs en startup:
# "create-drop" generará schemas nuevos
```

### Error: Validation failed

```bash
# Revisar logs para ver qué campo falló
# Ejemplos comunes:
# - importe debe ser > 0
# - concepto no puede estar vacío
# - numeroCuenta debe tener máx 20 chars
```

---

## 📊 Métricas de Prueba

**Ejemplo de salida esperada al completar todos los tests:**

```
✅ 10 tests ejecutados
✅ 8 tests passed
⚠️ 2 tests con validaciones esperadas (400, 404)
⏱️ Tiempo total: 2.5 segundos
📊 Cobertura de endpoints: 100%
```

---

**Documento actualizado**: 20 de febrero de 2026
