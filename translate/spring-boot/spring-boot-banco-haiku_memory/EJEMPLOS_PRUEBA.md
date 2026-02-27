# Ejemplos de Prueba - API REST Banco Ingresos

## 📌 Información Previa

- **URL Base**: `http://localhost:8080/banco-ingresos`
- **API Base**: `http://localhost:8080/banco-ingresos/api/ingresos`
- El servidor debe estar ejecutándose antes de realizar las pruebas
- Copie los comandos y cópielos en su terminal (bash, zsh, PowerShell, etc.)

---

## 1. Procesar Ingresos (Operación Principal)

Este endpoint simula la ejecución del programa COBOL BANCO-INGRESOS:
- 1000-INICIALIZAR
- 2000-PEDIR-DATOS-CUENTA
- 3000-REGISTRAR-INGRESOS
- 4000-CALCULAR-TOTAL
- 5000-MOSTRAR-RESUMEN
- 9000-FINALIZAR

### Endpoint
```
POST /api/ingresos/procesar?numeroCuenta=1234567890&titular=Juan%20Pérez
Content-Type: application/json
```

### Ejemplo 1: Registro de múltiples ingresos

**Comando cURL:**
```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=1234567890&titular=Juan%20Pérez" \
  -H "Content-Type: application/json" \
  -d '[
    {
      "importe": 1500.50,
      "concepto": "Salario mensual",
      "descripcion": "Pago de nómina febrero 2026"
    },
    {
      "importe": 250.00,
      "concepto": "Bonificación",
      "descripcion": "Bonificación por desempeño"
    },
    {
      "importe": 100.75,
      "concepto": "Intereses",
      "descripcion": "Intereses de la cuenta"
    }
  ]'
```

**Respuesta esperada (201 Created):**
```json
{
  "id": 1,
  "numeroCuenta": "1234567890",
  "titular": "Juan Pérez",
  "saldo": 1851.25,
  "numeroIngresos": 3,
  "sumaTotalIngresos": 1851.25,
  "ingresos": [
    {
      "id": 1,
      "importe": 1500.50,
      "concepto": "Salario mensual",
      "fechaIngreso": "2026-02-20T14:30:45.123456",
      "descripcion": "Pago de nómina febrero 2026"
    },
    {
      "id": 2,
      "importe": 250.00,
      "concepto": "Bonificación",
      "fechaIngreso": "2026-02-20T14:30:45.234567",
      "descripcion": "Bonificación por desempeño"
    },
    {
      "id": 3,
      "importe": 100.75,
      "concepto": "Intereses",
      "fechaIngreso": "2026-02-20T14:30:45.345678",
      "descripcion": "Intereses de la cuenta"
    }
  ]
}
```

### Ejemplo 2: Registro de un único ingreso

**Comando cURL:**
```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=9876543210&titular=María%20García" \
  -H "Content-Type: application/json" \
  -d '[
    {
      "importe": 5000.00,
      "concepto": "Depósito inicial",
      "descripcion": "Apertura de cuenta"
    }
  ]'
```

---

## 2. Obtener Cuenta por ID

Recupera los detalles completos de una cuenta incluyendo todos sus ingresos.

### Endpoint
```
GET /api/ingresos/cuentas/{id}
```

### Ejemplo

**Comando cURL:**
```bash
curl -X GET "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1" \
  -H "Accept: application/json"
```

**Respuesta esperada (200 OK):**
```json
{
  "id": 1,
  "numeroCuenta": "1234567890",
  "titular": "Juan Pérez",
  "saldo": 1851.25,
  "numeroIngresos": 3,
  "sumaTotalIngresos": 1851.25,
  "ingresos": [
    {
      "id": 1,
      "importe": 1500.50,
      "concepto": "Salario mensual",
      "fechaIngreso": "2026-02-20T14:30:45.123456",
      "descripcion": "Pago de nómina febrero 2026"
    },
    {
      "id": 2,
      "importe": 250.00,
      "concepto": "Bonificación",
      "fechaIngreso": "2026-02-20T14:30:45.234567",
      "descripcion": "Bonificación por desempeño"
    },
    {
      "id": 3,
      "importe": 100.75,
      "concepto": "Intereses",
      "fechaIngreso": "2026-02-20T14:30:45.345678",
      "descripcion": "Intereses de la cuenta"
    }
  ]
}
```

---

## 3. Obtener Cuenta por Número de Cuenta

Busca una cuenta utilizando su número identificador.

### Endpoint
```
GET /api/ingresos/cuentas/numero/{numeroCuenta}
```

### Ejemplo

**Comando cURL:**
```bash
curl -X GET "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/numero/1234567890" \
  -H "Accept: application/json"
```

**Respuesta esperada (200 OK):**
```json
{
  "id": 1,
  "numeroCuenta": "1234567890",
  "titular": "Juan Pérez",
  "saldo": 1851.25,
  "numeroIngresos": 3,
  "sumaTotalIngresos": 1851.25,
  "ingresos": [...]
}
```

---

## 4. Agregar Ingreso a Cuenta Existente

Agrega un nuevo ingreso a una cuenta que ya existe en el sistema.

### Endpoint
```
POST /api/ingresos/cuentas/{cuentaId}/agregar-ingreso
Content-Type: application/json
```

### Ejemplo

**Comando cURL:**
```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1/agregar-ingreso" \
  -H "Content-Type: application/json" \
  -d '{
    "importe": 750.00,
    "concepto": "Transferencia recibida",
    "descripcion": "Transferencia de Juan López"
  }'
```

**Respuesta esperada (200 OK):**
```json
{
  "id": 1,
  "numeroCuenta": "1234567890",
  "titular": "Juan Pérez",
  "saldo": 2601.25,
  "numeroIngresos": 4,
  "sumaTotalIngresos": 2601.25,
  "ingresos": [
    {
      "id": 1,
      "importe": 1500.50,
      "concepto": "Salario mensual",
      "fechaIngreso": "2026-02-20T14:30:45.123456",
      "descripcion": "Pago de nómina febrero 2026"
    },
    {
      "id": 2,
      "importe": 250.00,
      "concepto": "Bonificación",
      "fechaIngreso": "2026-02-20T14:30:45.234567",
      "descripcion": "Bonificación por desempeño"
    },
    {
      "id": 3,
      "importe": 100.75,
      "concepto": "Intereses",
      "fechaIngreso": "2026-02-20T14:30:45.345678",
      "descripcion": "Intereses de la cuenta"
    },
    {
      "id": 4,
      "importe": 750.00,
      "concepto": "Transferencia recibida",
      "fechaIngreso": "2026-02-20T14:35:22.567890",
      "descripcion": "Transferencia de Juan López"
    }
  ]
}
```

---

## 🔴 Manejo de Errores

### Error 1: Importe inválido (negativo o cero)

**Comando cURL:**
```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=1111111111&titular=Test" \
  -H "Content-Type: application/json" \
  -d '[
    {
      "importe": -100.00,
      "concepto": "Ingreso inválido"
    }
  ]'
```

**Respuesta esperada (400 Bad Request):**
```json
{
  "error": "OperacionInvalidaException",
  "mensaje": "El importe debe ser un valor positivo",
  "timestamp": "2026-02-20T14:40:30.123456"
}
```

### Error 2: Concepto vacío

**Comando cURL:**
```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=2222222222&titular=Test2" \
  -H "Content-Type: application/json" \
  -d '[
    {
      "importe": 100.00,
      "concepto": ""
    }
  ]'
```

**Respuesta esperada (400 Bad Request):**
```json
{
  "errors": [
    {
      "field": "concepto",
      "message": "El concepto del ingreso no puede estar vacío"
    }
  ]
}
```

### Error 3: Cuenta no encontrada

**Comando cURL:**
```bash
curl -X GET "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/9999"
```

**Respuesta esperada (404 Not Found):**
```json
{
  "error": "CuentaNoEncontradaException",
  "mensaje": "Cuenta no encontrada con ID: 9999",
  "timestamp": "2026-02-20T14:45:15.123456"
}
```

---

## 📊 Flujo Completo de Prueba

### Paso 1: Crear cuenta con ingresos iniciales

```bash
# Crear la primera cuenta con 3 ingresos
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=ACC001&titular=Cliente%20Ejemplo" \
  -H "Content-Type: application/json" \
  -d '[
    {"importe": 2000.00, "concepto": "Depósito inicial"},
    {"importe": 500.00, "concepto": "Interes"},
    {"importe": 300.50, "concepto": "Comisión negativa"}
  ]'
```

Guarde el `id` de la respuesta (ej: `1`).

### Paso 2: Obtener cuenta completa

```bash
curl -X GET "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1"
```

### Paso 3: Agregar un nuevo ingreso

```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1/agregar-ingreso" \
  -H "Content-Type: application/json" \
  -d '{"importe": 1500.00, "concepto": "Ingreso adicional"}'
```

### Paso 4: Verificar cambios

```bash
curl -X GET "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/numero/ACC001"
```

---

## 💡 Notas Importantes

1. **Validación de datos**: El servidor valida automáticamente:
   - El importe debe ser positivo
   - El concepto no puede estar vacío
   - El concepto máximo 30 caracteres
   - El número de cuenta máximo 20 caracteres
   - El titular máximo 40 caracteres

2. **Base de datos**: Se utiliza H2 en memoria (`jdbc:h2:mem:bancoingresosdb`)
   - Los datos se pierden al reiniciar la aplicación
   - Acceda a la consola en: `http://localhost:8080/banco-ingresos/h2-console`

3. **Formato de respuesta**: Todas las respuestas son en JSON

4. **Timestamps**: Los timestamps se guardan automáticamente en formato ISO-8601

---

## 🧪 Herramientas Alternativas de Prueba

### Con Postman
1. Importe la colección desde [URL postman si existe]
2. Configure la variable base_url: `http://localhost:8080/banco-ingresos`
3. Ejecute los requests en orden

### Con HTTPie
```bash
# Procesar ingresos
http POST http://localhost:8080/banco-ingresos/api/ingresos/procesar \
  numeroCuenta==ACC001 \
  titular=="Cliente Prueba" \
  < ingresos.json

# Obtener cuenta
http GET http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1
```

### Con wget
```bash
wget --method=GET \
  "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1" \
  --header="Accept: application/json"
```

---

## 📝 Archivo de Datos JSON (ingresos.json)

Guarde este contenido en un archivo llamado `ingresos.json`:

```json
[
  {
    "importe": 1500.50,
    "concepto": "Salario mensual",
    "descripcion": "Pago de nómina"
  },
  {
    "importe": 250.00,
    "concepto": "Bonificación",
    "descripcion": "Bonificación por desempeño"
  },
  {
    "importe": 100.75,
    "concepto": "Intereses",
    "descripcion": "Intereses de la cuenta"
  }
]
```

Luego úselo en cURL:

```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=TEST001&titular=Test%20User" \
  -H "Content-Type: application/json" \
  -d @ingresos.json
```

---

**Última actualización**: 20 de febrero de 2026
