# ⚡ Guía de Inicio Rápido - Banco Ingresos Spring Boot

## 📦 Requisitos Previos

- **Java**: 17 o superior
- **Maven**: 3.8.1 o superior
- **Git** (opcional, para clonar el repositorio)

Verificar instalación:
```bash
java -version
mvn -version
```

---

## 🚀 Pasos para Ejecutar

### 1. Navegar al directorio del proyecto

```bash
cd spring-boot-banco-ingresos
```

### 2. Compilar el proyecto

```bash
mvn clean compile
```

**Salida esperada:**
```
[INFO] BUILD SUCCESS
[INFO] Total time: XX.XXXs
```

### 3. Ejecutar las pruebas (opcional)

```bash
mvn test
```

### 4. Empaquetar la aplicación

```bash
mvn package
```

Esto genera: `target/banco-ingresos-1.0.0.jar`

### 5. Iniciar la aplicación

**Opción A: Directamente desde Maven**
```bash
mvn spring-boot:run
```

**Opción B: Desde el JAR empaquetado**
```bash
java -jar target/banco-ingresos-1.0.0.jar
```

### 6. Verificar que está ejecutándose

Una vez iniciado, debe ver mensajes como:
```
Tomcat started on port(s): 8080 (http)
Started BancoIngresosApplication in X.XXX seconds
```

Y podrá acceder a:
- **API Base**: `http://localhost:8080/banco-ingresos/api/ingresos`
- **Consola H2**: `http://localhost:8080/banco-ingresos/h2-console`

---

## 🧪 Prueba Rápida

Abra una nueva terminal y ejecute:

```bash
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=PRUEBA001&titular=TestUser" \
  -H "Content-Type: application/json" \
  -d '[
    {"importe": 100.00, "concepto": "Test 1"},
    {"importe": 200.00, "concepto": "Test 2"}
  ]'
```

**Respuesta esperada:**
```json
{
  "id": 1,
  "numeroCuenta": "PRUEBA001",
  "titular": "TestUser",
  "saldo": 300.00,
  "numeroIngresos": 2,
  "sumaTotalIngresos": 300.00,
  "ingresos": [...]
}
```

---

## 📚 Documentación Completa

- **README.md** - Descripción detallada del proyecto
- **GUIA_EJECUCION.md** - Guía exhaustiva de ejecución
- **MAPEO_DETALLADO.md** - Mapeo técnico COBOL → Spring Boot
- **EJEMPLOS_PRUEBA.md** - Ejemplos y casos de uso
- **postman_collection.json** - Colección para Postman

---

## 🆘 Solución de Problemas

### Puerto 8080 ya está en uso

```bash
# En Linux/Mac: Encontrar el proceso
lsof -i :8080
# Matar el proceso
kill -9 <PID>

# En Windows: Abrir cmd como administrador
netstat -ano | findstr :8080
taskkill /PID <PID> /F
```

### Maven no compiliza correctamente

Limpiar caché Maven:
```bash
mvn clean
rm -rf ~/.m2/repository
mvn install
```

### Error: "Cannot find Java"

Verificar la configuración de JAVA_HOME:
```bash
echo $JAVA_HOME
# Si está vacío, establecerlo:
export JAVA_HOME=/path/to/java17
```

---

## 📊 Estructura de Directorio

```
spring-boot-banco-ingresos/
├── src/
│   ├── main/
│   │   ├── java/com/banco/
│   │   │   ├── BancoIngresosApplication.java
│   │   │   ├── controller/IngresosController.java
│   │   │   ├── service/IngresosService.java
│   │   │   ├── model/CuentaBancaria.java
│   │   │   ├── model/Ingreso.java
│   │   │   ├── dto/...
│   │   │   ├── repository/...
│   │   │   └── exception/...
│   │   └── resources/
│   │       ├── application.properties
│   │       └── messages.properties
│   └── test/
├── pom.xml
├── README.md
├── GUIA_EJECUCION.md
├── MAPEO_DETALLADO.md
├── ARQUITECTURA.md
├── EJEMPLOS_PRUEBA.md
├── postman_collection.json
└── target/ (generado al compilar)
```

---

## 🔗 Endpoints Principales

| Método | Endpoint | Descripción |
|--------|----------|-------------|
| POST | `/api/ingresos/procesar` | Procesa ingresos (flujo principal) |
| GET | `/api/ingresos/cuentas/{id}` | Obtiene cuenta por ID |
| GET | `/api/ingresos/cuentas/numero/{num}` | Obtiene cuenta por número |
| POST | `/api/ingresos/cuentas/{id}/agregar-ingreso` | Agrega ingreso a cuenta |

---

## 💾 Base de Datos

- **Tipo**: H2 (en memoria)
- **URL**: `jdbc:h2:mem:bancoingresosdb`
- **Consola**: `http://localhost:8080/banco-ingresos/h2-console`
- **Credenciales**: usuario `sa`, contraseña vacía

**Nota**: Los datos se pierden al reiniciar la aplicación.

---

## 📝 Ejemplo Completo de Una Sesión

```bash
# 1. Iniciar la aplicación
mvn spring-boot:run

# En OTRA TERMINAL:

# 2. Procesar ingresos
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/procesar?numeroCuenta=CUENTA123&titular=Juan" \
  -H "Content-Type: application/json" \
  -d '[{"importe": 1000.00, "concepto": "Salario"}]'

# Guardamos el ID retornado (ej: 1)

# 3. Ver los detalles de la cuenta
curl "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1"

# 4. Agregar otro ingreso
curl -X POST "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1/agregar-ingreso" \
  -H "Content-Type: application/json" \
  -d '{"importe": 500.00, "concepto": "Bono"}'

# 5. Ver el estado actualizado
curl "http://localhost:8080/banco-ingresos/api/ingresos/cuentas/1"
```

---

## 🎯 Próximos Pasos

1. Revisar la [GUIA_EJECUCION.md](GUIA_EJECUCION.md) para más detalles
2. Explorar los [EJEMPLOS_PRUEBA.md](EJEMPLOS_PRUEBA.md) para casos más complejos
3. Importar la colección [postman_collection.json](postman_collection.json) en Postman
4. Revisar el [MAPEO_DETALLADO.md](MAPEO_DETALLADO.md) para entender la traducción

---

**Última actualización**: 20 de febrero de 2026
