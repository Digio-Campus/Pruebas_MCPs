# Índice de Documentación - TIC TAC TOE NCURSES

Bienvenido al proyecto Tic Tac Toe en C++ con ncurses. Este documento te guiará a través de toda la documentación disponible.

## 🚀 Inicio Rápido

**¿Solo quieres ejecutar el juego?**

```bash
cd code/tictactoe-ncurses
make run
```

Para más detalles, ve a [INICIO_RAPIDO.txt](INICIO_RAPIDO.txt)

---

## 📚 Documentación por Propósito

### 🎮 Quiero JUGAR
1. Lee [INICIO_RAPIDO.txt](INICIO_RAPIDO.txt) - Comandos básicos en 2 minutos
2. Lee [README.md](README.md) sección "Controles" - Aprende los controles

### 💻 Quiero COMPILAR
1. Lee [INSTRUCCIONES.md](INSTRUCCIONES.md) - Guía paso a paso
2. Verifica que tienes libncurses5-dev instalado
3. Ejecuta `make` en el directorio del proyecto

### 📖 Quiero APRENDER
1. Lee [README.md](README.md) - Documentación completa
2. Lee [RESUMEN_PROYECTO.md](RESUMEN_PROYECTO.md) - Visión arquitectónica
3. Explora el código en `src/` y `include/`

### 🐛 Tengo PROBLEMAS
1. Lee [INSTRUCCIONES.md](INSTRUCCIONES.md) sección "Solución de Problemas"
2. Intenta `make clean && make`
3. Verifica requisitos con `gcc --version` y `pkg-config --list-all | grep ncurses`

### ✅ Quiero VERIFICAR TODO
1. Lee [VERIFICACION.md](VERIFICACION.md) - Checklist completo de requisitos
2. Ejecuta `make test` - Verifica que los tests pasen

---

## 📄 Archivos de Documentación

| Archivo | Propósito | Público/Técnico | Tiempo |
|---------|-----------|-----------------|--------|
| [INICIO_RAPIDO.txt](INICIO_RAPIDO.txt) | Comandos y controles | Público | 2 min |
| [README.md](README.md) | Documentación completa | Público | 15 min |
| [INSTRUCCIONES.md](INSTRUCCIONES.md) | Compilación y requisitos | Técnico | 10 min |
| [EJEMPLOS.md](EJEMPLOS.md) | 14 ejemplos prácticos | Público | 20 min |
| [RESUMEN_PROYECTO.md](RESUMEN_PROYECTO.md) | Arquitectura y diseño | Técnico | 15 min |
| [VERIFICACION.md](VERIFICACION.md) | Checklist de requisitos | Técnico | 10 min |
| [INDEX.md](INDEX.md) | Este archivo | Público | 5 min |

---

## 📁 Estructura del Proyecto

```
code/tictactoe-ncurses/
├── include/                 # Headers del proyecto
│   ├── Board.h             # Tablero individual
│   ├── Game.h              # Controlador del juego
│   ├── Menu.h              # Sistema de menús
│   ├── Settings.h          # Configuración
│   └── UI.h                # Interfaz con ncurses
│
├── src/                     # Implementación
│   ├── Board.cpp
│   ├── Game.cpp
│   ├── Menu.cpp
│   ├── Settings.cpp
│   ├── UI.cpp
│   └── main.cpp
│
├── tests/                   # Tests unitarios
│   └── test_main.cpp       # 13 tests
│
├── bin/                     # Ejecutables (generado)
│   ├── tictactoe           # Juego compilado
│   └── test_tictactoe      # Tests compilados
│
├── obj/                     # Objetos (generado)
│
├── Makefile                # Script de compilación
├── INDEX.md                # Este archivo
├── INICIO_RAPIDO.txt       # Guía rápida
├── README.md               # Documentación principal
├── INSTRUCCIONES.md        # Guía de compilación
├── EJEMPLOS.md             # Ejemplos de uso
├── RESUMEN_PROYECTO.md     # Visión general
└── VERIFICACION.md         # Checklist
```

---

## 🎯 Preguntas Frecuentes

### P: ¿Cómo inicio el juego?
R: `cd code/tictactoe-ncurses && make run`

### P: ¿Cuáles son los requisitos?
R: Linux, g++ 7.0+, libncurses5-dev, make. Ver [INSTRUCCIONES.md](INSTRUCCIONES.md)

### P: ¿Cómo cambio el número de jugadores?
R: En el menú → Settings → Number of Players. Ver [INICIO_RAPIDO.txt](INICIO_RAPIDO.txt)

### P: ¿Puedo jugar con múltiples tableros?
R: Sí, 1-9 tableros. Configure en Settings → Number of Boards

### P: ¿Funciona el ratón?
R: Sí, la mayoría de emuladores de terminal. Si no, usa teclado.

### P: ¿Hay tests?
R: Sí, 13 tests unitarios. Ejecuta `make test`

### P: ¿Puedo ver ejemplos?
R: Sí, lee [EJEMPLOS.md](EJEMPLOS.md) con 14 ejemplos prácticos

---

## 🔑 Palabras Clave

- **TIC TAC TOE**: Juego clásico de 3x3
- **NCURSES**: Librería de interfaz terminal
- **C++17**: Estándar moderno del lenguaje
- **MÚLTIPLES TABLEROS**: 1-9 tableros simultáneos
- **3 MODOS DE JUEGO**: Auto (0), Manual (1), vs CPU (2)
- **MODULAR**: Separación clara entre componentes

---

## 🚀 Próximos Pasos

1. **Principiante**: Leer [INICIO_RAPIDO.txt](INICIO_RAPIDO.txt) → Ejecutar `make run`
2. **Desarrollador**: Leer [INSTRUCCIONES.md](INSTRUCCIONES.md) → Compilar código
3. **Técnico**: Leer [RESUMEN_PROYECTO.md](RESUMEN_PROYECTO.md) → Explorar `src/`
4. **Verificador**: Leer [VERIFICACION.md](VERIFICACION.md) → Ejecutar `make test`

---

## 📞 Soporte

Si tienes problemas:

1. Consulta [INSTRUCCIONES.md](INSTRUCCIONES.md) sección "Solución de Problemas"
2. Verifica los requisitos: `gcc --version` y `pkg-config --cflags --libs ncurses`
3. Intenta recompilar: `make clean && make`
4. Lee [EJEMPLOS.md](EJEMPLOS.md) para ver casos de uso similares

---

## ✅ Estado del Proyecto

- ✅ Completado
- ✅ Compilado sin errores
- ✅ Tests 13/13 PASSED
- ✅ Documentación completa
- ✅ Listo para distribuir

---

**Última actualización**: Febrero 2026  
**Versión**: 1.0  
**Licencia**: Proyecto educativo  

---

*Para comenzar ahora, ejecuta: `cd code/tictactoe-ncurses && make run`*
