# Índice del Proyecto Tictactoe

Este archivo sirve como punto de entrada rápido a toda la documentación del proyecto.

## 📚 Documentación Principal

### Para empezar:
1. **[README.md](README.md)** - **EMPIEZA AQUÍ** 
   - Instalación de dependencias
   - Compilación
   - Ejecución
   - Controles básicos

### Para aprender a usar:
2. **[USAGE_GUIDE.md](USAGE_GUIDE.md)** - Guía práctica
   - 6 ejemplos de uso paso a paso
   - Casos de uso por perfil de usuario
   - Trucos y consejos
   - Solución de problemas
   - Preguntas frecuentes

### Para entender el diseño:
3. **[ARCHITECTURE.md](ARCHITECTURE.md)** - Arquitectura técnica
   - Diagrama de componentes
   - Patrones de diseño
   - Flujo de ejecución
   - Estructuras de datos
   - Guías de extensibilidad

### Para verificar:
4. **[CHECKLIST.md](CHECKLIST.md)** - Lista de verificación completa
   - Todas las funcionalidades implementadas
   - Testing manual
   - Calidad del código
   - Estadísticas finales

### Resumen rápido:
5. **[PROJECT_SUMMARY.txt](PROJECT_SUMMARY.txt)** - Vista ejecutiva
   - Resumen de características
   - Estadísticas
   - Comandos rápidos
   - Estructura del proyecto

## 🛠️ Herramientas

- **[check_installation.sh](check_installation.sh)** - Script de diagnóstico
  ```bash
  ./check_installation.sh
  ```
  Verifica que todas las dependencias estén instaladas y que el proyecto compile correctamente.

- **[Makefile](Makefile)** - Sistema de compilación
  ```bash
  make        # Compilar
  make run    # Compilar y ejecutar
  make clean  # Limpiar archivos generados
  ```

## 📁 Código Fuente

### Headers (include/)
Define las interfaces de todas las clases:
- **board.h** - Tablero de Tictactoe 3x3
- **game.h** - Gestión del juego y múltiples tableros
- **menu.h** - Sistema de menús
- **player.h** - Jugadores (humano/automático)
- **ui.h** - Interfaz con ncurses

### Implementaciones (src/)
Contiene toda la lógica:
- **board.cpp** - Lógica del tablero, detección de victorias
- **game.cpp** - Control de flujo, turnos, puntuaciones
- **menu.cpp** - Navegación de menús, ajustes
- **player.cpp** - Movimientos automáticos
- **ui.cpp** - Renderizado, eventos de ratón/teclado
- **main.cpp** - Punto de entrada, loop principal

## 🎮 Inicio Rápido (TL;DR)

```bash
# 1. Verificar instalación
./check_installation.sh

# 2. Compilar (si no está compilado)
make

# 3. Ejecutar
./tictactoe

# 4. Jugar
# - Usa flechas para navegar menús
# - Haz clic con el ratón en las casillas
# - Presiona Q o ESC para salir
```

## 📊 Estadísticas del Proyecto

- **Código**: 981 líneas (C++)
- **Documentación**: 1,190+ líneas
- **Clases**: 5 (Board, Game, Menu, Player, UI)
- **Modos de juego**: 3 (0, 1, 2 jugadores)
- **Tableros simultáneos**: 1-9 configurables
- **Tamaño ejecutable**: 127KB

## 🔍 Búsqueda Rápida

### Quiero saber cómo...
- **Compilar**: Ver [README.md § Compilación](README.md#compilación)
- **Jugar**: Ver [USAGE_GUIDE.md § Inicio Rápido](USAGE_GUIDE.md#inicio-rápido)
- **Configurar modos**: Ver [USAGE_GUIDE.md § Ejemplos](USAGE_GUIDE.md#ejemplos-de-uso)
- **Extender el código**: Ver [ARCHITECTURE.md § Extensibilidad](ARCHITECTURE.md#extensibilidad)
- **Resolver problemas**: Ver [README.md § Solución de Problemas](README.md#solución-de-problemas)

### Tengo un problema...
- **No compila**: [README.md § Solución de Problemas](README.md#solución-de-problemas)
- **El ratón no funciona**: [USAGE_GUIDE.md § Problemas Comunes](USAGE_GUIDE.md#problema-el-ratón-no-funciona)
- **Pantalla muy pequeña**: [USAGE_GUIDE.md § Optimización](USAGE_GUIDE.md#optimización-de-terminal)

### Quiero modificar...
- **Tamaño del tablero**: [ARCHITECTURE.md § Extensibilidad](ARCHITECTURE.md#cambiar-tamaño-de-tablero)
- **IA más inteligente**: [ARCHITECTURE.md § Extensibilidad](ARCHITECTURE.md#agregar-nueva-estrategia-de-ia)
- **Nuevos modos**: [ARCHITECTURE.md § Extensibilidad](ARCHITECTURE.md#agregar-nuevos-modos-de-juego)

## 🎯 Rutas de Lectura Recomendadas

### Para usuarios finales:
1. README.md (secciones: Requisitos, Compilación, Ejecución, Controles)
2. USAGE_GUIDE.md (sección: Ejemplos de Uso)
3. check_installation.sh (ejecutar para verificar)

### Para desarrolladores:
1. README.md (completo)
2. ARCHITECTURE.md (completo)
3. Código fuente (empezar por main.cpp → game.cpp → ui.cpp)
4. CHECKLIST.md (para verificar cobertura)

### Para revisores:
1. PROJECT_SUMMARY.txt (vista general)
2. CHECKLIST.md (verificación de requisitos)
3. ARCHITECTURE.md (diseño técnico)
4. Código fuente seleccionado

## 📞 Soporte

- **Instalación**: README.md + check_installation.sh
- **Uso**: USAGE_GUIDE.md
- **Desarrollo**: ARCHITECTURE.md
- **Verificación**: CHECKLIST.md

## ✅ Estado del Proyecto

**Última actualización**: 2026-02-05  
**Estado**: ✓ Completo y Funcional  
**Versión**: 1.0  
**Compilador testado**: g++ 14.2.0  
**SO testado**: Ubuntu Linux  

---

**¡Disfruta del juego!** 🎮

Para cualquier duda, consulta primero el documento apropiado usando este índice.
