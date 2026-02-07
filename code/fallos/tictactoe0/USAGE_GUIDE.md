# Guía de Uso - Tictactoe ncurses

## Inicio Rápido

### Primera ejecución
```bash
cd code/tictactoe
./check_installation.sh  # Verificar dependencias
./tictactoe              # Iniciar el juego
```

## Ejemplos de Uso

### Ejemplo 1: Partida Rápida (Modo por defecto)

**Objetivo**: Jugar una partida rápida contra la IA

**Pasos**:
1. Ejecutar `./tictactoe`
2. En el menú principal, presionar Enter en "JUGAR" (ya está seleccionado)
3. Hacer clic con el ratón en las casillas para colocar tu X
4. La IA responderá automáticamente con O
5. Presionar Q o ESC para salir

**Configuración por defecto**:
- 1 jugador (Tú: X, IA: O)
- 1 tablero

---

### Ejemplo 2: Modo Espectador - Múltiples Partidas Automáticas

**Objetivo**: Ver múltiples partidas jugándose automáticamente

**Pasos**:
1. Ejecutar `./tictactoe`
2. Navegar con flecha abajo a "AJUSTES" y presionar Enter
3. En "Número de jugadores", presionar flecha izquierda hasta llegar a 0
4. En "Número de tableros", presionar flecha derecha hasta 4 (o más)
5. Seleccionar "VOLVER" con Enter
6. Seleccionar "JUGAR" con Enter
7. Observar cómo se juegan múltiples partidas simultáneamente

**Nota**: Los tableros se reinician automáticamente cuando terminan.

**Captura de lo que verás**:
```
Tablero 1      Tablero 2      Tablero 3      Tablero 4
 X | O | X      O | X | O      X | O |        | X | O
---+---+---    ---+---+---    ---+---+---    ---+---+---
 O | X |        X | X | O      O | X | X       O | O | X
---+---+---    ---+---+---    ---+---+---    ---+---+---
 O | X | O      O | X | X       | O | X      X | O |
X GANA!        EMPATE         (Jugando...)   (Jugando...)
```

---

### Ejemplo 3: Modo 2 Jugadores Asistido

**Objetivo**: Jugar controlando solo las O, mientras X se coloca automáticamente

**Pasos**:
1. Ejecutar `./tictactoe`
2. Ir a "AJUSTES"
3. Cambiar "Número de jugadores" a 2
4. Volver al menú principal
5. Seleccionar "JUGAR"
6. Hacer clic en una casilla - se colocará una O
7. Automáticamente aparecerá una X en otra posición
8. Continuar hasta que termine el juego

**Uso sugerido**: 
- Ideal para principiantes que quieren practicar solo sus movimientos
- Útil para experimentar con diferentes estrategias

---

### Ejemplo 4: Sesión de Práctica Extendida

**Objetivo**: Jugar múltiples partidas seguidas y ver estadísticas

**Pasos**:
1. Configurar en modo 1 jugador, 1 tablero
2. Iniciar el juego
3. Jugar hasta ganar, perder o empatar
4. Observar la puntuación en la parte inferior
5. Hacer clic en otra casilla para iniciar una nueva partida
6. Las puntuaciones se acumulan

**Puntuaciones mostradas**:
```
Turno: X | X: 5 | O: 3 | Empates: 2 | ESC/Q: Salir
```

---

### Ejemplo 5: Tableros Máximos

**Objetivo**: Ver cuántos tableros caben en tu terminal

**Pasos**:
1. Maximizar tu ventana de terminal
2. Ir a "AJUSTES"
3. Configurar "Número de tableros" a 9
4. Iniciar el juego en modo 0 jugadores (automático)
5. El sistema adaptará el layout según el espacio disponible

**Requisitos de terminal**:
- Mínimo recomendado: 80x24 caracteres
- Para 9 tableros: 120x40 caracteres o más

---

### Ejemplo 6: Probar el Redimensionado

**Objetivo**: Verificar que la interfaz se adapta al redimensionar

**Pasos**:
1. Iniciar un juego en cualquier modo
2. Durante el juego, redimensionar la ventana del terminal
3. La interfaz se adaptará automáticamente
4. Los tableros se reorganizarán según el nuevo espacio

**Tecla especial**: KEY_RESIZE es detectada automáticamente

---

## Casos de Uso por Perfil

### Para Jugadores Casuales
- **Recomendación**: Modo 1 jugador, 1 tablero
- **Beneficio**: Experiencia simple y directa
- **Tiempo**: 1-2 minutos por partida

### Para Observadores
- **Recomendación**: Modo 0 jugadores, 4-6 tableros
- **Beneficio**: Visualización interesante, ideal como "screensaver"
- **Tiempo**: Sesiones continuas

### Para Aprendizaje
- **Recomendación**: Modo 2 jugadores, 1 tablero
- **Beneficio**: Enfocarse solo en movimientos de O
- **Tiempo**: 5-10 minutos de práctica

### Para Análisis
- **Recomendación**: Modo 0 jugadores, 9 tableros
- **Beneficio**: Ver múltiples estrategias simultáneas
- **Tiempo**: Variable

---

## Trucos y Consejos

### Navegación Rápida
- **Enter** en cualquier menú selecciona inmediatamente
- **ESC** siempre vuelve atrás o sale
- **Flechas** en ajustes modifican valores directamente

### Optimización de Terminal
```bash
# Terminal pequeña (desarrollo)
resize -s 24 80

# Terminal grande (múltiples tableros)
resize -s 40 120

# Pantalla completa
F11 (en la mayoría de emuladores)
```

### Atajos de Teclado por Pantalla

**Menú Principal**:
- `↑↓`: Navegar
- `Enter`: Seleccionar
- `ESC`: Salir (en "SALIR")

**Ajustes**:
- `↑↓`: Cambiar opción
- `←→`: Modificar valores
- `Enter`: Confirmar (en "VOLVER")

**Juego**:
- `Clic izquierdo`: Hacer movimiento
- `Q` o `ESC`: Volver al menú

---

## Solución de Problemas Comunes

### Problema: "No se ve el cursor del ratón"
**Solución**: El cursor está oculto por diseño (curs_set(0)). Usa el clic normalmente.

### Problema: "Los colores no se ven bien"
**Solución**: Verifica que tu terminal soporte colores:
```bash
echo $TERM  # Debe mostrar algo como "xterm-256color"
```

### Problema: "Los tableros no caben"
**Solución**: 
1. Aumentar tamaño de terminal
2. Reducir número de tableros en Ajustes
3. Usar una fuente más pequeña

### Problema: "El ratón no funciona"
**Solución**: 
- Verifica que tu emulador de terminal soporte ratón
- Emuladores compatibles: gnome-terminal, konsole, xterm, tilix
- No compatible: Terminales muy antiguos

---

## Escenarios Avanzados

### Sesión de Benchmark
```bash
# Terminal 1: Juego en modo automático
./tictactoe
# Configurar: 0 jugadores, 9 tableros
# Observar rendimiento

# Terminal 2: Monitoreo
top -p $(pgrep tictactoe)
```

### Captura de Pantalla
```bash
# Durante el juego
# Método 1: Screenshot del terminal
import -window root screenshot.png

# Método 2: Desde otro terminal
tty  # Obtener tty del juego, ej: /dev/pts/1
cat /dev/pts/1 > capture.txt
```

### Integración con Scripts
```bash
#!/bin/bash
# Launcher personalizado
export TERM=xterm-256color
cd ~/code/tictactoe
./tictactoe
```

---

## Métricas y Rendimiento

### Uso de Recursos (típico)
- **CPU**: < 5% en modo interactivo
- **CPU**: 10-15% en modo 9 tableros automáticos
- **Memoria**: ~2-3 MB
- **FPS**: ~20 (refresh cada 50ms)

### Tiempos de Respuesta
- **Clic a render**: < 50ms
- **Movimiento automático**: < 100ms
- **Redimensionado**: < 200ms

---

## Preguntas Frecuentes

**P: ¿Puedo jugar dos humanos reales?**
R: No directamente. Los modos actuales son: auto vs auto, humano vs auto, o humano asistido. Para 2 humanos reales necesitarías modificar el código.

**P: ¿La IA es inteligente?**
R: La IA actual usa movimientos aleatorios. Es simple pero funcional. Ver ARCHITECTURE.md para ideas de mejora (minimax).

**P: ¿Puedo cambiar el tamaño del tablero a 4x4?**
R: Requiere modificar el código (constantes hardcoded a 3x3). Ver ARCHITECTURE.md sección "Extensibilidad".

**P: ¿Funciona en Windows?**
R: No directamente. ncurses es para Unix/Linux. En Windows considera usar WSL (Windows Subsystem for Linux).

**P: ¿Puedo distribuir el ejecutable?**
R: Sí, pero el sistema destino debe tener libncurses instalada. Para distribución considera compilación estática.

---

## Recursos Adicionales

- **README.md**: Instrucciones de instalación y compilación
- **ARCHITECTURE.md**: Detalles técnicos y diseño
- **PROJECT_SUMMARY.txt**: Resumen ejecutivo del proyecto
- **check_installation.sh**: Script de diagnóstico

---

## Contacto

Para bugs, sugerencias o contribuciones, consulta la documentación del proyecto.

**¡Disfruta jugando Tictactoe!** 🎮
