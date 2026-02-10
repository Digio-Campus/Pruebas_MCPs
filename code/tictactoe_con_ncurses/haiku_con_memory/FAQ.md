# FAQ - Tic-Tac-Toe ncurses

## Preguntas Frecuentes

### Compilación

**P: ¿Cómo compilo el proyecto?**
R: 
```bash
cd code/tictactoe-ncurses
make
```

**P: Recibo un error "ncurses not found"**
R: Instala las librerías de desarrollo de ncurses:
```bash
# Debian/Ubuntu
sudo apt-get install libncurses-dev

# Fedora
sudo dnf install ncurses-devel

# macOS
brew install ncurses
```

**P: ¿Puedo compilar con un compilador diferente a g++?**
R: Sí, abre Makefile y cambia `CXX = g++` a tu compilador preferido (clang++, etc.)

**P: ¿Qué versión de C++ se necesita?**
R: C++17 o superior. Si tienes una versión anterior, actualiza tu compilador.

---

### Ejecución

**P: ¿Cómo ejecuto el juego?**
R:
```bash
./tictactoe
# O
make run
```

**P: El programa se congela o no responde**
R: 
- Presiona Ctrl+C para terminar
- La terminal puede estar en modo raw; ejecuta `reset`
- Comprueba que tienes ncurses instalado correctamente

**P: La pantalla se ve corrupta**
R:
- Redimensiona la ventana del terminal
- Ejecuta `reset` para limpiar
- Aumenta el tamaño mínimo a 80x24 caracteres

**P: El ratón no funciona**
R:
- Algunos terminales no soportan eventos de ratón (p.ej., ssh)
- Usa el teclado en su lugar
- Prueba con `xterm`, `urxvt` o `GNOME Terminal`

---

### Gameplay

**P: ¿Cuál es la diferencia entre los 3 modos?**
R:
- **0 Jugadores**: Tableros se llenan automáticamente. Mira cómo juega la máquina.
- **1 Jugador**: Controlas X y O manualmente, alternando. Útil para practicar.
- **2 Jugadores**: Tú eres O, IA es X. Intenta ganar contra la máquina.

**P: ¿Por qué alterna automáticamente a O en modo 1 jugador?**
R: Así se respeta la regla de juego: X → O → X → O. Presiona SPACE para colocar cualquiera.

**P: ¿Puedo jugar 2 personas humanas en el mismo teclado?**
R: Sí, con el modo "1 Jugador" ambos pueden compartir teclado y alternar turnos manualmente.

**P: ¿Cómo cambio entre tableros?**
R: 
- Usa PgUp (página anterior) y PgDn (página siguiente)
- O configura 1 solo tablero si prefieres

**P: ¿Se afectan los tableros entre sí?**
R: No, cada tablero es completamente independiente. Su estado, turno y victoria son propios.

**P: ¿Cómo reinicio un tablero?**
R: Presiona `R` mientras estés en ese tablero.

**P: ¿Cuál es la máxima cantidad de tableros?**
R: 9 tableros, pero depende del tamaño de tu terminal.

---

### Controles

**P: ¿Cuáles son todas las teclas?**
R:
| Tecla | Acción |
|-------|--------|
| Flechas | Mover |
| SPACE/ENTER | Colocar marca |
| PgUp/PgDn | Cambiar tablero |
| R | Reiniciar tablero actual |
| Q | Salir |
| H | Ayuda |
| Ratón | Clic en casillas/menús |

**P: ¿Puedo jugar solo con ratón?**
R: Parcialmente. Puedes hacer clic en casillas, pero la navegación de menús es mejor con teclado.

**P: ¿El ratón funciona en SSH?**
R: No, SSH no transmite eventos de ratón. Usa solo teclado en SSH.

---

### Modos

**P: ¿Cuál es el mejor modo para empezar?**
R: Comienza con "2 Jugadores" y "1 tablero" para jugar contra la IA.

**P: ¿Puedo ver a la IA pensar?**
R: La IA juega instantáneamente. Si quieres verla "pensar", modifica el código para añadir delays.

**P: ¿Cómo mejoro mi estrategia contra la IA?**
R: La IA actual es aleatoria. Para verdadera estrategia, implementa un algoritmo Minimax (ver TECHNICAL.md).

**P: ¿Hay un modo de práctica?**
R: El modo "1 Jugador" es ideal para práctica: controlas ambos lados.

---

### Interfaz

**P: ¿Puedo cambiar los colores?**
R: Sí, edita `init_pair()` en UI.cpp:
```cpp
init_pair(3, COLOR_YELLOW, COLOR_BLACK);  // Cambiar X a amarillo
```

**P: ¿Puedo usar la interfaz en luz y oscuridad?**
R: Sí, el contraste (blanco sobre negro) funciona en ambos modos.

**P: ¿Qué pasa si redimensiono la ventana durante el juego?**
R: El layout se adaptará automáticamente al siguiente redibujado.

**P: ¿Cómo hago la pantalla más grande/pequeña?**
R: Redimensiona la ventana del terminal. El programa se adapta automáticamente.

---

### Técnico

**P: ¿Cuánta memoria usa?**
R: Muy poco (~1 MB). Con 9 tableros, cada uno ocupa ~100 bytes.

**P: ¿Es thread-safe?**
R: No. Es un programa secuencial simple. Para multiplayer necesitarías refactoring.

**P: ¿Puedo modificar el código?**
R: Claro, es código educativo. Ver TECHNICAL.md para arquitectura.

**P: ¿Por qué C++ en lugar de Python?**
R: Para demostrar principios sólidos: OOP, compilación, gestión de memoria, ncurses.

**P: ¿Dónde está la IA?**
R: Ver `Game::makeAIMove()` en Game.cpp. Es simple (elige celda aleatoria).

---

### Solución de Problemas

**P: Se ve un símbolo raro en los bordes**
R: Tu terminal no soporta caracteres ACS (ASCII graphics set). Usa una terminal moderna.

**P: El juego no responde a mi entrada**
R: 
- Asegúrate de tener `nodelay()` activado
- Presiona una tecla claramente
- Reinicia el programa

**P: Un movimiento no se registró**
R:
- Verifica que no golpeaste una casilla ocupada
- En modo 2 jugadores, espera a que IA termine
- Comprueba el turno actual (TURN: X o TURN: O)

**P: ¿Por qué la terminal queda corrupta al salir?**
R: Ejecuta `reset` o `stty sane` para limpiar.

**P: ¿Cómo veo los logs de debug?**
R: Redirige a archivo: `./tictactoe > game.log 2>&1`

---

### Características

**P: ¿Hay sonido?**
R: No, pero puedes añadirlo usando `system("beep")` o librerías de audio.

**P: ¿Hay guardado de partida?**
R: No actualmente, pero podrías añadir serialización.

**P: ¿Hay estadísticas o puntuación?**
R: No, pero el código está listo para extensión.

**P: ¿Funciona en Windows?**
R: No sin Windows Subsystem for Linux (WSL) o Cygwin. Prueba WSL2.

---

### Contribuciones

**P: ¿Puedo mejorar este proyecto?**
R: Sí! Ideas de mejora:
- Algoritmo Minimax para IA inteligente
- Sistema de puntuación
- Guardado de partidas
- Temas personalizables
- Modo networked/multiplayer

**P: ¿Cómo reporto un bug?**
R: Crea una issue en GitHub o describe exactamente qué pasó.

---

### Licencia y Uso

**P: ¿Puedo usar esto en mis proyectos?**
R: Sí, es código educativo. Siéntete libre de copiar, modificar y distribuir.

**P: ¿Debo dar crédito?**
R: No es requerido, pero es apreciado.

**P: ¿Puedo vender un juego basado en esto?**
R: Sí, aunque es mejor mejorar significativamente primero.

---

### Rendimiento

**P: ¿Es rápido?**
R: Sí, muy rápido. Compilado en C++ con optimizaciones -O2.

**P: ¿Funciona en máquinas lentasencia?**
R: Sí, usa muy poco CPU. Incluso en Raspberry Pi funcionaría.

**P: ¿Puedo jugar 100 tableros simultáneamente?**
R: No, la pantalla no cabe. Máximo 9 por limitaciones prácticas, pero el código podría manejar más.

---

¿No encuentras tu pregunta? ¡Abre un issue o contacta al autor!
