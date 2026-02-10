#!/bin/bash
# Script de utilidad para el proyecto Tic-Tac-Toe ncurses

VERSION="1.0"
BINARY="./tictactoe"
SRCDIR="."

echo "╔════════════════════════════════════════════╗"
echo "║   Tic-Tac-Toe ncurses - Utilidad v$VERSION       ║"
echo "╚════════════════════════════════════════════╝"

case "${1:-help}" in
    build)
        echo "🔨 Compilando proyecto..."
        make clean
        make
        if [ -f "$BINARY" ]; then
            echo "✅ Compilación exitosa"
        else
            echo "❌ Error en compilación"
            exit 1
        fi
        ;;
    
    run)
        echo "🎮 Iniciando juego..."
        if [ ! -f "$BINARY" ]; then
            echo "⚠️  Binario no encontrado. Compilando..."
            make
        fi
        exec "$BINARY"
        ;;
    
    clean)
        echo "🧹 Limpiando archivos compilados..."
        make clean
        echo "✅ Limpieza completada"
        ;;
    
    rebuild)
        echo "🔄 Recompilando..."
        make rebuild
        echo "✅ Recompilación completada"
        ;;
    
    help|--help|-h)
        cat << 'EOF'
Uso: ./build.sh [comando]

Comandos:
  build      - Compilar el proyecto
  run        - Compilar (si es necesario) y ejecutar
  clean      - Limpiar archivos compilados
  rebuild    - Compilar desde cero
  help       - Mostrar esta ayuda

Ejemplos:
  ./build.sh build      # Compilar
  ./build.sh run        # Jugar
  ./build.sh rebuild    # Recompilar todo

Documentación:
  README.md    - Guía de inicio rápido
  EXAMPLES.md  - Ejemplos de uso detallados
  TECHNICAL.md - Documentación técnica
  FAQ.md       - Preguntas frecuentes
EOF
        ;;
    
    *)
        echo "❌ Comando desconocido: $1"
        echo "Usa './build.sh help' para más información"
        exit 1
        ;;
esac
