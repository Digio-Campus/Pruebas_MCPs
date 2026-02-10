#!/bin/bash

# Script de prueba rápida para TicTacToe Multi-Tablero
# Este script verifica que todo esté correcto antes de ejecutar

echo "============================================"
echo "  TicTacToe Multi-Tablero - Test Script"
echo "============================================"
echo ""

# Verificar que estamos en el directorio correcto
if [ ! -f "Makefile" ]; then
    echo "❌ Error: No se encuentra Makefile"
    echo "   Ejecuta este script desde el directorio tictactoe/"
    exit 1
fi

# Verificar dependencias
echo "📋 Verificando dependencias..."

if ! command -v g++ &> /dev/null; then
    echo "❌ g++ no está instalado"
    echo "   Instalar con: sudo apt-get install g++"
    exit 1
fi
echo "✓ g++ encontrado: $(g++ --version | head -n1)"

if ! ldconfig -p | grep -q libncurses; then
    echo "❌ libncurses no está instalada"
    echo "   Instalar con: sudo apt-get install libncurses5-dev"
    exit 1
fi
echo "✓ ncurses encontrada"

echo ""
echo "🔨 Compilando proyecto..."
make clean > /dev/null 2>&1
if make; then
    echo "✓ Compilación exitosa"
else
    echo "❌ Error en la compilación"
    exit 1
fi

# Verificar que el ejecutable existe
if [ ! -f "tictactoe" ]; then
    echo "❌ El ejecutable no fue generado"
    exit 1
fi
echo "✓ Ejecutable generado: ./tictactoe"

# Verificar permisos de ejecución
if [ ! -x "tictactoe" ]; then
    chmod +x tictactoe
    echo "✓ Permisos de ejecución añadidos"
fi

# Mostrar información del ejecutable
SIZE=$(du -h tictactoe | cut -f1)
echo "✓ Tamaño del ejecutable: $SIZE"

echo ""
echo "📊 Estadísticas del proyecto:"
echo "   - Archivos fuente: $(ls -1 *.cpp *.h 2>/dev/null | wc -l)"
echo "   - Líneas de código: $(cat *.cpp *.h 2>/dev/null | wc -l)"
echo "   - Archivos de documentación: $(ls -1 *.md 2>/dev/null | wc -l)"

echo ""
echo "============================================"
echo "  ✅ Todas las verificaciones pasaron"
echo "============================================"
echo ""
echo "Para ejecutar el juego:"
echo "   ./tictactoe"
echo ""
echo "o"
echo ""
echo "   make run"
echo ""
echo "Controles básicos:"
echo "   - Flechas: Navegar"
echo "   - ENTER: Seleccionar/Jugar"
echo "   - TAB: Cambiar tablero"
echo "   - ESC/Q: Salir"
echo "   - Ratón: Clic para jugar"
echo ""
echo "¡Que disfrutes del juego!"
echo ""
