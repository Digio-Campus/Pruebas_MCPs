#!/bin/bash
# Script para verificar que el proyecto está correctamente instalado y configurado

echo "================================"
echo "Verificación del proyecto Tictactoe"
echo "================================"
echo ""

# Verificar que estamos en el directorio correcto
if [ ! -f "Makefile" ]; then
    echo "❌ Error: No se encontró el Makefile. Ejecuta este script desde code/tictactoe/"
    exit 1
fi

echo "✓ Directorio correcto detectado"

# Verificar g++
if ! command -v g++ &> /dev/null; then
    echo "❌ Error: g++ no está instalado"
    echo "   Instala con: sudo apt-get install build-essential"
    exit 1
fi

echo "✓ g++ encontrado: $(g++ --version | head -n1)"

# Verificar ncurses
if ! ldconfig -p | grep -q libncurses; then
    echo "❌ Error: libncurses no está instalada"
    echo "   Instala con: sudo apt-get install libncurses5-dev libncursesw5-dev"
    exit 1
fi

echo "✓ libncurses encontrada"

# Intentar compilar
echo ""
echo "Intentando compilar el proyecto..."
make clean > /dev/null 2>&1
if make > /dev/null 2>&1; then
    echo "✓ Compilación exitosa"
else
    echo "❌ Error en la compilación"
    echo "   Ejecuta 'make' para ver los errores detallados"
    exit 1
fi

# Verificar ejecutable
if [ -x "./tictactoe" ]; then
    echo "✓ Ejecutable creado correctamente"
    ls -lh tictactoe | awk '{print "   Tamaño:", $5}'
else
    echo "❌ Error: No se pudo crear el ejecutable"
    exit 1
fi

echo ""
echo "================================"
echo "✓ Todas las verificaciones pasaron"
echo "================================"
echo ""
echo "Para ejecutar el juego:"
echo "  ./tictactoe"
echo ""
echo "O usa:"
echo "  make run"
echo ""
