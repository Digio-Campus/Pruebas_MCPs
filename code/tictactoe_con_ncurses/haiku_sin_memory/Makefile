# Makefile para Tic Tac Toe
# Compilación con g++ y ncurses

CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++11 -O2
LDFLAGS = -lncurses

# Archivos fuente
SOURCES = main.cpp board.cpp game.cpp ui.cpp
HEADERS = board.h game.h ui.h
OBJECTS = $(SOURCES:.cpp=.o)
TARGET = tictactoe

# Reglas
.PHONY: all clean run help

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)
	@echo "Compilación completada: $(TARGET)"

%.o: %.cpp $(HEADERS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(TARGET)
	@echo "Archivos limpios"

run: $(TARGET)
	./$(TARGET)

help:
	@echo "Tic Tac Toe - Makefile"
	@echo ""
	@echo "Objetivos disponibles:"
	@echo "  make         - Compila el proyecto"
	@echo "  make run     - Compila y ejecuta el programa"
	@echo "  make clean   - Limpia los archivos objeto y ejecutable"
	@echo "  make help    - Muestra esta ayuda"
	@echo ""
	@echo "Requisitos:"
	@echo "  - g++ (compilador C++)"
	@echo "  - ncurses-dev (librería ncurses)"
