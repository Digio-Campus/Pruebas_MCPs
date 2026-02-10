# Makefile para TicTacToe Multi-Tablero con ncurses

# Compilador y flags
CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -pedantic
LDFLAGS = -lncurses

# Archivos fuente y objetos
SOURCES = main.cpp Board.cpp GameManager.cpp UI.cpp
OBJECTS = $(SOURCES:.cpp=.o)
EXECUTABLE = tictactoe

# Colores para output
COLOR_RESET = \033[0m
COLOR_GREEN = \033[32m
COLOR_YELLOW = \033[33m
COLOR_BLUE = \033[34m

# Target principal
all: $(EXECUTABLE)
	@echo "$(COLOR_GREEN)✓ Compilación completada exitosamente$(COLOR_RESET)"
	@echo "$(COLOR_BLUE)Ejecuta './$(EXECUTABLE)' para jugar$(COLOR_RESET)"

# Enlazar el ejecutable
$(EXECUTABLE): $(OBJECTS)
	@echo "$(COLOR_YELLOW)→ Enlazando $(EXECUTABLE)...$(COLOR_RESET)"
	$(CXX) $(OBJECTS) -o $(EXECUTABLE) $(LDFLAGS)

# Compilar archivos objeto
%.o: %.cpp
	@echo "$(COLOR_YELLOW)→ Compilando $<...$(COLOR_RESET)"
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Dependencias de archivos header
main.o: main.cpp UI.h GameManager.h
Board.o: Board.cpp Board.h
GameManager.o: GameManager.cpp GameManager.h Board.h
UI.o: UI.cpp UI.h GameManager.h Board.h

# Limpiar archivos compilados
clean:
	@echo "$(COLOR_YELLOW)→ Limpiando archivos...$(COLOR_RESET)"
	rm -f $(OBJECTS) $(EXECUTABLE)
	@echo "$(COLOR_GREEN)✓ Limpieza completada$(COLOR_RESET)"

# Recompilar todo desde cero
rebuild: clean all

# Ejecutar el programa
run: $(EXECUTABLE)
	@echo "$(COLOR_BLUE)→ Ejecutando $(EXECUTABLE)...$(COLOR_RESET)"
	./$(EXECUTABLE)

# Instalar dependencias (solo para referencia)
install-deps:
	@echo "$(COLOR_YELLOW)→ Instalando dependencias...$(COLOR_RESET)"
	@echo "En Ubuntu/Debian: sudo apt-get install libncurses5-dev libncursesw5-dev"
	@echo "En Fedora/RedHat: sudo dnf install ncurses-devel"
	@echo "En Arch Linux: sudo pacman -S ncurses"

# Ayuda
help:
	@echo "$(COLOR_BLUE)Makefile para TicTacToe Multi-Tablero$(COLOR_RESET)"
	@echo ""
	@echo "Targets disponibles:"
	@echo "  $(COLOR_GREEN)make$(COLOR_RESET)              - Compila el proyecto"
	@echo "  $(COLOR_GREEN)make run$(COLOR_RESET)          - Compila y ejecuta el programa"
	@echo "  $(COLOR_GREEN)make clean$(COLOR_RESET)        - Elimina archivos compilados"
	@echo "  $(COLOR_GREEN)make rebuild$(COLOR_RESET)      - Limpia y recompila todo"
	@echo "  $(COLOR_GREEN)make install-deps$(COLOR_RESET) - Muestra como instalar dependencias"
	@echo "  $(COLOR_GREEN)make help$(COLOR_RESET)         - Muestra esta ayuda"
	@echo ""
	@echo "Requisitos:"
	@echo "  - g++ con soporte C++11"
	@echo "  - biblioteca ncurses"

.PHONY: all clean rebuild run install-deps help
