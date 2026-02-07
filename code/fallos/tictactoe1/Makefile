# Makefile para TicTacToe con ncurses

# Compilador y flags
CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++11 -I./include
LDFLAGS = -lncurses

# Directorios
SRC_DIR = src
INC_DIR = include
OBJ_DIR = obj
BIN_DIR = bin

# Archivos fuente y objeto
SOURCES = $(wildcard $(SRC_DIR)/*.cpp)
OBJECTS = $(SOURCES:$(SRC_DIR)/%.cpp=$(OBJ_DIR)/%.o)

# Ejecutable
TARGET = $(BIN_DIR)/tictactoe

# Regla principal
all: directories $(TARGET)

# Crear directorios necesarios
directories:
	@mkdir -p $(OBJ_DIR) $(BIN_DIR)

# Compilar ejecutable
$(TARGET): $(OBJECTS)
	@echo "Enlazando $@..."
	@$(CXX) $(OBJECTS) -o $@ $(LDFLAGS)
	@echo "Compilación completada: $(TARGET)"

# Compilar archivos objeto
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@echo "Compilando $<..."
	@$(CXX) $(CXXFLAGS) -c $< -o $@

# Limpiar archivos compilados
clean:
	@echo "Limpiando archivos compilados..."
	@rm -rf $(OBJ_DIR) $(BIN_DIR)
	@echo "Limpieza completada"

# Ejecutar el juego
run: all
	@./$(TARGET)

# Información de ayuda
help:
	@echo "Makefile para TicTacToe"
	@echo ""
	@echo "Uso:"
	@echo "  make          - Compilar el proyecto"
	@echo "  make clean    - Limpiar archivos compilados"
	@echo "  make run      - Compilar y ejecutar"
	@echo "  make help     - Mostrar esta ayuda"

# Reglas especiales
.PHONY: all clean run help directories
