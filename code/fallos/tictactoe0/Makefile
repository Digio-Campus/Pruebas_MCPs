# Makefile para Tictactoe con ncurses

CXX = g++
CXXFLAGS = -std=c++14 -Wall -Wextra -I./include
LDFLAGS = -lncurses -lpthread

TARGET = tictactoe
SRC_DIR = src
OBJ_DIR = obj
SOURCES = $(wildcard $(SRC_DIR)/*.cpp)
OBJECTS = $(SOURCES:$(SRC_DIR)/%.cpp=$(OBJ_DIR)/%.o)

# Colores para output
GREEN = \033[0;32m
NC = \033[0m

all: $(TARGET)
	@echo "$(GREEN)Compilacion completada. Ejecuta con: ./$(TARGET)$(NC)"

$(TARGET): $(OBJECTS)
	@echo "$(GREEN)Enlazando...$(NC)"
	$(CXX) $(OBJECTS) -o $(TARGET) $(LDFLAGS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	@echo "Compilando $<..."
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

clean:
	@echo "Limpiando archivos generados..."
	rm -rf $(OBJ_DIR) $(TARGET)
	@echo "$(GREEN)Limpieza completada$(NC)"

run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
