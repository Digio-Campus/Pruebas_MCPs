CXX := g++
CXXFLAGS := -Wall -Wextra -std=c++17 -Iinclude
LDFLAGS := -lncurses

SRC_DIR := src
OBJ_DIR := obj
BIN_DIR := bin

SRCS := $(wildcard $(SRC_DIR)/*.cpp)
OBJS := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(SRCS))

TARGET := $(BIN_DIR)/tictactoe

.PHONY: all clean run help

all: $(TARGET)

$(TARGET): $(OBJS) | $(BIN_DIR)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

run: all
	./$(TARGET)

clean:
	@rm -rf $(OBJ_DIR) $(BIN_DIR)

help:
	@echo "Targets:";
	@echo "  make        - compilar";
	@echo "  make run    - compilar y ejecutar";
	@echo "  make clean  - limpiar";
