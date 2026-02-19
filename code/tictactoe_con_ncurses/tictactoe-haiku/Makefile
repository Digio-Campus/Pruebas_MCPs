CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -O2 -I./include
LDFLAGS := -lncurses

SRC_DIR := src
OBJ_DIR := obj
BIN_DIR := bin
TEST_DIR := tests

SOURCES := $(wildcard $(SRC_DIR)/*.cpp)
OBJECTS := $(SOURCES:$(SRC_DIR)/%.cpp=$(OBJ_DIR)/%.o)
TARGET := $(BIN_DIR)/tictactoe
TEST_TARGET := $(BIN_DIR)/test_tictactoe
TEST_SOURCES := $(wildcard $(TEST_DIR)/*.cpp)

# Default target
all: $(TARGET)

# Build executable
$(TARGET): $(OBJECTS)
	@mkdir -p $(BIN_DIR)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)
	@echo "Build successful: $(TARGET)"

# Compile object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Run the game
run: $(TARGET)
	./$(TARGET)

# Build and run tests
test: $(TEST_TARGET)
	./$(TEST_TARGET)

$(TEST_TARGET):
	@mkdir -p $(BIN_DIR)
	$(CXX) $(CXXFLAGS) -o $@ $(TEST_SOURCES) src/Board.cpp src/Settings.cpp
	@echo "Test build successful: $(TEST_TARGET)"

# Clean build artifacts
clean:
	@rm -rf $(OBJ_DIR) $(BIN_DIR)
	@echo "Clean complete"

# Help
help:
	@echo "Tic Tac Toe - ncurses Edition"
	@echo "Targets:"
	@echo "  all       - Build the project (default)"
	@echo "  run       - Build and run the game"
	@echo "  test      - Build and run unit tests"
	@echo "  clean     - Remove build artifacts"
	@echo "  help      - Show this help message"

.PHONY: all run test clean help

