CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -O2 -Iinclude
LDFLAGS = -lncurses

SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin
TEST_DIR = tests

TARGET = $(BIN_DIR)/tictactoe
TEST_TARGET = $(BIN_DIR)/test_board

SRCS = $(wildcard $(SRC_DIR)/*.cpp)
OBJS = $(patsubst $(SRC_DIR)/%.cpp, $(OBJ_DIR)/%.o, $(SRCS))

all: $(TARGET)

$(TARGET): $(OBJS) | $(BIN_DIR)
	$(CXX) $(OBJS) -o $@ $(LDFLAGS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

run: all
	./$(TARGET)

test: $(TEST_TARGET)
	./$(TEST_TARGET)

$(TEST_TARGET): $(TEST_DIR)/test_board.o $(OBJ_DIR)/Board.o | $(BIN_DIR)
	$(CXX) $^ -o $@ $(LDFLAGS)

$(TEST_DIR)/test_board.o: $(TEST_DIR)/test_board.cpp | $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

help:
	@echo "Available targets:"
	@echo "  all     - Build the main executable"
	@echo "  clean   - Remove build artifacts"
	@echo "  run     - Build and run the executable"
	@echo "  test    - Build and run unit tests"
	@echo "  help    - Show this help message"