CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -O2 -Iinclude
LDFLAGS = -lncurses

SRC = $(wildcard src/*.cpp)
OBJ = $(patsubst src/%.cpp,obj/%.o,$(SRC))
BIN = bin/tictactoe

TEST_SRC = $(wildcard tests/*.cpp)
TEST_BIN = bin/test_mouse_logic

all: $(BIN)

$(BIN): $(OBJ) | bin
	$(CXX) $(CXXFLAGS) -o $@ $(OBJ) $(LDFLAGS)

obj/%.o: src/%.cpp | obj
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin:
	mkdir -p bin

obj:
	mkdir -p obj

clean:
	rm -rf obj bin

run: all
	$(BIN)

help:
	@echo "Targets: all, clean, run, test, help"

test: all $(TEST_BIN)
	./$(TEST_BIN)
	bash tests/test_integration.sh

$(TEST_BIN): tests/test_mouse_logic.cpp | bin
	$(CXX) $(CXXFLAGS) -Iinclude -o $(TEST_BIN) tests/test_mouse_logic.cpp
