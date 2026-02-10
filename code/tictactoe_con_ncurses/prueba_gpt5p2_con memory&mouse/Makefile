CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -pedantic -Iinclude
LDFLAGS := -lncurses

SRC := $(wildcard src/*.cpp)
OBJ := $(patsubst src/%.cpp,build/%.o,$(SRC))
BIN := bin/tictactoe

.PHONY: all clean run

all: $(BIN)

$(BIN): $(OBJ) | bin
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

build/%.o: src/%.cpp | build
	$(CXX) $(CXXFLAGS) -c $< -o $@

build:
	mkdir -p build

bin:
	mkdir -p bin

run: all
	./$(BIN)

clean:
	rm -rf build bin
