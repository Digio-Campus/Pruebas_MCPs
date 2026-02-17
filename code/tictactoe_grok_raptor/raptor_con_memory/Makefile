CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -Iinclude
LDFLAGS = -lncurses
SRCS = src/main.cpp src/Board.cpp src/Settings.cpp src/Menu.cpp src/UI.cpp src/Game.cpp
BIN = bin/tictactoe

.PHONY: all clean run test

all: $(BIN)

$(BIN): $(SRCS)
	@mkdir -p bin
	$(CXX) $(CXXFLAGS) $(SRCS) -o $(BIN) $(LDFLAGS)

clean:
	rm -f $(BIN) bin/test_mouse_logic

run: $(BIN)
	./$(BIN)

test:
	@mkdir -p bin
	$(CXX) $(CXXFLAGS) tests/test_mouse_logic.cpp -o bin/test_mouse_logic
	./bin/test_mouse_logic
