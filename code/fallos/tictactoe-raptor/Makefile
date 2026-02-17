CXX := g++
CXXFLAGS := -std=c++17 -Iinclude -Wall -Wextra
LDFLAGS := -lncurses

SRCS := $(wildcard src/*.cpp)
OBJS := $(SRCS:.cpp=.o)
TARGET := bin/tictactoe

.PHONY: all clean run test
all: $(TARGET)

$(TARGET): $(SRCS)
	@mkdir -p bin
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

run: all
	./$(TARGET)

test: $(SRCS) tests/test_board.cpp
	$(CXX) $(CXXFLAGS) src/Board.cpp tests/test_board.cpp -o tests/test_board
	./tests/test_board

clean:
	rm -rf bin tests/test_board *.o
