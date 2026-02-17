CXX=g++
CXXFLAGS=-Wall -Wextra -std=c++11 -Iinclude
LDFLAGS=-lncurses
SRCS=$(wildcard src/*.cpp)
OBJS=$(SRCS:.cpp=.o)
TARGET=bin/tictactoe_ncurses

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(OBJS) | bin
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

bin:
	mkdir -p bin

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f src/*.o $(TARGET)

run: all
	./$(TARGET)
