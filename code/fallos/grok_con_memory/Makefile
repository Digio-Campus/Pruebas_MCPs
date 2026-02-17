CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -Iinclude
LDFLAGS = -lncurses

SRC_DIR = src
INC_DIR = include
BIN_DIR = bin

SOURCES = $(SRC_DIR)/main.cpp $(SRC_DIR)/Board.cpp $(SRC_DIR)/Game.cpp $(SRC_DIR)/UI.cpp
OBJECTS = $(SOURCES:.cpp=.o)
EXECUTABLE = $(BIN_DIR)/tres_en_raya

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	mkdir -p $(BIN_DIR)
	$(CXX) $(OBJECTS) -o $@ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

run: $(EXECUTABLE)
	./$(EXECUTABLE)

.PHONY: all clean run