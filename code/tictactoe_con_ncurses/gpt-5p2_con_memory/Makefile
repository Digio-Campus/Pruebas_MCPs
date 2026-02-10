CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -pedantic -O2 -Iinclude

# Intenta ncursesw primero (habitual en distros modernas); si falla, cae a ncurses.
NCURSES_LIBS := $(shell pkg-config --libs ncursesw 2>/dev/null || pkg-config --libs ncurses 2>/dev/null || echo -lncurses)
NCURSES_CFLAGS := $(shell pkg-config --cflags ncursesw 2>/dev/null || pkg-config --cflags ncurses 2>/dev/null || echo "")

CXXFLAGS += $(NCURSES_CFLAGS)
LDFLAGS := $(NCURSES_LIBS)

BIN := bin/tictactoe
SRC := $(wildcard src/*.cpp)
OBJ := $(SRC:.cpp=.o)

.PHONY: all clean run

all: $(BIN)

$(BIN): $(OBJ) | bin
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

bin:
	@mkdir -p bin

src/%.o: src/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

run: all
	./$(BIN)

clean:
	rm -f src/*.o $(BIN)
