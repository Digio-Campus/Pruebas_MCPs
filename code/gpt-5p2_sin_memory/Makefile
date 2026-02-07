CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -Wpedantic -O2
INCLUDES := -Iinclude
LDFLAGS := -lncurses

TARGET := bin/tres_en_raya

SRC := \
	src/main.cpp \
	src/Board.cpp \
	src/Settings.cpp \
	src/Ui.cpp

OBJ := $(SRC:.cpp=.o)

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(OBJ)
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) $(OBJ) -o $@ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $< -o $@

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(OBJ) $(TARGET)
