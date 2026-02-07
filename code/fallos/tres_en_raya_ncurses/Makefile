CXX       := g++
CXXFLAGS  := -std=c++17 -Wall -Wextra -Wpedantic -O2 -Iinclude
LDFLAGS   := -lncurses

TARGET    := bin/tres_en_raya_ncurses
SRC_DIR   := src
OBJ_DIR   := build

SRCS := $(wildcard $(SRC_DIR)/*.cpp)
OBJS := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(SRCS))

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(OBJS) | bin
	$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(LDFLAGS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

bin:
	@mkdir -p $@

$(OBJ_DIR):
	@mkdir -p $@

run: $(TARGET)
	./$(TARGET)

clean:
	@rm -rf $(OBJ_DIR) $(TARGET)
