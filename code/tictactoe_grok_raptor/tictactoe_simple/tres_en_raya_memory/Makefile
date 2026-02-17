CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++11
TARGET = tres_en_raya
SRC = tres_en_raya.cpp

all: $(TARGET)

$(TARGET): $(SRC)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET)

run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
