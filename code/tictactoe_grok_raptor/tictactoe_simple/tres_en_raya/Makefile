CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra
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
