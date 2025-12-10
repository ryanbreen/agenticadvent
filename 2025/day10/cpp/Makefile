CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall
TARGET = solution
SRC = solution.cpp

$(TARGET): $(SRC)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET)

run: $(TARGET)
	./$(TARGET)

.PHONY: clean run
