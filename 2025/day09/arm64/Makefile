CC = clang
CFLAGS = -O3 -march=armv8-a+simd -mtune=apple-m1 -flto -ffast-math
TARGET = solution

all: $(TARGET)

$(TARGET): solution.c
	$(CC) $(CFLAGS) -o $(TARGET) solution.c

clean:
	rm -f $(TARGET)

run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
