.PHONY: all clean run

all: solution

solution: solution.zig
	zig build-exe solution.zig -O ReleaseFast

clean:
	rm -f solution solution.o

run: solution
	./solution
