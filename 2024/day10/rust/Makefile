.PHONY: all clean run

all: solution

solution: solution.rs
	rustc -O solution.rs -o solution

run: solution
	./solution

clean:
	rm -f solution
