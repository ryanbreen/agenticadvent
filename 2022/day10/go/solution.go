package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// simulateCPU yields (cycle, x) for each cycle through a channel
func simulateCPU(instructions []string, ch chan<- [2]int) {
	x := 1
	cycle := 0

	for _, line := range instructions {
		if line == "noop" {
			cycle++
			ch <- [2]int{cycle, x}
		} else { // addx V
			parts := strings.Fields(line)
			v, _ := strconv.Atoi(parts[1])
			cycle++
			ch <- [2]int{cycle, x}
			cycle++
			ch <- [2]int{cycle, x}
			x += v
		}
	}
	close(ch)
}

func part1(instructions []string) int {
	targetCycles := map[int]bool{20: true, 60: true, 100: true, 140: true, 180: true, 220: true}
	total := 0

	ch := make(chan [2]int)
	go simulateCPU(instructions, ch)

	for cycleX := range ch {
		cycle, x := cycleX[0], cycleX[1]
		if targetCycles[cycle] {
			total += cycle * x
		}
	}

	return total
}

func part2(instructions []string) string {
	var screen []string
	var row strings.Builder

	ch := make(chan [2]int)
	go simulateCPU(instructions, ch)

	for cycleX := range ch {
		cycle, x := cycleX[0], cycleX[1]
		pos := (cycle - 1) % 40 // CRT position in current row

		if abs(pos-x) <= 1 {
			row.WriteByte('#')
		} else {
			row.WriteByte('.')
		}

		if cycle%40 == 0 {
			screen = append(screen, row.String())
			row.Reset()
		}
	}

	return strings.Join(screen, "\n")
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func main() {
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)
	inputPath := filepath.Join(dir, "..", "input.txt")

	// If running with go run, use the source file's directory
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		// Fallback: assume we're running from the go directory
		inputPath = "../input.txt"
	}

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var instructions []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			instructions = append(instructions, line)
		}
	}

	fmt.Println("Part 1:", part1(instructions))
	fmt.Println("Part 2:")
	fmt.Println(part2(instructions))
}
