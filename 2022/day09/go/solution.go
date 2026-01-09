package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

var directions = map[byte]Point{
	'U': {0, 1},
	'D': {0, -1},
	'L': {-1, 0},
	'R': {1, 0},
}

func sign(x int) int {
	if x == 0 {
		return 0
	}
	if x > 0 {
		return 1
	}
	return -1
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func moveTail(head, tail Point) Point {
	dx := head.x - tail.x
	dy := head.y - tail.y

	// If adjacent or overlapping, don't move
	if abs(dx) <= 1 && abs(dy) <= 1 {
		return tail
	}

	// Move toward head
	return Point{tail.x + sign(dx), tail.y + sign(dy)}
}

func simulateRope(moves []string, ropeLength int) int {
	knots := make([]Point, ropeLength)
	visited := make(map[Point]bool)
	visited[knots[ropeLength-1]] = true

	for _, line := range moves {
		parts := strings.Fields(line)
		direction := parts[0][0]
		count, _ := strconv.Atoi(parts[1])
		d := directions[direction]

		for i := 0; i < count; i++ {
			// Move head
			knots[0].x += d.x
			knots[0].y += d.y

			// Move each subsequent knot
			for j := 1; j < ropeLength; j++ {
				knots[j] = moveTail(knots[j-1], knots[j])
			}

			visited[knots[ropeLength-1]] = true
		}
	}

	return len(visited)
}

func part1(moves []string) int {
	return simulateRope(moves, 2)
}

func part2(moves []string) int {
	return simulateRope(moves, 10)
}

func main() {
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)
	inputPath := filepath.Join(dir, "..", "input.txt")

	// If running with go run, use the source file's directory
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = filepath.Join(".", "..", "input.txt")
	}

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var moves []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			moves = append(moves, line)
		}
	}

	fmt.Println("Part 1:", part1(moves))
	fmt.Println("Part 2:", part2(moves))
}
