package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const (
	gridSize     = 71
	initialBytes = 1024
)

type Point struct {
	x, y int
}

type QueueItem struct {
	point Point
	steps int
}

func parseInput(filename string) ([]Point, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var positions []Point
	scanner := bufio.NewScanner(file)
	lineNum := 0
	for scanner.Scan() {
		lineNum++
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Split(line, ",")
		if len(parts) != 2 {
			return nil, fmt.Errorf("line %d: expected format 'x,y', got %q", lineNum, line)
		}
		x, err := strconv.Atoi(parts[0])
		if err != nil {
			return nil, fmt.Errorf("line %d: parsing x coordinate: %w", lineNum, err)
		}
		y, err := strconv.Atoi(parts[1])
		if err != nil {
			return nil, fmt.Errorf("line %d: parsing y coordinate: %w", lineNum, err)
		}
		positions = append(positions, Point{x, y})
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("reading input: %w", err)
	}
	return positions, nil
}

// bfs finds the shortest path from (0,0) to (size-1,size-1) avoiding corrupted cells.
// Returns (steps, true) if a path exists, or (0, false) if no path is found.
func bfs(corrupted map[Point]bool, size int) (int, bool) {
	start := Point{0, 0}
	goal := Point{size - 1, size - 1}

	if corrupted[start] || corrupted[goal] {
		return 0, false
	}

	queue := []QueueItem{{start, 0}}
	visited := make(map[Point]bool)
	visited[start] = true

	directions := []Point{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current.point == goal {
			return current.steps, true
		}

		for _, d := range directions {
			next := Point{current.point.x + d.x, current.point.y + d.y}
			if next.x >= 0 && next.x < size && next.y >= 0 && next.y < size &&
				!visited[next] && !corrupted[next] {
				visited[next] = true
				queue = append(queue, QueueItem{next, current.steps + 1})
			}
		}
	}

	return 0, false
}

func part1(positions []Point, numBytes, size int) int {
	corrupted := make(map[Point]bool)
	for i := 0; i < numBytes && i < len(positions); i++ {
		corrupted[positions[i]] = true
	}
	steps, _ := bfs(corrupted, size)
	return steps
}

func part2(positions []Point, size int) string {
	left, right := 0, len(positions)

	for left < right {
		mid := (left + right) / 2
		corrupted := make(map[Point]bool)
		for i := 0; i <= mid; i++ {
			corrupted[positions[i]] = true
		}
		_, found := bfs(corrupted, size)
		if !found {
			right = mid
		} else {
			left = mid + 1
		}
	}

	blockingPos := positions[left]
	return fmt.Sprintf("%d,%d", blockingPos.x, blockingPos.y)
}

func main() {
	positions, err := parseInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(positions, initialBytes, gridSize))
	fmt.Println("Part 2:", part2(positions, gridSize))
}
