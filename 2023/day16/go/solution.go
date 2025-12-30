package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
)

const maxSize = 256

// Directions: 0=right, 1=down, 2=left, 3=up
var dr = [4]int{0, 1, 0, -1}
var dc = [4]int{1, 0, -1, 0}

type state struct {
	row, col, dir int
}

func parseInput(filename string) ([]string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) > 0 {
			grid = append(grid, line)
		}
	}
	return grid, scanner.Err()
}

func countEnergized(grid []string, startRow, startCol, startDir int) int {
	rows := len(grid)
	cols := len(grid[0])

	var visited [maxSize][maxSize][4]bool
	queue := make([]state, 0, 1024)
	queue = append(queue, state{startRow, startCol, startDir})

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		r, c, d := current.row, current.col, current.dir

		// Check bounds
		if r < 0 || r >= rows || c < 0 || c >= cols {
			continue
		}

		// Check if already visited
		if visited[r][c][d] {
			continue
		}
		visited[r][c][d] = true

		cell := grid[r][c]

		switch cell {
		case '.':
			queue = append(queue, state{r + dr[d], c + dc[d], d})
		case '/':
			nd := [4]int{3, 2, 1, 0}[d]
			queue = append(queue, state{r + dr[nd], c + dc[nd], nd})
		case '\\':
			nd := [4]int{1, 0, 3, 2}[d]
			queue = append(queue, state{r + dr[nd], c + dc[nd], nd})
		case '|':
			if d == 0 || d == 2 {
				queue = append(queue, state{r + dr[1], c + dc[1], 1})
				queue = append(queue, state{r + dr[3], c + dc[3], 3})
			} else {
				queue = append(queue, state{r + dr[d], c + dc[d], d})
			}
		case '-':
			if d == 1 || d == 3 {
				queue = append(queue, state{r + dr[0], c + dc[0], 0})
				queue = append(queue, state{r + dr[2], c + dc[2], 2})
			} else {
				queue = append(queue, state{r + dr[d], c + dc[d], d})
			}
		}
	}

	// Count unique positions
	count := 0
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if visited[r][c][0] || visited[r][c][1] || visited[r][c][2] || visited[r][c][3] {
				count++
			}
		}
	}
	return count
}

func part1(grid []string) int {
	return countEnergized(grid, 0, 0, 0)
}

func part2(grid []string) int {
	rows := len(grid)
	cols := len(grid[0])
	maxEnergized := 0

	// Top row, heading down
	for c := 0; c < cols; c++ {
		e := countEnergized(grid, 0, c, 1)
		if e > maxEnergized {
			maxEnergized = e
		}
	}

	// Bottom row, heading up
	for c := 0; c < cols; c++ {
		e := countEnergized(grid, rows-1, c, 3)
		if e > maxEnergized {
			maxEnergized = e
		}
	}

	// Left column, heading right
	for r := 0; r < rows; r++ {
		e := countEnergized(grid, r, 0, 0)
		if e > maxEnergized {
			maxEnergized = e
		}
	}

	// Right column, heading left
	for r := 0; r < rows; r++ {
		e := countEnergized(grid, r, cols-1, 2)
		if e > maxEnergized {
			maxEnergized = e
		}
	}

	return maxEnergized
}

func main() {
	_, currentFile, _, _ := runtime.Caller(0)
	inputPath := filepath.Join(filepath.Dir(currentFile), "..", "input.txt")

	grid, err := parseInput(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(grid))
	fmt.Printf("Part 2: %d\n", part2(grid))
}
