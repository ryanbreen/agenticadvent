package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type Point struct {
	r, c int
}

var dirs = []Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

func main() {
	// Read input file
	exePath, _ := os.Executable()
	exeDir := filepath.Dir(exePath)
	inputPath := filepath.Join(exeDir, "..", "input.txt")

	// Try relative path if executable path doesn't work
	data, err := os.ReadFile(inputPath)
	if err != nil {
		inputPath = "../input.txt"
		data, err = os.ReadFile(inputPath)
		if err != nil {
			panic(err)
		}
	}

	input := strings.TrimSpace(string(data))

	// Parse grid
	lines := strings.Split(input, "\n")
	rows := len(lines)
	cols := len(lines[0])
	grid := make([][]int, rows)
	for i, line := range lines {
		grid[i] = make([]int, cols)
		for j, ch := range line {
			grid[i][j] = int(ch - '0')
		}
	}

	fmt.Printf("Part 1: %d\n", part1(grid, rows, cols))
	fmt.Printf("Part 2: %d\n", part2(grid, rows, cols))
}

func findTrailheads(grid [][]int, rows, cols int) []Point {
	trailheads := []Point{}
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] == 0 {
				trailheads = append(trailheads, Point{r, c})
			}
		}
	}
	return trailheads
}

func countReachableNines(grid [][]int, rows, cols int, startR, startC int) int {
	visited := make(map[Point]bool)
	visited[Point{startR, startC}] = true
	queue := []Point{{startR, startC}}
	nines := make(map[Point]bool)

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		currentHeight := grid[current.r][current.c]

		if currentHeight == 9 {
			nines[current] = true
			continue
		}

		// Try all four directions
		for _, dir := range dirs {
			nr := current.r + dir.r
			nc := current.c + dir.c

			if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
				next := Point{nr, nc}
				if !visited[next] && grid[nr][nc] == currentHeight+1 {
					visited[next] = true
					queue = append(queue, next)
				}
			}
		}
	}

	return len(nines)
}

func part1(grid [][]int, rows, cols int) int {
	trailheads := findTrailheads(grid, rows, cols)
	totalScore := 0
	for _, th := range trailheads {
		totalScore += countReachableNines(grid, rows, cols, th.r, th.c)
	}
	return totalScore
}

func countDistinctTrails(grid [][]int, rows, cols int, r, c int) int {
	currentHeight := grid[r][c]
	if currentHeight == 9 {
		return 1
	}

	total := 0
	for _, dir := range dirs {
		nr := r + dir.r
		nc := c + dir.c

		if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
			if grid[nr][nc] == currentHeight+1 {
				total += countDistinctTrails(grid, rows, cols, nr, nc)
			}
		}
	}

	return total
}

func part2(grid [][]int, rows, cols int) int {
	trailheads := findTrailheads(grid, rows, cols)
	totalRating := 0
	for _, th := range trailheads {
		totalRating += countDistinctTrails(grid, rows, cols, th.r, th.c)
	}
	return totalRating
}
