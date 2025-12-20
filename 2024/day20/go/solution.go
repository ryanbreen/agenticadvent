package main

import (
	"bufio"
	"fmt"
	"os"
)

type Point struct {
	r, c int
}

func parseGrid(lines []string) ([][]byte, Point, Point) {
	grid := make([][]byte, len(lines))
	var start, end Point

	for r, line := range lines {
		grid[r] = []byte(line)
		for c, ch := range grid[r] {
			if ch == 'S' {
				start = Point{r, c}
			} else if ch == 'E' {
				end = Point{r, c}
			}
		}
	}

	return grid, start, end
}

func tracePath(grid [][]byte, start, end Point) map[Point]int {
	rows := len(grid)
	cols := len(grid[0])
	dist := make(map[Point]int)
	dist[start] = 0

	queue := []Point{start}
	directions := []Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

	for len(queue) > 0 {
		cur := queue[0]
		queue = queue[1:]

		if cur == end {
			break
		}

		for _, d := range directions {
			nr, nc := cur.r+d.r, cur.c+d.c
			next := Point{nr, nc}

			if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#' {
				if _, exists := dist[next]; !exists {
					dist[next] = dist[cur] + 1
					queue = append(queue, next)
				}
			}
		}
	}

	return dist
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func countCheats(dist map[Point]int, maxCheatTime, minSavings int) int {
	count := 0

	// Convert map to slice for iteration
	trackPositions := make([]Point, 0, len(dist))
	for p := range dist {
		trackPositions = append(trackPositions, p)
	}

	for _, p1 := range trackPositions {
		d1 := dist[p1]
		for _, p2 := range trackPositions {
			// Manhattan distance is the cheat cost
			cheatCost := abs(p2.r-p1.r) + abs(p2.c-p1.c)
			if cheatCost <= maxCheatTime {
				d2 := dist[p2]
				savings := d2 - d1 - cheatCost
				if savings >= minSavings {
					count++
				}
			}
		}
	}

	return count
}

func part1(grid [][]byte, start, end Point) int {
	dist := tracePath(grid, start, end)
	return countCheats(dist, 2, 100)
}

func part2(grid [][]byte, start, end Point) int {
	dist := tracePath(grid, start, end)
	return countCheats(dist, 20, 100)
}

func main() {
	file, err := os.Open("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) > 0 {
			lines = append(lines, line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	grid, start, end := parseGrid(lines)

	fmt.Println("Part 1:", part1(grid, start, end))
	fmt.Println("Part 2:", part2(grid, start, end))
}
