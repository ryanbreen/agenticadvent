package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Point struct {
	r, c int
}

var directions = []Point{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}

var slopeDirs = map[byte]Point{
	'^': {-1, 0},
	'v': {1, 0},
	'<': {0, -1},
	'>': {0, 1},
}

func parseInput(filename string) []string {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			grid = append(grid, line)
		}
	}
	return grid
}

func findJunctions(grid []string) map[Point]bool {
	rows, cols := len(grid), len(grid[0])
	junctions := make(map[Point]bool)

	// Start and end points
	start := Point{0, strings.Index(grid[0], ".")}
	end := Point{rows - 1, strings.Index(grid[rows-1], ".")}
	junctions[start] = true
	junctions[end] = true

	// Find intersections (cells with 3+ walkable neighbors)
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] == '#' {
				continue
			}
			neighbors := 0
			for _, d := range directions {
				nr, nc := r+d.r, c+d.c
				if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#' {
					neighbors++
				}
			}
			if neighbors >= 3 {
				junctions[Point{r, c}] = true
			}
		}
	}

	return junctions
}

func buildGraph(grid []string, junctions map[Point]bool, respectSlopes bool) map[Point]map[Point]int {
	rows, cols := len(grid), len(grid[0])
	graph := make(map[Point]map[Point]int)

	for startJunction := range junctions {
		graph[startJunction] = make(map[Point]int)

		// DFS from each junction to find reachable junctions
		type State struct {
			p    Point
			dist int
		}
		stack := []State{{startJunction, 0}}
		visited := make(map[Point]bool)
		visited[startJunction] = true

		for len(stack) > 0 {
			// Pop from stack
			current := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			r, c, dist := current.p.r, current.p.c, current.dist

			if dist > 0 && junctions[current.p] {
				// Found another junction
				graph[startJunction][current.p] = dist
				continue
			}

			// Explore neighbors
			for _, d := range directions {
				nr, nc := r+d.r, c+d.c
				if nr < 0 || nr >= rows || nc < 0 || nc >= cols {
					continue
				}
				if grid[nr][nc] == '#' {
					continue
				}
				np := Point{nr, nc}
				if visited[np] {
					continue
				}

				// Check slope constraints for Part 1
				if respectSlopes {
					cell := grid[r][c]
					if reqDir, isSlope := slopeDirs[cell]; isSlope {
						if d.r != reqDir.r || d.c != reqDir.c {
							continue
						}
					}
				}

				visited[np] = true
				stack = append(stack, State{np, dist + 1})
			}
		}
	}

	return graph
}

func longestPathDFS(graph map[Point]map[Point]int, start, end Point) int {
	visited := make(map[Point]bool)

	var dfs func(node Point) int
	dfs = func(node Point) int {
		if node == end {
			return 0
		}

		visited[node] = true
		maxDist := -1 << 30 // Very negative number

		for neighbor, dist := range graph[node] {
			if !visited[neighbor] {
				result := dfs(neighbor)
				if result != -1<<30 {
					if dist+result > maxDist {
						maxDist = dist + result
					}
				}
			}
		}

		visited[node] = false
		return maxDist
	}

	return dfs(start)
}

func solve(grid []string, respectSlopes bool) int {
	rows := len(grid)
	start := Point{0, strings.Index(grid[0], ".")}
	end := Point{rows - 1, strings.Index(grid[rows-1], ".")}

	junctions := findJunctions(grid)
	graph := buildGraph(grid, junctions, respectSlopes)

	return longestPathDFS(graph, start, end)
}

func part1(grid []string) int {
	return solve(grid, true)
}

func part2(grid []string) int {
	return solve(grid, false)
}

func main() {
	grid := parseInput("../input.txt")
	fmt.Printf("Part 1: %d\n", part1(grid))
	fmt.Printf("Part 2: %d\n", part2(grid))
}
