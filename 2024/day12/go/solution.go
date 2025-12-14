package main

import (
	"fmt"
	"os"
	"strings"
)

// Point represents a coordinate in the grid
type Point struct {
	r, c int
}

// Add returns a new Point offset by the given delta
func (p Point) Add(delta Point) Point {
	return Point{p.r + delta.r, p.c + delta.c}
}

// Grid represents the garden map with plant types
type Grid struct {
	cells [][]rune
	rows  int
	cols  int
}

// NewGrid creates a Grid from input lines
func NewGrid(lines []string) *Grid {
	cells := make([][]rune, len(lines))
	for i, line := range lines {
		cells[i] = []rune(line)
	}
	return &Grid{
		cells: cells,
		rows:  len(cells),
		cols:  len(cells[0]),
	}
}

// InBounds checks if a point is within the grid boundaries
func (g *Grid) InBounds(p Point) bool {
	return p.r >= 0 && p.r < g.rows && p.c >= 0 && p.c < g.cols
}

// At returns the plant type at the given point
func (g *Grid) At(p Point) rune {
	return g.cells[p.r][p.c]
}

// Cardinal directions (up, down, left, right)
var directions = []Point{
	{-1, 0}, // up
	{1, 0},  // down
	{0, -1}, // left
	{0, 1},  // right
}

// cornerPatterns defines the conditions for detecting corners
// Each pattern checks: [side1, side2, diagonal]
// true = in region, false = not in region
type cornerPattern struct {
	side1    Point
	side2    Point
	diagonal Point
}

var cornerPatterns = []cornerPattern{
	// Top-left
	{Point{-1, 0}, Point{0, -1}, Point{-1, -1}},
	// Top-right
	{Point{-1, 0}, Point{0, 1}, Point{-1, 1}},
	// Bottom-left
	{Point{1, 0}, Point{0, -1}, Point{1, -1}},
	// Bottom-right
	{Point{1, 0}, Point{0, 1}, Point{1, 1}},
}

// findRegions identifies all contiguous regions of the same plant type
func (g *Grid) findRegions() []map[Point]bool {
	visited := make(map[Point]bool)
	var regions []map[Point]bool

	for r := 0; r < g.rows; r++ {
		for c := 0; c < g.cols; c++ {
			p := Point{r, c}
			if visited[p] {
				continue
			}

			// BFS to find all cells in this region
			plant := g.At(p)
			region := make(map[Point]bool)
			queue := []Point{p}

			for len(queue) > 0 {
				curr := queue[0]
				queue = queue[1:]

				if visited[curr] {
					continue
				}
				if !g.InBounds(curr) {
					continue
				}
				if g.At(curr) != plant {
					continue
				}

				visited[curr] = true
				region[curr] = true

				// Check all 4 neighbors
				for _, d := range directions {
					next := curr.Add(d)
					if !visited[next] {
						queue = append(queue, next)
					}
				}
			}

			regions = append(regions, region)
		}
	}

	return regions
}

// calculatePerimeter computes the perimeter of a region
func calculatePerimeter(region map[Point]bool) int {
	perimeter := 0

	for p := range region {
		for _, d := range directions {
			neighbor := p.Add(d)
			if !region[neighbor] {
				perimeter++
			}
		}
	}

	return perimeter
}

// countSides counts the number of sides (corners) in a region
// The number of sides equals the number of corners
func countSides(region map[Point]bool) int {
	corners := 0

	for p := range region {
		// Check each corner pattern
		for _, pattern := range cornerPatterns {
			side1In := region[p.Add(pattern.side1)]
			side2In := region[p.Add(pattern.side2)]
			diagIn := region[p.Add(pattern.diagonal)]

			// Convex corner: both sides are out
			if !side1In && !side2In {
				corners++
			}
			// Concave corner: both sides are in, but diagonal is out
			if side1In && side2In && !diagIn {
				corners++
			}
		}
	}

	return corners
}

// part1 calculates the total fencing cost using perimeter
func part1(g *Grid) int {
	regions := g.findRegions()
	total := 0

	for _, region := range regions {
		area := len(region)
		perimeter := calculatePerimeter(region)
		total += area * perimeter
	}

	return total
}

// part2 calculates the total fencing cost using sides (bulk discount)
func part2(g *Grid) int {
	regions := g.findRegions()
	total := 0

	for _, region := range regions {
		area := len(region)
		sides := countSides(region)
		total += area * sides
	}

	return total
}

func main() {
	// Read input
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		panic(err)
	}

	// Parse grid
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	grid := NewGrid(lines)

	fmt.Printf("Part 1: %d\n", part1(grid))
	fmt.Printf("Part 2: %d\n", part2(grid))
}
