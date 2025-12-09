package main

import (
	"fmt"
	"math"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

type Edge struct {
	y, xMin, xMax int // For horizontal edges
}

type VertEdge struct {
	x, yMin, yMax int // For vertical edges
}

func parseInput(filename string) ([]Point, error) {
	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	points := make([]Point, 0, len(lines))

	for _, line := range lines {
		parts := strings.Split(line, ",")
		if len(parts) != 2 {
			continue
		}
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		points = append(points, Point{x, y})
	}

	return points, nil
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func part1(points []Point) int {
	maxArea := 0
	n := len(points)

	// Check all pairs of points as opposite corners
	for i := 0; i < n; i++ {
		x1, y1 := points[i].x, points[i].y
		for j := i + 1; j < n; j++ {
			x2, y2 := points[j].x, points[j].y
			// Rectangle area = width * height (inclusive of both corners)
			width := abs(x2-x1) + 1
			height := abs(y2-y1) + 1
			area := width * height
			if area > maxArea {
				maxArea = area
			}
		}
	}

	return maxArea
}

func part2(points []Point) int {
	n := len(points)

	// Build edges
	horizontalEdges := make([]Edge, 0)
	verticalEdges := make([]VertEdge, 0)

	for i := 0; i < n; i++ {
		x1, y1 := points[i].x, points[i].y
		x2, y2 := points[(i+1)%n].x, points[(i+1)%n].y

		if y1 == y2 { // Horizontal edge
			horizontalEdges = append(horizontalEdges, Edge{y1, min(x1, x2), max(x1, x2)})
		} else { // Vertical edge
			verticalEdges = append(verticalEdges, VertEdge{x1, min(y1, y2), max(y1, y2)})
		}
	}

	// Map edges by coordinate for efficient lookup
	vertByX := make(map[int][]VertEdge)
	for _, edge := range verticalEdges {
		vertByX[edge.x] = append(vertByX[edge.x], edge)
	}

	horizByY := make(map[int][]Edge)
	for _, edge := range horizontalEdges {
		horizByY[edge.y] = append(horizByY[edge.y], edge)
	}

	// Check if point is inside polygon using ray casting
	isInsidePolygon := func(x, y float64) bool {
		crossings := 0.0
		// Cast ray to the right
		for vx := range vertByX {
			if float64(vx) <= x {
				continue
			}
			for _, edge := range vertByX[vx] {
				if float64(edge.yMin) < y && y < float64(edge.yMax) {
					// Strict inequality - ray crosses edge
					crossings += 1.0
				} else if y == float64(edge.yMin) || y == float64(edge.yMax) {
					// On corner - count as 0.5 crossing
					crossings += 0.5
				}
			}
		}
		return math.Mod(crossings, 2) == 1
	}

	// Check if rectangle is valid (entirely inside polygon)
	rectangleValid := func(x1, y1, x2, y2 int) bool {
		minX, maxX := min(x1, x2), max(x1, x2)
		minY, maxY := min(y1, y2), max(y1, y2)

		// Check if any vertical edge crosses through the rectangle interior
		for vx, edges := range vertByX {
			if minX < vx && vx < maxX {
				for _, edge := range edges {
					// Check if this edge segment overlaps with rectangle's y range
					if !(edge.yMax <= minY || edge.yMin >= maxY) {
						return false
					}
				}
			}
		}

		// Check if any horizontal edge crosses through the rectangle interior
		for hy, edges := range horizByY {
			if minY < hy && hy < maxY {
				for _, edge := range edges {
					// Check if this edge segment overlaps with rectangle's x range
					if !(edge.xMax <= minX || edge.xMin >= maxX) {
						return false
					}
				}
			}
		}

		// Finally, check that we're inside the polygon (not outside)
		// Check center point
		centerX := float64(minX+maxX) / 2.0
		centerY := float64(minY+maxY) / 2.0
		return isInsidePolygon(centerX, centerY)
	}

	// Find largest valid rectangle with red corners
	maxArea := 0

	for i := 0; i < len(points); i++ {
		x1, y1 := points[i].x, points[i].y
		for j := i + 1; j < len(points); j++ {
			x2, y2 := points[j].x, points[j].y

			if rectangleValid(x1, y1, x2, y2) {
				width := abs(x2-x1) + 1
				height := abs(y2-y1) + 1
				area := width * height
				if area > maxArea {
					maxArea = area
				}
			}
		}
	}

	return maxArea
}

func main() {
	// Get the directory containing this executable
	exePath, err := os.Executable()
	if err != nil {
		fmt.Println("Error getting executable path:", err)
		return
	}
	exeDir := filepath.Dir(exePath)
	inputPath := filepath.Join(exeDir, "..", "input.txt")

	// If running with go run, use current directory
	if strings.Contains(exePath, "go-build") {
		inputPath = "../input.txt"
	}

	points, err := parseInput(inputPath)
	if err != nil {
		fmt.Println("Error reading input:", err)
		return
	}

	fmt.Printf("Part 1: %d\n", part1(points))
	fmt.Printf("Part 2: %d\n", part2(points))
}
