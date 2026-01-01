// Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem.
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Instruction struct {
	direction string
	distance  int
	color     string
}

type Point struct {
	r, c int
}

func parseInput(filename string) ([]Instruction, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var instructions []Instruction
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Fields(line)
		direction := parts[0]
		distance, _ := strconv.Atoi(parts[1])
		// Extract color code: "(#70c710)" -> "70c710"
		color := parts[2][2 : len(parts[2])-1]
		instructions = append(instructions, Instruction{direction, distance, color})
	}
	return instructions, scanner.Err()
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

// calculateArea computes total area using Shoelace formula and Pick's theorem.
// Shoelace gives twice the signed area of the polygon.
// Pick's theorem: A = i + b/2 - 1, where i = interior points, b = boundary points
// We want: Total = i + b = A + b/2 + 1
func calculateArea(vertices []Point, perimeter int) int {
	n := len(vertices)
	area := 0
	for i := 0; i < n; i++ {
		j := (i + 1) % n
		area += vertices[i].r * vertices[j].c
		area -= vertices[j].r * vertices[i].c
	}
	area = abs(area) / 2

	// Total points = interior + boundary
	// From Pick's theorem: interior = area - boundary/2 + 1
	// Total = interior + boundary = area + boundary/2 + 1
	return area + perimeter/2 + 1
}

func part1(instructions []Instruction) int {
	directionMap := map[string]Point{
		"R": {0, 1},
		"D": {1, 0},
		"L": {0, -1},
		"U": {-1, 0},
	}

	vertices := []Point{{0, 0}}
	perimeter := 0
	r, c := 0, 0

	for _, inst := range instructions {
		delta := directionMap[inst.direction]
		r += delta.r * inst.distance
		c += delta.c * inst.distance
		vertices = append(vertices, Point{r, c})
		perimeter += inst.distance
	}

	return calculateArea(vertices, perimeter)
}

func part2(instructions []Instruction) int {
	// Last digit of hex: 0=R, 1=D, 2=L, 3=U
	// First 5 digits: distance in hex
	directionMap := map[byte]Point{
		'0': {0, 1},   // R
		'1': {1, 0},   // D
		'2': {0, -1},  // L
		'3': {-1, 0},  // U
	}

	vertices := []Point{{0, 0}}
	perimeter := 0
	r, c := 0, 0

	for _, inst := range instructions {
		// Parse distance from first 5 hex digits
		distance, _ := strconv.ParseInt(inst.color[:5], 16, 64)
		// Get direction from last hex digit
		dirChar := inst.color[5]
		delta := directionMap[dirChar]
		r += delta.r * int(distance)
		c += delta.c * int(distance)
		vertices = append(vertices, Point{r, c})
		perimeter += int(distance)
	}

	return calculateArea(vertices, perimeter)
}

func main() {
	instructions, err := parseInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(instructions))
	fmt.Printf("Part 2: %d\n", part2(instructions))
}
