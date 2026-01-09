package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

// Point represents a position on the grid
type Point struct {
	r, c int
}

// parseInput reads the input file and returns elf positions
func parseInput(text []string) map[Point]bool {
	elves := make(map[Point]bool)
	for r, line := range text {
		for c, ch := range line {
			if ch == '#' {
				elves[Point{r, c}] = true
			}
		}
	}
	return elves
}

// Direction checking data
type DirCheck struct {
	checks []Point // positions to check
	move   Point   // delta to move
}

var dirChecks = map[byte]DirCheck{
	'N': {[]Point{{-1, -1}, {-1, 0}, {-1, 1}}, Point{-1, 0}},
	'S': {[]Point{{1, -1}, {1, 0}, {1, 1}}, Point{1, 0}},
	'W': {[]Point{{-1, -1}, {0, -1}, {1, -1}}, Point{0, -1}},
	'E': {[]Point{{-1, 1}, {0, 1}, {1, 1}}, Point{0, 1}},
}

var allNeighbors = []Point{
	{-1, -1}, {-1, 0}, {-1, 1},
	{0, -1}, {0, 1},
	{1, -1}, {1, 0}, {1, 1},
}

// simulateRound runs one round of simulation
func simulateRound(elves map[Point]bool, directions []byte) (map[Point]bool, bool) {
	// Phase 1: Each elf proposes a move
	proposals := make(map[Point]Point)    // elf -> proposed position
	proposalCounts := make(map[Point]int) // position -> count

	for elf := range elves {
		// Check if any neighbors
		hasNeighbor := false
		for _, d := range allNeighbors {
			if elves[Point{elf.r + d.r, elf.c + d.c}] {
				hasNeighbor = true
				break
			}
		}

		if !hasNeighbor {
			continue // Don't move
		}

		// Try each direction
		for _, dir := range directions {
			check := dirChecks[dir]
			canMove := true
			for _, c := range check.checks {
				if elves[Point{elf.r + c.r, elf.c + c.c}] {
					canMove = false
					break
				}
			}
			if canMove {
				newPos := Point{elf.r + check.move.r, elf.c + check.move.c}
				proposals[elf] = newPos
				proposalCounts[newPos]++
				break
			}
		}
	}

	// Phase 2: Execute moves (only if unique proposal)
	newElves := make(map[Point]bool)
	moved := false

	for elf := range elves {
		if newPos, ok := proposals[elf]; ok {
			if proposalCounts[newPos] == 1 {
				newElves[newPos] = true
				moved = true
			} else {
				newElves[elf] = true
			}
		} else {
			newElves[elf] = true
		}
	}

	return newElves, moved
}

// boundingRectEmpty counts empty tiles in the bounding rectangle
func boundingRectEmpty(elves map[Point]bool) int {
	if len(elves) == 0 {
		return 0
	}

	minR, maxR := 1<<30, -1<<30
	minC, maxC := 1<<30, -1<<30

	for elf := range elves {
		if elf.r < minR {
			minR = elf.r
		}
		if elf.r > maxR {
			maxR = elf.r
		}
		if elf.c < minC {
			minC = elf.c
		}
		if elf.c > maxC {
			maxC = elf.c
		}
	}

	area := (maxR - minR + 1) * (maxC - minC + 1)
	return area - len(elves)
}

// part1 counts empty tiles after 10 rounds
func part1(lines []string) int {
	elves := parseInput(lines)
	directions := []byte{'N', 'S', 'W', 'E'}

	for i := 0; i < 10; i++ {
		elves, _ = simulateRound(elves, directions)
		// Rotate directions
		directions = append(directions[1:], directions[0])
	}

	return boundingRectEmpty(elves)
}

// part2 finds the first round where no elf moves
func part2(lines []string) int {
	elves := parseInput(lines)
	directions := []byte{'N', 'S', 'W', 'E'}

	roundNum := 0
	for {
		roundNum++
		var moved bool
		elves, moved = simulateRound(elves, directions)
		if !moved {
			return roundNum
		}
		// Rotate directions
		directions = append(directions[1:], directions[0])
	}
}

func main() {
	// Get input file path
	execPath, _ := os.Executable()
	execDir := filepath.Dir(execPath)
	inputPath := filepath.Join(execDir, "..", "input.txt")

	// If running with go run, use the source file location
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		// Try relative to current working directory
		inputPath = filepath.Join(".", "..", "input.txt")
	}

	// If still not found, try from script location
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		// For go run, we need to use a different approach
		inputPath = "../input.txt"
	}

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	fmt.Println("Part 1:", part1(lines))
	fmt.Println("Part 2:", part2(lines))
}
