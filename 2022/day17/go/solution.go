package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Rock shapes as slices of (dx, dy) offsets from bottom-left
var rocks = [][][2]int{
	{{0, 0}, {1, 0}, {2, 0}, {3, 0}},         // Horizontal line
	{{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}}, // Plus
	{{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}}, // L shape
	{{0, 0}, {0, 1}, {0, 2}, {0, 3}},         // Vertical line
	{{0, 0}, {1, 0}, {0, 1}, {1, 1}},         // Square
}

const width = 7

type point struct {
	x, y int
}

type state struct {
	rockType int
	jetIdx   int
	profile  [7]int
}

func simulate(jets string, numRocks int) int {
	occupied := make(map[point]bool)
	height := 0
	jetIdx := 0

	// For cycle detection
	states := make(map[state]int)
	heights := make([]int, 0, numRocks)

	for rockNum := 0; rockNum < numRocks; rockNum++ {
		rockType := rockNum % 5
		rock := rocks[rockType]

		// Starting position: left edge at x=2, bottom at y=height+3
		x, y := 2, height+3

		for {
			// Jet push
			jet := jets[jetIdx]
			jetIdx = (jetIdx + 1) % len(jets)

			dx := 1
			if jet == '<' {
				dx = -1
			}

			// Check if can move horizontally
			canMove := true
			for _, offset := range rock {
				nx := x + offset[0] + dx
				ny := y + offset[1]
				if nx < 0 || nx >= width || occupied[point{nx, ny}] {
					canMove = false
					break
				}
			}

			if canMove {
				x += dx
			}

			// Fall down
			canFall := true
			for _, offset := range rock {
				nx := x + offset[0]
				ny := y + offset[1] - 1
				if ny < 0 || occupied[point{nx, ny}] {
					canFall = false
					break
				}
			}

			if canFall {
				y--
			} else {
				// Rock stops
				for _, offset := range rock {
					occupied[point{x + offset[0], y + offset[1]}] = true
					if y+offset[1]+1 > height {
						height = y + offset[1] + 1
					}
				}
				break
			}
		}

		heights = append(heights, height)

		// Cycle detection for Part 2
		if numRocks > 10000 {
			// Create state key from surface profile
			// Use relative heights from top
			var profile [7]int
			profileDepth := 30
			for col := 0; col < width; col++ {
				for row := 0; row < profileDepth; row++ {
					if occupied[point{col, height - 1 - row}] {
						profile[col] = row
						break
					}
				}
				if profile[col] == 0 && !occupied[point{col, height - 1}] {
					profile[col] = profileDepth
				}
			}

			s := state{rockType, jetIdx, profile}

			if prevRockNum, found := states[s]; found {
				// Found cycle
				cycleLen := rockNum - prevRockNum
				cycleHeight := height - heights[prevRockNum]

				// Calculate final height
				remaining := numRocks - rockNum - 1
				fullCycles := remaining / cycleLen
				leftover := remaining % cycleLen

				finalHeight := height + fullCycles*cycleHeight
				if leftover > 0 {
					finalHeight += heights[prevRockNum+leftover] - heights[prevRockNum]
				}

				return finalHeight
			}

			states[s] = rockNum
		}
	}

	return height
}

func part1(jets string) int {
	return simulate(jets, 2022)
}

func part2(jets string) int {
	return simulate(jets, 1000000000000)
}

func main() {
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)
	inputPath := filepath.Join(dir, "..", "input.txt")

	// Try relative path if executable path doesn't work
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = "../input.txt"
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	jets := strings.TrimSpace(string(data))

	fmt.Println("Part 1:", part1(jets))
	fmt.Println("Part 2:", part2(jets))
}
