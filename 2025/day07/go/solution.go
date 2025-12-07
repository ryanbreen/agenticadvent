package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

func part1(lines []string) int {
	rows := len(lines)
	if rows == 0 {
		return 0
	}
	cols := len(lines[0])

	// Find starting position S
	startCol := -1
	for col := 0; col < cols; col++ {
		if lines[0][col] == 'S' {
			startCol = col
			break
		}
	}

	if startCol == -1 {
		return 0
	}

	// Track active beam columns at each row
	// Use a map to handle beam merging
	activeBeams := make(map[int]bool)
	activeBeams[startCol] = true
	splitCount := 0

	// Process row by row starting from row 1 (below S)
	for row := 1; row < rows; row++ {
		newBeams := make(map[int]bool)

		for col := range activeBeams {
			if col >= 0 && col < cols {
				cell := lines[row][col]
				if cell == '^' {
					// Beam hits splitter - count it and emit left/right
					splitCount++
					// Left beam goes to col-1, right beam goes to col+1
					if col-1 >= 0 {
						newBeams[col-1] = true
					}
					if col+1 < cols {
						newBeams[col+1] = true
					}
				} else if cell == '.' {
					// Beam continues straight down
					newBeams[col] = true
				} else {
					// If cell is something else (like S), beam continues
					newBeams[col] = true
				}
			}
		}

		activeBeams = newBeams

		// If no more beams, stop
		if len(activeBeams) == 0 {
			break
		}
	}

	return splitCount
}

func part2(lines []string) uint64 {
	rows := len(lines)
	if rows == 0 {
		return 0
	}
	cols := len(lines[0])

	// Find starting position S
	startCol := -1
	for col := 0; col < cols; col++ {
		if lines[0][col] == 'S' {
			startCol = col
			break
		}
	}

	if startCol == -1 {
		return 0
	}

	// Track number of timelines at each column position
	// Use a map: col -> count of timelines at that position
	timelines := make(map[int]uint64)
	timelines[startCol] = 1

	// Process row by row starting from row 1 (below S)
	for row := 1; row < rows; row++ {
		newTimelines := make(map[int]uint64)

		for col, count := range timelines {
			if col >= 0 && col < cols {
				cell := lines[row][col]
				if cell == '^' {
					// Each timeline splits into 2 (left and right)
					if col-1 >= 0 {
						newTimelines[col-1] += count
					}
					if col+1 < cols {
						newTimelines[col+1] += count
					}
				} else if cell == '.' {
					// Timelines continue straight down
					newTimelines[col] += count
				} else {
					// Other characters - timelines continue
					newTimelines[col] += count
				}
			}
		}

		timelines = newTimelines

		// If no more timelines, stop
		if len(timelines) == 0 {
			break
		}
	}

	// Total number of timelines
	var total uint64 = 0
	for _, count := range timelines {
		total += count
	}
	return total
}

func main() {
	// Read input file from ../input.txt
	execPath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting executable path: %v\n", err)
		os.Exit(1)
	}
	execDir := filepath.Dir(execPath)
	inputPath := filepath.Join(execDir, "..", "input.txt")

	// Try current directory if executable path doesn't work
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = "../input.txt"
	}

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
