package main

import (
	"bufio"
	"fmt"
	"os"
)

type Position struct {
	row, col int
}

func inBounds(r, c, rows, cols int) bool {
	return r >= 0 && r < rows && c >= 0 && c < cols
}

func parseInput(filename string) (int, int, map[rune][]Position, error) {
	file, err := os.Open(filename)
	if err != nil {
		return 0, 0, nil, err
	}
	defer file.Close()

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return 0, 0, nil, err
	}

	rows := len(grid)
	cols := 0
	if rows > 0 {
		cols = len(grid[0])
	}

	// Group antenna positions by frequency
	antennas := make(map[rune][]Position)
	for r, row := range grid {
		for c, ch := range row {
			if ch != '.' {
				antennas[ch] = append(antennas[ch], Position{r, c})
			}
		}
	}

	return rows, cols, antennas, nil
}

func part1(rows, cols int, antennas map[rune][]Position) int {
	antinodes := make(map[Position]struct{})

	for _, positions := range antennas {
		// For each pair of antennas with the same frequency
		for i := 0; i < len(positions); i++ {
			for j := i + 1; j < len(positions); j++ {
				r1, c1 := positions[i].row, positions[i].col
				r2, c2 := positions[j].row, positions[j].col

				// Calculate the two antinodes
				// Antinode beyond antenna 1 (away from antenna 2)
				ar1, ac1 := 2*r1-r2, 2*c1-c2
				// Antinode beyond antenna 2 (away from antenna 1)
				ar2, ac2 := 2*r2-r1, 2*c2-c1

				// Add if within bounds
				if inBounds(ar1, ac1, rows, cols) {
					antinodes[Position{ar1, ac1}] = struct{}{}
				}
				if inBounds(ar2, ac2, rows, cols) {
					antinodes[Position{ar2, ac2}] = struct{}{}
				}
			}
		}
	}

	return len(antinodes)
}

func part2(rows, cols int, antennas map[rune][]Position) int {
	antinodes := make(map[Position]struct{})

	for _, positions := range antennas {
		// For each pair of antennas with the same frequency
		for i := 0; i < len(positions); i++ {
			for j := i + 1; j < len(positions); j++ {
				r1, c1 := positions[i].row, positions[i].col
				r2, c2 := positions[j].row, positions[j].col

				dr, dc := r2-r1, c2-c1

				// Extend in both directions along the line
				// Direction 1: from antenna 1 towards and beyond antenna 2
				r, c := r1, c1
				for inBounds(r, c, rows, cols) {
					antinodes[Position{r, c}] = struct{}{}
					r += dr
					c += dc
				}

				// Direction 2: from antenna 1 away from antenna 2
				r, c = r1-dr, c1-dc
				for inBounds(r, c, rows, cols) {
					antinodes[Position{r, c}] = struct{}{}
					r -= dr
					c -= dc
				}
			}
		}
	}

	return len(antinodes)
}

func main() {
	rows, cols, antennas, err := parseInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(rows, cols, antennas))
	fmt.Println("Part 2:", part2(rows, cols, antennas))
}
