package main

import (
	"bufio"
	"fmt"
	"os"
)

type point struct {
	row, col int
}

func parseGrid(lines []string) []point {
	var galaxies []point
	for r, line := range lines {
		for c, ch := range line {
			if ch == '#' {
				galaxies = append(galaxies, point{r, c})
			}
		}
	}
	return galaxies
}

func findEmptyRowsAndCols(lines []string) (map[int]bool, map[int]bool) {
	rows := len(lines)
	cols := 0
	if rows > 0 {
		cols = len(lines[0])
	}

	emptyRows := make(map[int]bool)
	emptyCols := make(map[int]bool)

	// Find empty rows
	for r, line := range lines {
		hasGalaxy := false
		for _, ch := range line {
			if ch == '#' {
				hasGalaxy = true
				break
			}
		}
		if !hasGalaxy {
			emptyRows[r] = true
		}
	}

	// Find empty columns
	for c := 0; c < cols; c++ {
		hasGalaxy := false
		for r := 0; r < rows; r++ {
			if lines[r][c] == '#' {
				hasGalaxy = true
				break
			}
		}
		if !hasGalaxy {
			emptyCols[c] = true
		}
	}

	return emptyRows, emptyCols
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

func calculateDistances(galaxies []point, emptyRows, emptyCols map[int]bool, expansionFactor int) int64 {
	var total int64 = 0

	for i := 0; i < len(galaxies); i++ {
		for j := i + 1; j < len(galaxies); j++ {
			r1, c1 := galaxies[i].row, galaxies[i].col
			r2, c2 := galaxies[j].row, galaxies[j].col

			// Calculate row distance with expansion
			minR, maxR := min(r1, r2), max(r1, r2)
			rowDist := int64(maxR - minR)
			for r := minR; r < maxR; r++ {
				if emptyRows[r] {
					rowDist += int64(expansionFactor - 1)
				}
			}

			// Calculate column distance with expansion
			minC, maxC := min(c1, c2), max(c1, c2)
			colDist := int64(maxC - minC)
			for c := minC; c < maxC; c++ {
				if emptyCols[c] {
					colDist += int64(expansionFactor - 1)
				}
			}

			total += rowDist + colDist
		}
	}

	return total
}

func part1(lines []string) int64 {
	galaxies := parseGrid(lines)
	emptyRows, emptyCols := findEmptyRowsAndCols(lines)
	return calculateDistances(galaxies, emptyRows, emptyCols, 2)
}

func part2(lines []string) int64 {
	galaxies := parseGrid(lines)
	emptyRows, emptyCols := findEmptyRowsAndCols(lines)
	return calculateDistances(galaxies, emptyRows, emptyCols, 1000000)
}

func main() {
	inputFile := "../input.txt"
	if len(os.Args) > 1 {
		inputFile = os.Args[1]
	}

	file, err := os.Open(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			lines = append(lines, line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(lines))
	fmt.Println("Part 2:", part2(lines))
}
