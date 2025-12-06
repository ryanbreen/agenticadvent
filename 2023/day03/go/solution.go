package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"
)

type Point struct {
	row, col int
}

func main() {
	// Read input from ../input.txt
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)

	// Try current directory first (for go run)
	inputPath := filepath.Join("..", "input.txt")
	input, err := os.ReadFile(inputPath)
	if err != nil {
		// Try from executable directory
		inputPath = filepath.Join(dir, "..", "input.txt")
		input, err = os.ReadFile(inputPath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
			os.Exit(1)
		}
	}

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	fmt.Println("Part 1:", part1(lines))
	fmt.Println("Part 2:", part2(lines))
}

func part1(lines []string) int {
	sum := 0

	for row, line := range lines {
		col := 0
		for col < len(line) {
			if unicode.IsDigit(rune(line[col])) {
				// Found start of a number
				numStart := col
				numStr := ""
				for col < len(line) && unicode.IsDigit(rune(line[col])) {
					numStr += string(line[col])
					col++
				}
				numEnd := col - 1

				// Check if this number is adjacent to any symbol
				if isAdjacentToSymbol(lines, row, numStart, numEnd) {
					num, _ := strconv.Atoi(numStr)
					sum += num
				}
			} else {
				col++
			}
		}
	}

	return sum
}

func isAdjacentToSymbol(lines []string, row, colStart, colEnd int) bool {
	// Check all adjacent cells (including diagonals)
	for r := row - 1; r <= row+1; r++ {
		if r < 0 || r >= len(lines) {
			continue
		}
		for c := colStart - 1; c <= colEnd+1; c++ {
			if c < 0 || c >= len(lines[r]) {
				continue
			}
			if r == row && c >= colStart && c <= colEnd {
				// This is the number itself, skip
				continue
			}

			ch := rune(lines[r][c])
			// A symbol is anything that's not a digit and not a period
			if !unicode.IsDigit(ch) && ch != '.' {
				return true
			}
		}
	}
	return false
}

func part2(lines []string) int {
	// Map from gear position to list of adjacent numbers
	gearNumbers := make(map[Point][]int)

	for row, line := range lines {
		col := 0
		for col < len(line) {
			if unicode.IsDigit(rune(line[col])) {
				// Found start of a number
				numStart := col
				numStr := ""
				for col < len(line) && unicode.IsDigit(rune(line[col])) {
					numStr += string(line[col])
					col++
				}
				numEnd := col - 1

				// Find all adjacent gears
				num, _ := strconv.Atoi(numStr)
				gears := findAdjacentGears(lines, row, numStart, numEnd)
				for _, gear := range gears {
					gearNumbers[gear] = append(gearNumbers[gear], num)
				}
			} else {
				col++
			}
		}
	}

	// Calculate sum of gear ratios
	sum := 0
	for _, nums := range gearNumbers {
		if len(nums) == 2 {
			// This is a valid gear - adjacent to exactly 2 numbers
			sum += nums[0] * nums[1]
		}
	}

	return sum
}

func findAdjacentGears(lines []string, row, colStart, colEnd int) []Point {
	gears := []Point{}
	seen := make(map[Point]bool)

	// Check all adjacent cells (including diagonals)
	for r := row - 1; r <= row+1; r++ {
		if r < 0 || r >= len(lines) {
			continue
		}
		for c := colStart - 1; c <= colEnd+1; c++ {
			if c < 0 || c >= len(lines[r]) {
				continue
			}
			if r == row && c >= colStart && c <= colEnd {
				// This is the number itself, skip
				continue
			}

			if lines[r][c] == '*' {
				gear := Point{r, c}
				if !seen[gear] {
					gears = append(gears, gear)
					seen[gear] = true
				}
			}
		}
	}
	return gears
}
