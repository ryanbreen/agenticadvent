package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Problem struct {
	numbers  []int
	operator string
}

func readInput() (string, error) {
	exePath, err := os.Executable()
	if err != nil {
		return "", fmt.Errorf("failed to get executable path: %w", err)
	}
	exeDir := filepath.Dir(exePath)
	inputPath := filepath.Join(exeDir, "..", "input.txt")

	// Try relative path from current directory if executable path doesn't work
	data, err := os.ReadFile(inputPath)
	if err != nil {
		inputPath = "../input.txt"
		data, err = os.ReadFile(inputPath)
		if err != nil {
			return "", fmt.Errorf("failed to read input file: %w", err)
		}
	}

	return strings.TrimSpace(string(data)), nil
}

func parseProblems(lines []string) []Problem {
	if len(lines) == 0 {
		return []Problem{}
	}

	// Find the operator row (last non-empty row with only +, *, and spaces)
	opRowIdx := -1
	for i := len(lines) - 1; i >= 0; i-- {
		trimmed := strings.TrimSpace(lines[i])
		if trimmed == "" {
			continue
		}

		validOpRow := true
		for _, ch := range lines[i] {
			if ch != '+' && ch != '*' && ch != ' ' {
				validOpRow = false
				break
			}
		}

		if validOpRow {
			opRowIdx = i
			break
		}
	}

	if opRowIdx < 0 {
		return []Problem{}
	}

	opRow := lines[opRowIdx]
	numberRows := lines[:opRowIdx]

	// Find max width
	maxWidth := 0
	for _, line := range lines {
		if len(line) > maxWidth {
			maxWidth = len(line)
		}
	}

	// Pad all rows to the same width
	paddedNumberRows := make([]string, len(numberRows))
	for i, line := range numberRows {
		paddedNumberRows[i] = padRight(line, maxWidth)
	}
	paddedOpRow := padRight(opRow, maxWidth)

	// Find problem boundaries by looking for columns that are all spaces
	var problems []Problem
	col := 0

	for col < maxWidth {
		// Skip separator columns (all spaces)
		for col < maxWidth && isSpaceColumn(paddedNumberRows, paddedOpRow, col) {
			col++
		}

		if col >= maxWidth {
			break
		}

		// Find the end of this problem
		startCol := col
		for col < maxWidth {
			if isSpaceColumn(paddedNumberRows, paddedOpRow, col) {
				break
			}
			col++
		}

		endCol := col

		// Extract numbers and operator for this problem
		var numbers []int
		for _, row := range paddedNumberRows {
			numStr := strings.TrimSpace(row[startCol:endCol])
			if numStr != "" {
				num, err := strconv.Atoi(numStr)
				if err == nil {
					numbers = append(numbers, num)
				}
			}
		}

		opStr := strings.TrimSpace(paddedOpRow[startCol:endCol])
		if opStr != "" && len(numbers) > 0 {
			problems = append(problems, Problem{numbers: numbers, operator: opStr})
		}
	}

	return problems
}

func parseProblemsPartTwo(lines []string) []Problem {
	if len(lines) == 0 {
		return []Problem{}
	}

	// Find the operator row (last non-empty row with only +, *, and spaces)
	opRowIdx := -1
	for i := len(lines) - 1; i >= 0; i-- {
		trimmed := strings.TrimSpace(lines[i])
		if trimmed == "" {
			continue
		}

		validOpRow := true
		for _, ch := range lines[i] {
			if ch != '+' && ch != '*' && ch != ' ' {
				validOpRow = false
				break
			}
		}

		if validOpRow {
			opRowIdx = i
			break
		}
	}

	if opRowIdx < 0 {
		return []Problem{}
	}

	opRow := lines[opRowIdx]
	numberRows := lines[:opRowIdx]

	// Find max width
	maxWidth := 0
	for _, line := range lines {
		if len(line) > maxWidth {
			maxWidth = len(line)
		}
	}

	// Pad all rows to the same width
	paddedNumberRows := make([]string, len(numberRows))
	for i, line := range numberRows {
		paddedNumberRows[i] = padRight(line, maxWidth)
	}
	paddedOpRow := padRight(opRow, maxWidth)

	// Find problem boundaries by looking for columns that are all spaces
	var problems []Problem
	col := 0

	for col < maxWidth {
		// Skip separator columns (all spaces)
		for col < maxWidth && isSpaceColumn(paddedNumberRows, paddedOpRow, col) {
			col++
		}

		if col >= maxWidth {
			break
		}

		// Find the end of this problem
		startCol := col
		for col < maxWidth {
			if isSpaceColumn(paddedNumberRows, paddedOpRow, col) {
				break
			}
			col++
		}

		endCol := col

		// For Part 2: Read columns right-to-left, each column forms a number
		// reading top-to-bottom as most-to-least significant digit
		var numbers []int
		for c := endCol - 1; c >= startCol; c-- { // Right to left
			var digits []rune
			for _, row := range paddedNumberRows {
				ch := rune(row[c])
				if ch >= '0' && ch <= '9' {
					digits = append(digits, ch)
				}
			}
			if len(digits) > 0 {
				// Join digits to form number (top=most significant, bottom=least)
				numStr := string(digits)
				num, err := strconv.Atoi(numStr)
				if err == nil {
					numbers = append(numbers, num)
				}
			}
		}

		opStr := strings.TrimSpace(paddedOpRow[startCol:endCol])
		if opStr != "" && len(numbers) > 0 {
			problems = append(problems, Problem{numbers: numbers, operator: opStr})
		}
	}

	return problems
}

func padRight(s string, length int) string {
	if len(s) >= length {
		return s
	}
	return s + strings.Repeat(" ", length-len(s))
}

func isSpaceColumn(numberRows []string, opRow string, col int) bool {
	for _, row := range numberRows {
		if row[col] != ' ' {
			return false
		}
	}
	return opRow[col] == ' '
}

func solveProblem(numbers []int, op string) int {
	if op == "+" {
		sum := 0
		for _, n := range numbers {
			sum += n
		}
		return sum
	} else if op == "*" {
		product := 1
		for _, n := range numbers {
			product *= n
		}
		return product
	}
	return 0
}

func part1(lines []string) int {
	problems := parseProblems(lines)
	total := 0
	for _, problem := range problems {
		result := solveProblem(problem.numbers, problem.operator)
		total += result
	}
	return total
}

func part2(lines []string) int {
	problems := parseProblemsPartTwo(lines)
	total := 0
	for _, problem := range problems {
		result := solveProblem(problem.numbers, problem.operator)
		total += result
	}
	return total
}

func main() {
	input, err := readInput()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
	lines := strings.Split(input, "\n")

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
