package main

import (
	"fmt"
	"os"
	"strings"
)

// parseInput parses the input text into a list of patterns
func parseInput(text string) [][]string {
	blocks := strings.Split(strings.TrimSpace(text), "\n\n")
	patterns := make([][]string, len(blocks))
	for i, block := range blocks {
		patterns[i] = strings.Split(block, "\n")
	}
	return patterns
}

// findVerticalReflection finds vertical line of reflection
// Returns columns to the left, or 0 if none
func findVerticalReflection(pattern []string) int {
	if len(pattern) == 0 {
		return 0
	}
	width := len(pattern[0])

	for col := 1; col < width; col++ {
		isReflection := true
		for _, row := range pattern {
			// Compare left side with right side (mirrored)
			left := row[:col]
			right := row[col:]

			// Reverse the left side for comparison
			leftReversed := reverse(left)

			// Compare the overlapping parts
			minLen := min(len(leftReversed), len(right))
			if leftReversed[:minLen] != right[:minLen] {
				isReflection = false
				break
			}
		}
		if isReflection {
			return col
		}
	}
	return 0
}

// findHorizontalReflection finds horizontal line of reflection
// Returns rows above, or 0 if none
func findHorizontalReflection(pattern []string) int {
	if len(pattern) == 0 {
		return 0
	}
	height := len(pattern)

	for row := 1; row < height; row++ {
		isReflection := true
		// Compare top with bottom (mirrored)
		top := pattern[:row]
		bottom := pattern[row:]

		// Reverse the top for comparison
		topReversed := reverseSlice(top)

		minLen := min(len(topReversed), len(bottom))
		for i := 0; i < minLen; i++ {
			if topReversed[i] != bottom[i] {
				isReflection = false
				break
			}
		}
		if isReflection {
			return row
		}
	}
	return 0
}

// summarizePattern gets the summary value for a pattern
func summarizePattern(pattern []string) int {
	v := findVerticalReflection(pattern)
	if v > 0 {
		return v
	}
	h := findHorizontalReflection(pattern)
	return h * 100
}

// part1 calculates the sum of all pattern summaries
func part1(patterns [][]string) int {
	total := 0
	for _, pattern := range patterns {
		total += summarizePattern(pattern)
	}
	return total
}

// countDifferences counts character differences between two strings
func countDifferences(s1, s2 string) int {
	count := 0
	minLen := min(len(s1), len(s2))
	for i := 0; i < minLen; i++ {
		if s1[i] != s2[i] {
			count++
		}
	}
	return count
}

// findVerticalReflectionWithSmudge finds vertical line with exactly one smudge fix needed
func findVerticalReflectionWithSmudge(pattern []string) int {
	if len(pattern) == 0 {
		return 0
	}
	width := len(pattern[0])

	for col := 1; col < width; col++ {
		totalDiff := 0
		for _, row := range pattern {
			left := row[:col]
			right := row[col:]

			leftReversed := reverse(left)

			minLen := min(len(leftReversed), len(right))
			totalDiff += countDifferences(leftReversed[:minLen], right[:minLen])

			if totalDiff > 1 {
				break
			}
		}
		if totalDiff == 1 {
			return col
		}
	}
	return 0
}

// findHorizontalReflectionWithSmudge finds horizontal line with exactly one smudge fix needed
func findHorizontalReflectionWithSmudge(pattern []string) int {
	if len(pattern) == 0 {
		return 0
	}
	height := len(pattern)

	for row := 1; row < height; row++ {
		totalDiff := 0
		top := pattern[:row]
		bottom := pattern[row:]

		topReversed := reverseSlice(top)

		minLen := min(len(topReversed), len(bottom))
		for i := 0; i < minLen; i++ {
			totalDiff += countDifferences(topReversed[i], bottom[i])
			if totalDiff > 1 {
				break
			}
		}
		if totalDiff == 1 {
			return row
		}
	}
	return 0
}

// summarizePatternWithSmudge gets the summary value for a pattern with smudge fix
func summarizePatternWithSmudge(pattern []string) int {
	v := findVerticalReflectionWithSmudge(pattern)
	if v > 0 {
		return v
	}
	h := findHorizontalReflectionWithSmudge(pattern)
	return h * 100
}

// part2 calculates the sum with smudge fixes
func part2(patterns [][]string) int {
	total := 0
	for _, pattern := range patterns {
		total += summarizePatternWithSmudge(pattern)
	}
	return total
}

// Helper functions
func reverse(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func reverseSlice(s []string) []string {
	result := make([]string, len(s))
	for i, v := range s {
		result[len(s)-1-i] = v
	}
	return result
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	text := string(data)
	patterns := parseInput(text)

	fmt.Printf("Part 1: %d\n", part1(patterns))
	fmt.Printf("Part 2: %d\n", part2(patterns))
}
