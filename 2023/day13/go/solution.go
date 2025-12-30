package main

import (
	"fmt"
	"os"
	"strings"
)

// parsePatterns parses the input text into a list of patterns
func parsePatterns(input string) [][]string {
	blocks := strings.Split(strings.TrimSpace(input), "\n\n")
	patterns := make([][]string, len(blocks))
	for i, block := range blocks {
		patterns[i] = strings.Split(block, "\n")
	}
	return patterns
}

// countMirrorDifferences counts character differences between mirrored portions
// of a string split at the given position
func countMirrorDifferences(s string, splitPos int) int {
	differences := 0
	left, right := splitPos-1, splitPos
	for left >= 0 && right < len(s) {
		if s[left] != s[right] {
			differences++
		}
		left--
		right++
	}
	return differences
}

// findVerticalReflection finds a vertical line of reflection where the total
// character differences across all rows equals targetDiff.
// Returns columns to the left of the reflection line, or 0 if none found.
func findVerticalReflection(pattern []string, targetDiff int) int {
	if len(pattern) == 0 {
		return 0
	}
	width := len(pattern[0])

	for col := 1; col < width; col++ {
		totalDiff := 0
		for _, row := range pattern {
			totalDiff += countMirrorDifferences(row, col)
			if totalDiff > targetDiff {
				break
			}
		}
		if totalDiff == targetDiff {
			return col
		}
	}
	return 0
}

// countRowDifferences counts character differences between two rows
func countRowDifferences(row1, row2 string) int {
	differences := 0
	for i := 0; i < len(row1) && i < len(row2); i++ {
		if row1[i] != row2[i] {
			differences++
		}
	}
	return differences
}

// findHorizontalReflection finds a horizontal line of reflection where the total
// character differences across mirrored rows equals targetDiff.
// Returns rows above the reflection line, or 0 if none found.
func findHorizontalReflection(pattern []string, targetDiff int) int {
	height := len(pattern)
	if height == 0 {
		return 0
	}

	for row := 1; row < height; row++ {
		totalDiff := 0
		above, below := row-1, row
		for above >= 0 && below < height {
			totalDiff += countRowDifferences(pattern[above], pattern[below])
			if totalDiff > targetDiff {
				break
			}
			above--
			below++
		}
		if totalDiff == targetDiff {
			return row
		}
	}
	return 0
}

// summarizePattern calculates the summary value for a pattern.
// targetDiff=0 for perfect reflection (Part 1), targetDiff=1 for smudge (Part 2).
func summarizePattern(pattern []string, targetDiff int) int {
	if v := findVerticalReflection(pattern, targetDiff); v > 0 {
		return v
	}
	return findHorizontalReflection(pattern, targetDiff) * 100
}

// solve calculates the sum of all pattern summaries with the given target difference
func solve(patterns [][]string, targetDiff int) int {
	total := 0
	for _, pattern := range patterns {
		total += summarizePattern(pattern, targetDiff)
	}
	return total
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	patterns := parsePatterns(string(data))

	fmt.Printf("Part 1: %d\n", solve(patterns, 0))
	fmt.Printf("Part 2: %d\n", solve(patterns, 1))
}
