package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Range struct {
	start int
	end   int
}

// must is a helper that panics on error, useful for mandatory conversions
func must(val int, err error) int {
	if err != nil {
		panic(err)
	}
	return val
}

func main() {
	// Read input file
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		panic(err)
	}

	input := strings.TrimSpace(string(data))
	lines := strings.Split(input, "\n")

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}

func part1(lines []string) int {
	// Find the blank line separator
	blankIdx := -1
	for i, line := range lines {
		if line == "" {
			blankIdx = i
			break
		}
	}

	// Parse ranges from the first section
	var ranges []Range
	for i := 0; i < blankIdx; i++ {
		parts := strings.Split(lines[i], "-")
		start := must(strconv.Atoi(parts[0]))
		end := must(strconv.Atoi(parts[1]))
		ranges = append(ranges, Range{start, end})
	}

	// Parse ingredient IDs from the second section
	var ingredientIDs []int
	for i := blankIdx + 1; i < len(lines); i++ {
		if lines[i] != "" {
			id := must(strconv.Atoi(lines[i]))
			ingredientIDs = append(ingredientIDs, id)
		}
	}

	// Count how many ingredient IDs fall within any range
	freshCount := 0
	for _, ingredientID := range ingredientIDs {
		for _, r := range ranges {
			if r.start <= ingredientID && ingredientID <= r.end {
				freshCount++
				break // Found a match, no need to check other ranges
			}
		}
	}

	return freshCount
}

func part2(lines []string) int {
	// Find the blank line separator
	blankIdx := -1
	for i, line := range lines {
		if line == "" {
			blankIdx = i
			break
		}
	}

	// Parse ranges from the first section
	var ranges []Range
	for i := 0; i < blankIdx; i++ {
		parts := strings.Split(lines[i], "-")
		start := must(strconv.Atoi(parts[0]))
		end := must(strconv.Atoi(parts[1]))
		ranges = append(ranges, Range{start, end})
	}

	// Sort ranges by start position
	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i].start < ranges[j].start
	})

	// Merge overlapping ranges
	var merged []Range
	for _, r := range ranges {
		if len(merged) > 0 && r.start <= merged[len(merged)-1].end+1 {
			// Overlapping or adjacent - merge with the last range
			lastIdx := len(merged) - 1
			if r.end > merged[lastIdx].end {
				merged[lastIdx].end = r.end
			}
		} else {
			// No overlap - add as new range
			merged = append(merged, r)
		}
	}

	// Count total unique IDs covered by merged ranges
	totalCount := 0
	for _, r := range merged {
		totalCount += (r.end - r.start + 1)
	}

	return totalCount
}
