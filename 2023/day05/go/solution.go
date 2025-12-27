package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

// MapRange represents a single mapping range
type MapRange struct {
	dstStart int
	srcStart int
	length   int
}

// Range represents a range of values [start, end)
type Range struct {
	start int
	end   int
}

func parseInput(text string) ([]int, [][]MapRange) {
	sections := strings.Split(strings.TrimSpace(text), "\n\n")

	// Parse seeds
	seedParts := strings.Split(sections[0], ": ")
	seedStrs := strings.Fields(seedParts[1])
	seeds := make([]int, len(seedStrs))
	for i, s := range seedStrs {
		seeds[i], _ = strconv.Atoi(s)
	}

	// Parse maps
	maps := make([][]MapRange, len(sections)-1)
	for i, section := range sections[1:] {
		lines := strings.Split(strings.TrimSpace(section), "\n")
		ranges := make([]MapRange, len(lines)-1)
		for j, line := range lines[1:] { // Skip header
			parts := strings.Fields(line)
			dstStart, _ := strconv.Atoi(parts[0])
			srcStart, _ := strconv.Atoi(parts[1])
			length, _ := strconv.Atoi(parts[2])
			ranges[j] = MapRange{dstStart, srcStart, length}
		}
		maps[i] = ranges
	}

	return seeds, maps
}

func applyMap(value int, ranges []MapRange) int {
	for _, r := range ranges {
		if value >= r.srcStart && value < r.srcStart+r.length {
			return r.dstStart + (value - r.srcStart)
		}
	}
	return value
}

func seedToLocation(seed int, maps [][]MapRange) int {
	value := seed
	for _, mapRanges := range maps {
		value = applyMap(value, mapRanges)
	}
	return value
}

func part1(seeds []int, maps [][]MapRange) int {
	minLoc := seedToLocation(seeds[0], maps)
	for _, seed := range seeds[1:] {
		loc := seedToLocation(seed, maps)
		if loc < minLoc {
			minLoc = loc
		}
	}
	return minLoc
}

func applyMapToRanges(inputRanges []Range, mapRanges []MapRange) []Range {
	var result []Range

	for _, ir := range inputRanges {
		remaining := []Range{{ir.start, ir.end}}

		for _, mr := range mapRanges {
			srcEnd := mr.srcStart + mr.length
			var newRemaining []Range

			for _, r := range remaining {
				// Part before the map range (unmapped)
				if r.start < mr.srcStart {
					end := r.end
					if end > mr.srcStart {
						end = mr.srcStart
					}
					newRemaining = append(newRemaining, Range{r.start, end})
				}

				// Part within the map range (mapped)
				overlapStart := r.start
				if overlapStart < mr.srcStart {
					overlapStart = mr.srcStart
				}
				overlapEnd := r.end
				if overlapEnd > srcEnd {
					overlapEnd = srcEnd
				}
				if overlapStart < overlapEnd {
					offset := mr.dstStart - mr.srcStart
					result = append(result, Range{overlapStart + offset, overlapEnd + offset})
				}

				// Part after the map range (unmapped)
				if r.end > srcEnd {
					start := r.start
					if start < srcEnd {
						start = srcEnd
					}
					newRemaining = append(newRemaining, Range{start, r.end})
				}
			}

			remaining = newRemaining
		}

		// Any remaining parts are unmapped (identity)
		result = append(result, remaining...)
	}

	return result
}

func part2(seeds []int, maps [][]MapRange) int {
	// Convert seeds to ranges: pairs of (start, start + length)
	var ranges []Range
	for i := 0; i < len(seeds); i += 2 {
		start := seeds[i]
		length := seeds[i+1]
		ranges = append(ranges, Range{start, start + length})
	}

	// Apply each map to the ranges
	for _, mapRanges := range maps {
		ranges = applyMapToRanges(ranges, mapRanges)
	}

	// Find minimum start of any range
	minStart := ranges[0].start
	for _, r := range ranges[1:] {
		if r.start < minStart {
			minStart = r.start
		}
	}
	return minStart
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	seeds, maps := parseInput(string(data))

	fmt.Println("Part 1:", part1(seeds, maps))
	fmt.Println("Part 2:", part2(seeds, maps))
}
