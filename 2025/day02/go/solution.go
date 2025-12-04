// Run: go run solution.go
package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func readInput() (string, error) {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(data)), nil
}

func isInvalidIDPart1(num int) bool {
	// Check if a number is invalid (a pattern repeated exactly twice)
	s := strconv.Itoa(num)
	length := len(s)

	// Must have even length to be repeated twice
	if length%2 != 0 {
		return false
	}

	// Check if it starts with 0 (leading zeros not allowed)
	if s[0] == '0' {
		return false
	}

	// Split in half and check if both halves are identical
	mid := length / 2
	firstHalf := s[:mid]
	secondHalf := s[mid:]

	return firstHalf == secondHalf
}

func isInvalidIDPart2(num int) bool {
	// Check if a number is invalid (a pattern repeated at least twice)
	s := strconv.Itoa(num)
	length := len(s)

	// Check if it starts with 0 (leading zeros not allowed)
	if s[0] == '0' {
		return false
	}

	// Try all possible pattern lengths from 1 to length//2
	// The pattern must be repeated at least twice, so max pattern length is length//2
	for patternLength := 1; patternLength <= length/2; patternLength++ {
		// Check if the string length is divisible by patternLength
		if length%patternLength == 0 {
			pattern := s[:patternLength]
			repetitions := length / patternLength

			// Check if repeating the pattern gives us the original string
			repeated := strings.Repeat(pattern, repetitions)
			if repeated == s {
				return true
			}
		}
	}

	return false
}

func parseRanges(input string) ([][2]int, error) {
	var ranges [][2]int
	parts := strings.Split(input, ",")

	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}

		// Split on dash to get start and end
		dashIndex := strings.Index(part, "-")
		if dashIndex == -1 {
			continue
		}

		start, err1 := strconv.Atoi(part[:dashIndex])
		end, err2 := strconv.Atoi(part[dashIndex+1:])

		if err1 != nil || err2 != nil {
			return nil, fmt.Errorf("error parsing range %s", part)
		}

		ranges = append(ranges, [2]int{start, end})
	}

	return ranges, nil
}

func part1(input string) (int, error) {
	ranges, err := parseRanges(input)
	if err != nil {
		return 0, err
	}

	total := 0
	for _, r := range ranges {
		start, end := r[0], r[1]
		for num := start; num <= end; num++ {
			if isInvalidIDPart1(num) {
				total += num
			}
		}
	}

	return total, nil
}

func part2(input string) (int, error) {
	ranges, err := parseRanges(input)
	if err != nil {
		return 0, err
	}

	total := 0
	for _, r := range ranges {
		start, end := r[0], r[1]
		for num := start; num <= end; num++ {
			if isInvalidIDPart2(num) {
				total += num
			}
		}
	}

	return total, nil
}

func main() {
	input, err := readInput()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	result1, err := part1(input)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error in part1: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Part 1: %d\n", result1)

	result2, err := part2(input)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error in part2: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Part 2: %d\n", result2)
}
