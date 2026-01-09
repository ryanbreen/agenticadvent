package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
)

// Element stores the original index and value
type Element struct {
	origIdx int
	value   int
}

func parseInput(filename string) ([]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var numbers []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		n, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		numbers = append(numbers, n)
	}
	return numbers, scanner.Err()
}

func mix(numbers []int, times int) []int {
	n := len(numbers)

	// Create indexed elements
	indexed := make([]Element, n)
	for i, v := range numbers {
		indexed[i] = Element{origIdx: i, value: v}
	}

	for round := 0; round < times; round++ {
		for origIdx := 0; origIdx < n; origIdx++ {
			// Find current position of this element
			var currPos int
			for i, elem := range indexed {
				if elem.origIdx == origIdx {
					currPos = i
					break
				}
			}

			// Get the element and its value
			elem := indexed[currPos]
			val := elem.value

			// Remove from current position
			indexed = append(indexed[:currPos], indexed[currPos+1:]...)

			// Calculate new position (modulo n-1 because we removed the element)
			newPos := (currPos + val) % (n - 1)
			if newPos < 0 {
				newPos += n - 1
			}

			// Insert at new position
			indexed = append(indexed[:newPos], append([]Element{elem}, indexed[newPos:]...)...)
		}
	}

	// Extract values
	result := make([]int, n)
	for i, elem := range indexed {
		result[i] = elem.value
	}
	return result
}

func groveCoordinates(mixed []int) int {
	n := len(mixed)

	// Find zero index
	var zeroIdx int
	for i, v := range mixed {
		if v == 0 {
			zeroIdx = i
			break
		}
	}

	// Sum values at offsets 1000, 2000, 3000 from zero
	offsets := []int{1000, 2000, 3000}
	sum := 0
	for _, offset := range offsets {
		sum += mixed[(zeroIdx+offset)%n]
	}
	return sum
}

func part1(numbers []int) int {
	mixed := mix(numbers, 1)
	return groveCoordinates(mixed)
}

func part2(numbers []int) int {
	decryptionKey := 811589153

	// Apply decryption key
	decrypted := make([]int, len(numbers))
	for i, n := range numbers {
		decrypted[i] = n * decryptionKey
	}

	mixed := mix(decrypted, 10)
	return groveCoordinates(mixed)
}

func main() {
	// Get the directory of the executable
	execPath, err := os.Executable()
	if err != nil {
		// Fallback to using the current working directory approach
		execPath = os.Args[0]
	}
	dir := filepath.Dir(execPath)
	inputFile := filepath.Join(dir, "..", "input.txt")

	// Try relative path from source file location
	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		// Try from current working directory
		inputFile = "../input.txt"
	}

	numbers, err := parseInput(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(numbers))
	fmt.Println("Part 2:", part2(numbers))
}
