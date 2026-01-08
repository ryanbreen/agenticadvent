package main

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
)

func parseInput(filename string) ([]int, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	var elves []int
	groups := strings.Split(strings.TrimSpace(string(content)), "\n\n")

	for _, group := range groups {
		total := 0
		for _, line := range strings.Split(group, "\n") {
			if line == "" {
				continue
			}
			calories, err := strconv.Atoi(line)
			if err != nil {
				return nil, err
			}
			total += calories
		}
		elves = append(elves, total)
	}

	return elves, nil
}

func part1(elves []int) int {
	maxCalories := 0
	for _, calories := range elves {
		if calories > maxCalories {
			maxCalories = calories
		}
	}
	return maxCalories
}

func part2(elves []int) int {
	sorted := make([]int, len(elves))
	copy(sorted, elves)
	sort.Sort(sort.Reverse(sort.IntSlice(sorted)))

	total := 0
	for i := 0; i < 3 && i < len(sorted); i++ {
		total += sorted[i]
	}
	return total
}

func main() {
	exePath, err := os.Executable()
	if err != nil {
		// Fallback to using working directory
		exePath = "."
	}
	dir := filepath.Dir(exePath)

	// When running with 'go run', the executable is in a temp directory
	// So we check if input.txt exists relative to executable, otherwise use cwd
	inputFile := filepath.Join(dir, "..", "input.txt")
	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		inputFile = filepath.Join(".", "..", "input.txt")
	}

	elves, err := parseInput(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(elves))
	fmt.Println("Part 2:", part2(elves))
}
