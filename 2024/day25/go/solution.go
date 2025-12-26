package main

import (
	"fmt"
	"os"
	"strings"
)

// parseInput parses schematics into locks and keys
func parseInput(text string) ([][]int, [][]int) {
	locks := [][]int{}
	keys := [][]int{}

	schematics := strings.Split(strings.TrimSpace(text), "\n\n")

	for _, schematic := range schematics {
		lines := strings.Split(strings.TrimSpace(schematic), "\n")

		// Lock: top row is all #, bottom is all .
		// Key: top row is all ., bottom is all #
		if lines[0] == "#####" {
			// It's a lock - count # from top (excluding top row)
			heights := make([]int, 5)
			for col := 0; col < 5; col++ {
				height := 0
				for row := 1; row < 7; row++ { // rows 1-6
					if lines[row][col] == '#' {
						height++
					} else {
						break
					}
				}
				heights[col] = height
			}
			locks = append(locks, heights)
		} else {
			// It's a key - count # from bottom (excluding bottom row)
			heights := make([]int, 5)
			for col := 0; col < 5; col++ {
				height := 0
				for row := 5; row >= 0; row-- { // rows 5 down to 0
					if lines[row][col] == '#' {
						height++
					} else {
						break
					}
				}
				heights[col] = height
			}
			keys = append(keys, heights)
		}
	}

	return locks, keys
}

// fits checks if a key fits a lock (no column exceeds 5)
func fits(lock, key []int) bool {
	for i := 0; i < 5; i++ {
		if lock[i]+key[i] > 5 {
			return false
		}
	}
	return true
}

// part1 counts unique lock/key pairs that fit together
func part1(locks, keys [][]int) int {
	count := 0
	for _, lock := range locks {
		for _, key := range keys {
			if fits(lock, key) {
				count++
			}
		}
	}
	return count
}

func main() {
	// Read input file
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	text := string(data)
	locks, keys := parseInput(text)

	answer1 := part1(locks, keys)
	fmt.Printf("Part 1: %d\n", answer1)

	// Day 25 typically only has Part 1
	fmt.Println("Part 2: Merry Christmas!")
}
