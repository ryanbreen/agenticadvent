// Run: go run solution.go
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func readInput() ([]string, error) {
	file, err := os.Open("../input.txt")
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != "" {
			lines = append(lines, line)
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return lines, nil
}

func part1(lines []string) int {
	position := 50 // Starting position
	zeroCount := 0

	for _, line := range lines {
		direction := line[0]
		distance, _ := strconv.Atoi(line[1:])

		if direction == 'L' {
			position = (position - distance) % 100
			if position < 0 {
				position += 100
			}
		} else { // direction == 'R'
			position = (position + distance) % 100
		}

		if position == 0 {
			zeroCount++
		}
	}

	return zeroCount
}

func part2(lines []string) int {
	position := 50 // Starting position
	zeroCount := 0

	for _, line := range lines {
		direction := line[0]
		distance, _ := strconv.Atoi(line[1:])

		if direction == 'L' {
			// Moving left (toward lower numbers)
			// We hit 0 after exactly 'position' steps, then every 100 steps after that
			if position > 0 && distance >= position {
				zeroCount += 1 + (distance-position)/100
			} else if position == 0 && distance >= 100 {
				// Starting from 0, we hit it again after 100 steps, then every 100 steps
				zeroCount += distance / 100
			}
		} else { // direction == 'R'
			// Moving right (toward higher numbers)
			// We hit 0 after (100 - position) steps, then every 100 steps after that
			if position > 0 {
				stepsToZero := 100 - position
				if distance >= stepsToZero {
					zeroCount += 1 + (distance-stepsToZero)/100
				}
			} else { // position == 0
				// Starting from 0, we hit it again after 100 steps, then every 100 steps
				if distance >= 100 {
					zeroCount += distance / 100
				}
			}
		}

		// Update position
		if direction == 'L' {
			position = (position - distance) % 100
			if position < 0 {
				position += 100
			}
		} else {
			position = (position + distance) % 100
		}
	}

	return zeroCount
}

func main() {
	lines, err := readInput()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
