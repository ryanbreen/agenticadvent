package main

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

type Machine struct {
	ax, ay int64
	bx, by int64
	px, py int64
}

func parseMachines(text string) []Machine {
	machines := []Machine{}
	blocks := strings.Split(strings.TrimSpace(text), "\n\n")

	buttonARegex := regexp.MustCompile(`Button A: X\+(\d+), Y\+(\d+)`)
	buttonBRegex := regexp.MustCompile(`Button B: X\+(\d+), Y\+(\d+)`)
	prizeRegex := regexp.MustCompile(`Prize: X=(\d+), Y=(\d+)`)

	for _, block := range blocks {
		lines := strings.Split(strings.TrimSpace(block), "\n")
		if len(lines) < 3 {
			continue
		}

		// Parse Button A
		aMatch := buttonARegex.FindStringSubmatch(lines[0])
		ax, _ := strconv.ParseInt(aMatch[1], 10, 64)
		ay, _ := strconv.ParseInt(aMatch[2], 10, 64)

		// Parse Button B
		bMatch := buttonBRegex.FindStringSubmatch(lines[1])
		bx, _ := strconv.ParseInt(bMatch[1], 10, 64)
		by, _ := strconv.ParseInt(bMatch[2], 10, 64)

		// Parse Prize
		pMatch := prizeRegex.FindStringSubmatch(lines[2])
		px, _ := strconv.ParseInt(pMatch[1], 10, 64)
		py, _ := strconv.ParseInt(pMatch[2], 10, 64)

		machines = append(machines, Machine{ax, ay, bx, by, px, py})
	}

	return machines
}

func solveMachine(ax, ay, bx, by, px, py int64, maxPresses *int64) *int64 {
	// Cramer's rule for system of linear equations:
	// a*ax + b*bx = px
	// a*ay + b*by = py
	//
	// det = ax*by - ay*bx
	// a = (px*by - py*bx) / det
	// b = (ax*py - ay*px) / det

	det := ax*by - ay*bx

	if det == 0 {
		return nil // No unique solution
	}

	// Calculate numerators
	aNum := px*by - py*bx
	bNum := ax*py - ay*px

	// Check if solutions are integers
	if aNum%det != 0 || bNum%det != 0 {
		return nil
	}

	a := aNum / det
	b := bNum / det

	// Check non-negative
	if a < 0 || b < 0 {
		return nil
	}

	// Check max presses constraint (Part 1)
	if maxPresses != nil && (a > *maxPresses || b > *maxPresses) {
		return nil
	}

	cost := 3*a + b
	return &cost
}

func part1(machines []Machine) int64 {
	total := int64(0)
	maxPresses := int64(100)

	for _, m := range machines {
		if cost := solveMachine(m.ax, m.ay, m.bx, m.by, m.px, m.py, &maxPresses); cost != nil {
			total += *cost
		}
	}

	return total
}

func part2(machines []Machine) int64 {
	total := int64(0)
	offset := int64(10_000_000_000_000)

	for _, m := range machines {
		// Shift prize coordinates
		if cost := solveMachine(m.ax, m.ay, m.bx, m.by, m.px+offset, m.py+offset, nil); cost != nil {
			total += *cost
		}
	}

	return total
}

func main() {
	// Read input file
	execPath, _ := os.Executable()
	execDir := filepath.Dir(execPath)
	inputPath := filepath.Join(execDir, "..", "input.txt")

	// Fallback to relative path if executable path doesn't work
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = "../input.txt"
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
		os.Exit(1)
	}

	machines := parseMachines(string(data))

	fmt.Printf("Part 1: %d\n", part1(machines))
	fmt.Printf("Part 2: %d\n", part2(machines))
}
