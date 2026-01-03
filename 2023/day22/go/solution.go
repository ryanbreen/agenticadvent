// Day 22: Sand Slabs - 3D falling bricks simulation
package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

// Brick represents a 3D brick with coordinates
type Brick struct {
	x1, y1, z1 int
	x2, y2, z2 int
}

// Coord3D represents a 3D coordinate
type Coord3D struct {
	x, y, z int
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func parseInput(filename string) ([]Brick, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var bricks []Brick
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		parts := strings.Split(line, "~")
		left := strings.Split(parts[0], ",")
		right := strings.Split(parts[1], ",")

		x1, _ := strconv.Atoi(left[0])
		y1, _ := strconv.Atoi(left[1])
		z1, _ := strconv.Atoi(left[2])
		x2, _ := strconv.Atoi(right[0])
		y2, _ := strconv.Atoi(right[1])
		z2, _ := strconv.Atoi(right[2])

		// Ensure z1 <= z2 for consistent processing
		if z1 > z2 {
			x1, y1, z1, x2, y2, z2 = x2, y2, z2, x1, y1, z1
		}

		bricks = append(bricks, Brick{x1, y1, z1, x2, y2, z2})
	}

	return bricks, scanner.Err()
}

// settleBricks simulates bricks falling and settling
// Returns support relationships: supports[i] = bricks supported by i, supporters[i] = bricks supporting i
func settleBricks(bricks []Brick) ([]Brick, map[int]map[int]bool, map[int]map[int]bool) {
	n := len(bricks)

	// Create indexed slice for sorting
	type indexedBrick struct {
		idx   int
		brick Brick
	}
	sortedBricks := make([]indexedBrick, n)
	for i, b := range bricks {
		sortedBricks[i] = indexedBrick{i, b}
	}

	// Sort by minimum z coordinate
	sort.Slice(sortedBricks, func(i, j int) bool {
		return sortedBricks[i].brick.z1 < sortedBricks[j].brick.z1
	})

	// Track occupied cells: (x, y, z) -> brick index
	occupied := make(map[Coord3D]int)
	settled := make([]Brick, n)

	// supports[i] = set of brick indices that brick i supports (bricks above)
	// supporters[i] = set of brick indices that support brick i (bricks below)
	supports := make(map[int]map[int]bool)
	supporters := make(map[int]map[int]bool)

	for i := 0; i < n; i++ {
		supports[i] = make(map[int]bool)
		supporters[i] = make(map[int]bool)
	}

	for _, ib := range sortedBricks {
		origIdx := ib.idx
		brick := ib.brick
		x1, y1, z1 := brick.x1, brick.y1, brick.z1
		x2, y2, z2 := brick.x2, brick.y2, brick.z2

		// Find the maximum drop (to z=1)
		drop := z1 - 1

		// Check xy footprint of this brick
		for x := min(x1, x2); x <= max(x1, x2); x++ {
			for y := min(y1, y2); y <= max(y1, y2); y++ {
				// Check each z level below the brick
				for z := z1 - 1; z >= 1; z-- {
					if _, exists := occupied[Coord3D{x, y, z}]; exists {
						drop = min(drop, z1-z-1)
						break
					}
				}
			}
		}

		// Drop the brick
		newZ1 := z1 - drop
		newZ2 := z2 - drop
		newBrick := Brick{x1, y1, newZ1, x2, y2, newZ2}
		settled[origIdx] = newBrick

		// Mark cells as occupied and find supporters
		for x := min(x1, x2); x <= max(x1, x2); x++ {
			for y := min(y1, y2); y <= max(y1, y2); y++ {
				// Check if there's a brick directly below
				if supporterIdx, exists := occupied[Coord3D{x, y, newZ1 - 1}]; exists {
					supporters[origIdx][supporterIdx] = true
					supports[supporterIdx][origIdx] = true
				}

				// Mark all cells of this brick as occupied
				for z := newZ1; z <= newZ2; z++ {
					occupied[Coord3D{x, y, z}] = origIdx
				}
			}
		}
	}

	return settled, supports, supporters
}

func part1(bricks []Brick) int {
	_, supports, supporters := settleBricks(bricks)

	safeCount := 0
	for i := 0; i < len(bricks); i++ {
		// Brick i can be safely removed if every brick it supports
		// has at least one other supporter
		canRemove := true
		for supported := range supports[i] {
			if len(supporters[supported]) == 1 {
				canRemove = false
				break
			}
		}
		if canRemove {
			safeCount++
		}
	}

	return safeCount
}

func part2(bricks []Brick) int {
	_, supports, supporters := settleBricks(bricks)

	totalFalls := 0

	for i := 0; i < len(bricks); i++ {
		// Simulate removing brick i and count chain reaction using BFS
		falling := make(map[int]bool)
		falling[i] = true
		queue := []int{i}

		for len(queue) > 0 {
			brick := queue[0]
			queue = queue[1:]

			// Check all bricks that this brick supports
			for supported := range supports[brick] {
				if falling[supported] {
					continue
				}

				// This brick falls if all its supporters have fallen
				allFallen := true
				for supporter := range supporters[supported] {
					if !falling[supporter] {
						allFallen = false
						break
					}
				}

				if allFallen {
					falling[supported] = true
					queue = append(queue, supported)
				}
			}
		}

		// Don't count the initial brick we removed
		totalFalls += len(falling) - 1
	}

	return totalFalls
}

func main() {
	inputPath := "../input.txt"
	bricks, err := parseInput(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(bricks))
	fmt.Printf("Part 2: %d\n", part2(bricks))
}
