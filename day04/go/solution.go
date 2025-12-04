package main

import (
	"bufio"
	"fmt"
	"os"
)

func readInput(filename string) ([][]rune, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var grid [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		grid = append(grid, []rune(line))
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return grid, nil
}

func countNeighbors(grid [][]rune, row, col int) int {
	count := 0
	rows := len(grid)
	cols := len(grid[0])

	// 8 directions: N, NE, E, SE, S, SW, W, NW
	directions := [][2]int{
		{-1, 0},  // N
		{-1, 1},  // NE
		{0, 1},   // E
		{1, 1},   // SE
		{1, 0},   // S
		{1, -1},  // SW
		{0, -1},  // W
		{-1, -1}, // NW
	}

	for _, dir := range directions {
		newRow := row + dir[0]
		newCol := col + dir[1]

		if newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols {
			if grid[newRow][newCol] == '@' {
				count++
			}
		}
	}

	return count
}

func part1(grid [][]rune) int {
	count := 0
	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[row]); col++ {
			if grid[row][col] == '@' {
				neighbors := countNeighbors(grid, row, col)
				if neighbors < 4 {
					count++
				}
			}
		}
	}
	return count
}

func findAccessibleRolls(grid [][]rune) [][2]int {
	var accessible [][2]int
	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[row]); col++ {
			if grid[row][col] == '@' {
				neighbors := countNeighbors(grid, row, col)
				if neighbors < 4 {
					accessible = append(accessible, [2]int{row, col})
				}
			}
		}
	}
	return accessible
}

func part2(grid [][]rune) int {
	// Make a deep copy of the grid
	gridCopy := make([][]rune, len(grid))
	for i := range grid {
		gridCopy[i] = make([]rune, len(grid[i]))
		copy(gridCopy[i], grid[i])
	}

	totalRemoved := 0

	for {
		accessible := findAccessibleRolls(gridCopy)
		if len(accessible) == 0 {
			break
		}

		// Remove all accessible rolls
		for _, pos := range accessible {
			gridCopy[pos[0]][pos[1]] = '.'
		}

		totalRemoved += len(accessible)
	}

	return totalRemoved
}

func main() {
	grid, err := readInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(grid))
	fmt.Printf("Part 2: %d\n", part2(grid))
}
