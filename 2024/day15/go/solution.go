package main

import (
	"fmt"
	"os"
	"strings"
)

type Point struct {
	r, c int
}

func parseInput(text string) ([][]rune, string) {
	parts := strings.Split(strings.TrimSpace(text), "\n\n")
	gridLines := strings.Split(parts[0], "\n")

	grid := make([][]rune, len(gridLines))
	for i, line := range gridLines {
		grid[i] = []rune(line)
	}

	moves := strings.ReplaceAll(parts[1], "\n", "")
	return grid, moves
}

func findRobot(grid [][]rune) Point {
	for r, row := range grid {
		for c, cell := range row {
			if cell == '@' {
				return Point{r, c}
			}
		}
	}
	return Point{-1, -1}
}

func moveRobot(grid [][]rune, robotPos Point, direction rune) Point {
	deltas := map[rune]Point{
		'<': {0, -1},
		'>': {0, 1},
		'^': {-1, 0},
		'v': {1, 0},
	}

	dr := deltas[direction].r
	dc := deltas[direction].c
	r, c := robotPos.r, robotPos.c
	nr, nc := r+dr, c+dc

	if grid[nr][nc] == '#' {
		return robotPos
	}

	if grid[nr][nc] == '.' {
		grid[r][c] = '.'
		grid[nr][nc] = '@'
		return Point{nr, nc}
	}

	if grid[nr][nc] == 'O' {
		checkR, checkC := nr, nc
		for grid[checkR][checkC] == 'O' {
			checkR += dr
			checkC += dc
		}

		if grid[checkR][checkC] == '#' {
			return robotPos
		}

		grid[checkR][checkC] = 'O'
		grid[r][c] = '.'
		grid[nr][nc] = '@'
		return Point{nr, nc}
	}

	return robotPos
}

func calculateGPS(grid [][]rune, boxChar rune) int {
	total := 0
	for r, row := range grid {
		for c, cell := range row {
			if cell == boxChar {
				total += 100*r + c
			}
		}
	}
	return total
}

func part1(text string) int {
	grid, moves := parseInput(text)
	robotPos := findRobot(grid)

	for _, move := range moves {
		robotPos = moveRobot(grid, robotPos, move)
	}

	return calculateGPS(grid, 'O')
}

func scaleGrid(grid [][]rune) [][]rune {
	newGrid := make([][]rune, len(grid))
	for i, row := range grid {
		newRow := make([]rune, 0, len(row)*2)
		for _, cell := range row {
			switch cell {
			case '#':
				newRow = append(newRow, '#', '#')
			case 'O':
				newRow = append(newRow, '[', ']')
			case '.':
				newRow = append(newRow, '.', '.')
			case '@':
				newRow = append(newRow, '@', '.')
			}
		}
		newGrid[i] = newRow
	}
	return newGrid
}

func canMoveBoxVertical(grid [][]rune, boxLeftC, r, dr int) bool {
	nr := r + dr
	leftC, rightC := boxLeftC, boxLeftC+1

	leftTarget := grid[nr][leftC]
	rightTarget := grid[nr][rightC]

	if leftTarget == '#' || rightTarget == '#' {
		return false
	}

	boxesToCheck := make(map[Point]bool)

	if leftTarget == '[' {
		boxesToCheck[Point{nr, leftC}] = true
	} else if leftTarget == ']' {
		boxesToCheck[Point{nr, leftC - 1}] = true
	}

	if rightTarget == '[' {
		boxesToCheck[Point{nr, rightC}] = true
	} else if rightTarget == ']' {
		boxesToCheck[Point{nr, rightC - 1}] = true
	}

	for box := range boxesToCheck {
		if !canMoveBoxVertical(grid, box.c, box.r, dr) {
			return false
		}
	}

	return true
}

func collectBoxesVertical(grid [][]rune, boxLeftC, r, dr int, collected map[Point]bool) {
	box := Point{r, boxLeftC}
	collected[box] = true

	nr := r + dr
	leftC, rightC := boxLeftC, boxLeftC+1

	leftTarget := grid[nr][leftC]
	rightTarget := grid[nr][rightC]

	boxesToCheck := make(map[Point]bool)

	if leftTarget == '[' {
		boxesToCheck[Point{nr, leftC}] = true
	} else if leftTarget == ']' {
		boxesToCheck[Point{nr, leftC - 1}] = true
	}

	if rightTarget == '[' {
		boxesToCheck[Point{nr, rightC}] = true
	} else if rightTarget == ']' {
		boxesToCheck[Point{nr, rightC - 1}] = true
	}

	for box := range boxesToCheck {
		if !collected[box] {
			collectBoxesVertical(grid, box.c, box.r, dr, collected)
		}
	}
}

func moveRobotWide(grid [][]rune, robotPos Point, direction rune) Point {
	deltas := map[rune]Point{
		'<': {0, -1},
		'>': {0, 1},
		'^': {-1, 0},
		'v': {1, 0},
	}

	dr := deltas[direction].r
	dc := deltas[direction].c
	r, c := robotPos.r, robotPos.c
	nr, nc := r+dr, c+dc

	target := grid[nr][nc]

	if target == '#' {
		return robotPos
	}

	if target == '.' {
		grid[r][c] = '.'
		grid[nr][nc] = '@'
		return Point{nr, nc}
	}

	if target == '[' || target == ']' {
		if dc != 0 { // Horizontal movement
			checkC := nc
			for grid[r][checkC] == '[' || grid[r][checkC] == ']' {
				checkC += dc
			}

			if grid[r][checkC] == '#' {
				return robotPos
			}

			// Shift all boxes
			if dc > 0 { // Moving right
				for col := checkC; col > nc; col-- {
					grid[r][col] = grid[r][col-1]
				}
			} else { // Moving left
				for col := checkC; col < nc; col++ {
					grid[r][col] = grid[r][col+1]
				}
			}

			grid[r][c] = '.'
			grid[nr][nc] = '@'
			return Point{nr, nc}
		} else { // Vertical movement
			var boxLeftC int
			if target == '[' {
				boxLeftC = nc
			} else {
				boxLeftC = nc - 1
			}

			if !canMoveBoxVertical(grid, boxLeftC, nr, dr) {
				return robotPos
			}

			// Collect all boxes that need to move
			boxesToMove := make(map[Point]bool)
			collectBoxesVertical(grid, boxLeftC, nr, dr, boxesToMove)

			// Sort boxes by row
			boxList := make([]Point, 0, len(boxesToMove))
			for box := range boxesToMove {
				boxList = append(boxList, box)
			}

			// Sort based on direction
			sortBoxes := func(boxes []Point, dr int) {
				for i := 0; i < len(boxes); i++ {
					for j := i + 1; j < len(boxes); j++ {
						if dr > 0 {
							if boxes[i].r < boxes[j].r {
								boxes[i], boxes[j] = boxes[j], boxes[i]
							}
						} else {
							if boxes[i].r > boxes[j].r {
								boxes[i], boxes[j] = boxes[j], boxes[i]
							}
						}
					}
				}
			}
			sortBoxes(boxList, dr)

			// Move all boxes
			for _, box := range boxList {
				boxR, boxC := box.r, box.c
				grid[boxR][boxC] = '.'
				grid[boxR][boxC+1] = '.'
				grid[boxR+dr][boxC] = '['
				grid[boxR+dr][boxC+1] = ']'
			}

			// Move robot
			grid[r][c] = '.'
			grid[nr][nc] = '@'
			return Point{nr, nc}
		}
	}

	return robotPos
}

func part2(text string) int {
	grid, moves := parseInput(text)
	grid = scaleGrid(grid)
	robotPos := findRobot(grid)

	for _, move := range moves {
		robotPos = moveRobotWide(grid, robotPos, move)
	}

	return calculateGPS(grid, '[')
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	text := string(data)

	fmt.Printf("Part 1: %d\n", part1(text))
	fmt.Printf("Part 2: %d\n", part2(text))
}
