package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"unicode"
)

func parseInput(text string) ([]string, []interface{}) {
	parts := strings.Split(text, "\n\n")
	gridLines := strings.Split(parts[0], "\n")
	pathStr := strings.TrimSpace(parts[1])

	// Find max width
	width := 0
	for _, line := range gridLines {
		if len(line) > width {
			width = len(line)
		}
	}

	// Pad lines to consistent width
	grid := make([]string, len(gridLines))
	for i, line := range gridLines {
		grid[i] = line + strings.Repeat(" ", width-len(line))
	}

	// Parse path instructions
	var instructions []interface{}
	i := 0
	for i < len(pathStr) {
		if unicode.IsDigit(rune(pathStr[i])) {
			j := i
			for j < len(pathStr) && unicode.IsDigit(rune(pathStr[j])) {
				j++
			}
			num := 0
			for k := i; k < j; k++ {
				num = num*10 + int(pathStr[k]-'0')
			}
			instructions = append(instructions, num)
			i = j
		} else {
			instructions = append(instructions, string(pathStr[i]))
			i++
		}
	}

	return grid, instructions
}

func part1(text string) int {
	grid, instructions := parseInput(text)
	height := len(grid)
	width := len(grid[0])

	// Directions: 0=right, 1=down, 2=left, 3=up
	DR := []int{0, 1, 0, -1}
	DC := []int{1, 0, -1, 0}

	// Find starting position (leftmost open tile on top row)
	row := 0
	col := strings.Index(grid[0], ".")
	facing := 0 // Start facing right

	for _, instr := range instructions {
		switch v := instr.(type) {
		case int:
			// Move forward
			for step := 0; step < v; step++ {
				dr, dc := DR[facing], DC[facing]
				nr, nc := row+dr, col+dc

				// Wrap around if needed
				if facing == 0 { // Right
					if nc >= width || grid[nr][nc] == ' ' {
						nc = 0
						for grid[nr][nc] == ' ' {
							nc++
						}
					}
				} else if facing == 2 { // Left
					if nc < 0 || grid[nr][nc] == ' ' {
						nc = width - 1
						for grid[nr][nc] == ' ' {
							nc--
						}
					}
				} else if facing == 1 { // Down
					if nr >= height || grid[nr][nc] == ' ' {
						nr = 0
						for grid[nr][nc] == ' ' {
							nr++
						}
					}
				} else if facing == 3 { // Up
					if nr < 0 || grid[nr][nc] == ' ' {
						nr = height - 1
						for grid[nr][nc] == ' ' {
							nr--
						}
					}
				}

				// Check if we hit a wall
				if grid[nr][nc] == '#' {
					break
				}

				// Move to new position
				row, col = nr, nc
			}
		case string:
			// Turn
			if v == "R" {
				facing = (facing + 1) % 4
			} else {
				facing = (facing + 3) % 4
			}
		}
	}

	// Calculate password: 1000*row + 4*col + facing (1-indexed)
	return 1000*(row+1) + 4*(col+1) + facing
}

func getCubeFaceAndLocal(row, col, faceSize int) (int, int, int) {
	faceRow := row / faceSize
	faceCol := col / faceSize
	localR := row % faceSize
	localC := col % faceSize

	// Map face_row, face_col to face number for layout:
	//   12
	//   3
	//  45
	//  6
	if faceRow == 0 && faceCol == 1 {
		return 1, localR, localC
	} else if faceRow == 0 && faceCol == 2 {
		return 2, localR, localC
	} else if faceRow == 1 && faceCol == 1 {
		return 3, localR, localC
	} else if faceRow == 2 && faceCol == 0 {
		return 4, localR, localC
	} else if faceRow == 2 && faceCol == 1 {
		return 5, localR, localC
	} else if faceRow == 3 && faceCol == 0 {
		return 6, localR, localC
	}

	return -1, localR, localC
}

func wrapCube(row, col, facing, faceSize int) (int, int, int) {
	S := faceSize
	face, lr, lc := getCubeFaceAndLocal(row, col, S)

	switch face {
	case 1:
		if facing == 3 { // Up: goes to face 6, from left, facing right
			return 3*S + lc, 0, 0
		} else if facing == 2 { // Left: goes to face 4, from left, facing right (inverted)
			return 3*S - 1 - lr, 0, 0
		}
	case 2:
		if facing == 0 { // Right: goes to face 5, from right, facing left (inverted)
			return 3*S - 1 - lr, 2*S - 1, 2
		} else if facing == 1 { // Down: goes to face 3, from right, facing left
			return S + lc, 2*S - 1, 2
		} else if facing == 3 { // Up: goes to face 6, from bottom, facing up
			return 4*S - 1, lc, 3
		}
	case 3:
		if facing == 0 { // Right: goes to face 2, from bottom, facing up
			return S - 1, 2*S + lr, 3
		} else if facing == 2 { // Left: goes to face 4, from top, facing down
			return 2 * S, lr, 1
		}
	case 4:
		if facing == 3 { // Up: goes to face 3, from left, facing right
			return S + lc, S, 0
		} else if facing == 2 { // Left: goes to face 1, from left, facing right (inverted)
			return S - 1 - lr, S, 0
		}
	case 5:
		if facing == 0 { // Right: goes to face 2, from right, facing left (inverted)
			return S - 1 - lr, 3*S - 1, 2
		} else if facing == 1 { // Down: goes to face 6, from right, facing left
			return 3*S + lc, S - 1, 2
		}
	case 6:
		if facing == 0 { // Right: goes to face 5, from bottom, facing up
			return 3*S - 1, S + lr, 3
		} else if facing == 1 { // Down: goes to face 2, from top, facing down
			return 0, 2*S + lc, 1
		} else if facing == 2 { // Left: goes to face 1, from top, facing down
			return 0, S + lr, 1
		}
	}

	return row, col, facing
}

func part2(text string) int {
	grid, instructions := parseInput(text)
	height := len(grid)
	width := len(grid[0])

	// Determine face size
	faceSize := 50
	if height <= 50 {
		faceSize = 4
	}

	// Directions: 0=right, 1=down, 2=left, 3=up
	DR := []int{0, 1, 0, -1}
	DC := []int{1, 0, -1, 0}

	// Find starting position
	row := 0
	col := strings.Index(grid[0], ".")
	facing := 0

	for _, instr := range instructions {
		switch v := instr.(type) {
		case int:
			for step := 0; step < v; step++ {
				dr, dc := DR[facing], DC[facing]
				nr, nc := row+dr, col+dc
				nf := facing

				// Check if we need to wrap
				needWrap := false
				if nr < 0 || nr >= height || nc < 0 || nc >= width {
					needWrap = true
				} else if grid[nr][nc] == ' ' {
					needWrap = true
				}

				if needWrap {
					nr, nc, nf = wrapCube(row, col, facing, faceSize)
				}

				// Check for wall
				if grid[nr][nc] == '#' {
					break
				}

				row, col, facing = nr, nc, nf
			}
		case string:
			if v == "R" {
				facing = (facing + 1) % 4
			} else {
				facing = (facing + 3) % 4
			}
		}
	}

	return 1000*(row+1) + 4*(col+1) + facing
}

func main() {
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)

	// Try to find input.txt relative to executable first, then current directory
	inputPath := filepath.Join(dir, "..", "input.txt")
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		// Try relative to source file location (when running with go run)
		inputPath = filepath.Join(filepath.Dir(os.Args[0]), "..", "input.txt")
		if _, err := os.Stat(inputPath); os.IsNotExist(err) {
			// Default to current working directory pattern
			inputPath = "../input.txt"
		}
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	text := string(data)

	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
