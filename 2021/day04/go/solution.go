package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Board [5][5]int
type Marked [5][5]bool

func parseInput() ([]int, []Board) {
	exePath, _ := os.Executable()
	inputPath := filepath.Join(filepath.Dir(exePath), "..", "input.txt")

	// Fallback for go run
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = filepath.Join(".", "..", "input.txt")
	}

	file, err := os.Open(inputPath)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	// Read drawn numbers
	scanner.Scan()
	numStrs := strings.Split(scanner.Text(), ",")
	numbers := make([]int, len(numStrs))
	for i, s := range numStrs {
		numbers[i], _ = strconv.Atoi(s)
	}

	// Read boards
	var boards []Board
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		var board Board
		// First line of board
		fields := strings.Fields(line)
		for col, s := range fields {
			board[0][col], _ = strconv.Atoi(s)
		}

		// Remaining 4 lines
		for row := 1; row < 5; row++ {
			scanner.Scan()
			fields = strings.Fields(scanner.Text())
			for col, s := range fields {
				board[row][col], _ = strconv.Atoi(s)
			}
		}

		boards = append(boards, board)
	}

	return numbers, boards
}

func checkWinner(marked *Marked) bool {
	// Check rows
	for row := 0; row < 5; row++ {
		allMarked := true
		for col := 0; col < 5; col++ {
			if !marked[row][col] {
				allMarked = false
				break
			}
		}
		if allMarked {
			return true
		}
	}

	// Check columns
	for col := 0; col < 5; col++ {
		allMarked := true
		for row := 0; row < 5; row++ {
			if !marked[row][col] {
				allMarked = false
				break
			}
		}
		if allMarked {
			return true
		}
	}

	return false
}

func calculateScore(board *Board, marked *Marked, lastNumber int) int {
	unmarkedSum := 0
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if !marked[row][col] {
				unmarkedSum += board[row][col]
			}
		}
	}
	return unmarkedSum * lastNumber
}

func markNumber(board *Board, marked *Marked, number int) {
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if board[row][col] == number {
				marked[row][col] = true
			}
		}
	}
}

func part1(numbers []int, boards []Board) int {
	marked := make([]Marked, len(boards))

	for _, number := range numbers {
		for i := range boards {
			markNumber(&boards[i], &marked[i], number)
			if checkWinner(&marked[i]) {
				return calculateScore(&boards[i], &marked[i], number)
			}
		}
	}

	return 0
}

func part2(numbers []int, boards []Board) int {
	marked := make([]Marked, len(boards))
	won := make([]bool, len(boards))
	lastScore := 0

	for _, number := range numbers {
		for i := range boards {
			if won[i] {
				continue
			}
			markNumber(&boards[i], &marked[i], number)
			if checkWinner(&marked[i]) {
				won[i] = true
				lastScore = calculateScore(&boards[i], &marked[i], number)
			}
		}
	}

	return lastScore
}

func main() {
	numbers, boards := parseInput()
	fmt.Printf("Part 1: %d\n", part1(numbers, boards))
	fmt.Printf("Part 2: %d\n", part2(numbers, boards))
}
