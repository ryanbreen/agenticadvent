package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"
)

type Move struct {
	count int
	from  int
	to    int
}

func parseInput(filename string) ([][]byte, []Move) {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	// Find the blank line separating stacks from moves
	blankLine := 0
	for i, line := range lines {
		if line == "" {
			blankLine = i
			break
		}
	}

	stackLines := lines[:blankLine]
	moveLines := lines[blankLine+1:]

	// Find number of stacks from the last stack line (the numbers)
	numStacks := len(strings.Fields(stackLines[len(stackLines)-1]))

	// Parse stacks (excluding the number line)
	stacks := make([][]byte, numStacks)
	for i := range stacks {
		stacks[i] = []byte{}
	}

	for _, line := range stackLines[:len(stackLines)-1] {
		for i := 0; i < numStacks; i++ {
			pos := 1 + i*4 // Position of crate letter
			if pos < len(line) && line[pos] != ' ' {
				stacks[i] = append(stacks[i], line[pos])
			}
		}
	}

	// Reverse each stack so bottom is at index 0
	for i := range stacks {
		reverse(stacks[i])
	}

	// Parse moves
	moveRegex := regexp.MustCompile(`move (\d+) from (\d+) to (\d+)`)
	var moves []Move
	for _, line := range moveLines {
		if match := moveRegex.FindStringSubmatch(line); match != nil {
			count, _ := strconv.Atoi(match[1])
			from, _ := strconv.Atoi(match[2])
			to, _ := strconv.Atoi(match[3])
			moves = append(moves, Move{count, from - 1, to - 1}) // 0-indexed
		}
	}

	return stacks, moves
}

func reverse(s []byte) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}

func copyStacks(stacks [][]byte) [][]byte {
	result := make([][]byte, len(stacks))
	for i, stack := range stacks {
		result[i] = make([]byte, len(stack))
		copy(result[i], stack)
	}
	return result
}

func part1(stacks [][]byte, moves []Move) string {
	stacks = copyStacks(stacks)
	for _, move := range moves {
		for i := 0; i < move.count; i++ {
			// Pop from source, push to destination
			crate := stacks[move.from][len(stacks[move.from])-1]
			stacks[move.from] = stacks[move.from][:len(stacks[move.from])-1]
			stacks[move.to] = append(stacks[move.to], crate)
		}
	}
	return topCrates(stacks)
}

func part2(stacks [][]byte, moves []Move) string {
	stacks = copyStacks(stacks)
	for _, move := range moves {
		// Move multiple crates at once (preserve order)
		fromLen := len(stacks[move.from])
		crates := make([]byte, move.count)
		copy(crates, stacks[move.from][fromLen-move.count:])
		stacks[move.from] = stacks[move.from][:fromLen-move.count]
		stacks[move.to] = append(stacks[move.to], crates...)
	}
	return topCrates(stacks)
}

func topCrates(stacks [][]byte) string {
	var result []byte
	for _, stack := range stacks {
		if len(stack) > 0 {
			result = append(result, stack[len(stack)-1])
		}
	}
	return string(result)
}

func main() {
	_, currentFile, _, _ := runtime.Caller(0)
	dir := filepath.Dir(currentFile)
	inputFile := filepath.Join(dir, "..", "input.txt")

	stacks, moves := parseInput(inputFile)

	fmt.Println("Part 1:", part1(stacks, moves))
	fmt.Println("Part 2:", part2(stacks, moves))
}
