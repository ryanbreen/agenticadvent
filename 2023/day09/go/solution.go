package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func parseInput(text string) [][]int {
	lines := strings.Split(strings.TrimSpace(text), "\n")
	histories := make([][]int, len(lines))

	for i, line := range lines {
		parts := strings.Fields(line)
		seq := make([]int, len(parts))
		for j, p := range parts {
			num, _ := strconv.Atoi(p)
			seq[j] = num
		}
		histories[i] = seq
	}

	return histories
}

func getDifferences(seq []int) []int {
	diffs := make([]int, len(seq)-1)
	for i := 0; i < len(seq)-1; i++ {
		diffs[i] = seq[i+1] - seq[i]
	}
	return diffs
}

func allZero(seq []int) bool {
	for _, v := range seq {
		if v != 0 {
			return false
		}
	}
	return true
}

func extrapolateNext(seq []int) int {
	sequences := [][]int{seq}
	current := seq

	for !allZero(current) {
		current = getDifferences(current)
		sequences = append(sequences, current)
	}

	for i := len(sequences) - 2; i >= 0; i-- {
		lastVal := sequences[i][len(sequences[i])-1]
		belowVal := sequences[i+1][len(sequences[i+1])-1]
		sequences[i] = append(sequences[i], lastVal+belowVal)
	}

	return sequences[0][len(sequences[0])-1]
}

func extrapolatePrev(seq []int) int {
	sequences := [][]int{seq}
	current := seq

	for !allZero(current) {
		current = getDifferences(current)
		sequences = append(sequences, current)
	}

	for i := len(sequences) - 2; i >= 0; i-- {
		firstVal := sequences[i][0]
		belowVal := sequences[i+1][0]
		sequences[i] = append([]int{firstVal - belowVal}, sequences[i]...)
	}

	return sequences[0][0]
}

func copySeq(seq []int) []int {
	cp := make([]int, len(seq))
	copy(cp, seq)
	return cp
}

func part1(histories [][]int) int {
	sum := 0
	for _, h := range histories {
		sum += extrapolateNext(copySeq(h))
	}
	return sum
}

func part2(histories [][]int) int {
	sum := 0
	for _, h := range histories {
		sum += extrapolatePrev(copySeq(h))
	}
	return sum
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	histories := parseInput(string(data))

	fmt.Printf("Part 1: %d\n", part1(histories))
	fmt.Printf("Part 2: %d\n", part2(histories))
}
