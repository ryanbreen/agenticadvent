package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Equation struct {
	target int64
	nums   []int64
}

func parseInput(text string) []Equation {
	var equations []Equation
	lines := strings.Split(strings.TrimSpace(text), "\n")

	for _, line := range lines {
		parts := strings.Split(line, ": ")
		target, _ := strconv.ParseInt(parts[0], 10, 64)

		numStrs := strings.Fields(parts[1])
		nums := make([]int64, len(numStrs))
		for i, s := range numStrs {
			nums[i], _ = strconv.ParseInt(s, 10, 64)
		}

		equations = append(equations, Equation{target, nums})
	}

	return equations
}

func concat(a, b int64) int64 {
	// Concatenate two numbers by treating them as strings
	result, _ := strconv.ParseInt(fmt.Sprintf("%d%d", a, b), 10, 64)
	return result
}

func evaluate(nums []int64, ops []string) int64 {
	result := nums[0]
	for i, op := range ops {
		switch op {
		case "+":
			result += nums[i+1]
		case "*":
			result *= nums[i+1]
		case "||":
			result = concat(result, nums[i+1])
		}
	}
	return result
}

func generateOperatorCombinations(n int, operators []string) [][]string {
	if n == 0 {
		return [][]string{{}}
	}

	// Total combinations = operators^n
	totalCombos := 1
	for i := 0; i < n; i++ {
		totalCombos *= len(operators)
	}

	result := make([][]string, totalCombos)

	for i := 0; i < totalCombos; i++ {
		combo := make([]string, n)
		idx := i
		for j := 0; j < n; j++ {
			combo[j] = operators[idx%len(operators)]
			idx /= len(operators)
		}
		result[i] = combo
	}

	return result
}

func canMakeTarget(target int64, nums []int64, operators []string) bool {
	nOps := len(nums) - 1
	combinations := generateOperatorCombinations(nOps, operators)

	for _, ops := range combinations {
		if evaluate(nums, ops) == target {
			return true
		}
	}

	return false
}

func part1(equations []Equation) int64 {
	operators := []string{"+", "*"}
	var total int64 = 0

	for _, eq := range equations {
		if canMakeTarget(eq.target, eq.nums, operators) {
			total += eq.target
		}
	}

	return total
}

func part2(equations []Equation) int64 {
	operators := []string{"+", "*", "||"}
	var total int64 = 0

	for _, eq := range equations {
		if canMakeTarget(eq.target, eq.nums, operators) {
			total += eq.target
		}
	}

	return total
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	equations := parseInput(string(data))

	fmt.Println("Part 1:", part1(equations))
	fmt.Println("Part 2:", part2(equations))
}
