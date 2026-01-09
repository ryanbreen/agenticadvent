package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
)

// Monkey represents either a number or an operation
type Monkey struct {
	isNumber bool
	value    int64
	left     string
	op       string
	right    string
}

func parseInput(text string) map[string]Monkey {
	monkeys := make(map[string]Monkey)
	scanner := bufio.NewScanner(strings.NewReader(text))

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}

		parts := strings.SplitN(line, ": ", 2)
		name := parts[0]
		job := parts[1]

		jobParts := strings.Fields(job)
		if len(jobParts) == 1 {
			val, _ := strconv.ParseInt(jobParts[0], 10, 64)
			monkeys[name] = Monkey{isNumber: true, value: val}
		} else {
			monkeys[name] = Monkey{
				isNumber: false,
				left:     jobParts[0],
				op:       jobParts[1],
				right:    jobParts[2],
			}
		}
	}

	return monkeys
}

func evaluate(monkeys map[string]Monkey, name string, memo map[string]int64) int64 {
	if val, ok := memo[name]; ok {
		return val
	}

	monkey := monkeys[name]
	if monkey.isNumber {
		return monkey.value
	}

	leftVal := evaluate(monkeys, monkey.left, memo)
	rightVal := evaluate(monkeys, monkey.right, memo)

	var result int64
	switch monkey.op {
	case "+":
		result = leftVal + rightVal
	case "-":
		result = leftVal - rightVal
	case "*":
		result = leftVal * rightVal
	case "/":
		result = leftVal / rightVal
	}

	memo[name] = result
	return result
}

func containsHumn(monkeys map[string]Monkey, name string, memo map[string]bool) bool {
	if val, ok := memo[name]; ok {
		return val
	}

	if name == "humn" {
		return true
	}

	monkey := monkeys[name]
	if monkey.isNumber {
		memo[name] = false
		return false
	}

	result := containsHumn(monkeys, monkey.left, memo) || containsHumn(monkeys, monkey.right, memo)
	memo[name] = result
	return result
}

func solveForHumn(monkeys map[string]Monkey, name string, target int64, humnMemo map[string]bool, evalMemo map[string]int64) int64 {
	if name == "humn" {
		return target
	}

	monkey := monkeys[name]
	if monkey.isNumber {
		// Can't solve if it's just a number
		return 0
	}

	leftHasHumn := containsHumn(monkeys, monkey.left, humnMemo)

	if leftHasHumn {
		// Evaluate right side to get its value
		rightVal := evaluate(monkeys, monkey.right, evalMemo)
		var newTarget int64

		switch monkey.op {
		case "+":
			// left + right = target => left = target - right
			newTarget = target - rightVal
		case "-":
			// left - right = target => left = target + right
			newTarget = target + rightVal
		case "*":
			// left * right = target => left = target / right
			newTarget = target / rightVal
		case "/":
			// left / right = target => left = target * right
			newTarget = target * rightVal
		}
		return solveForHumn(monkeys, monkey.left, newTarget, humnMemo, evalMemo)
	} else {
		// Evaluate left side to get its value
		leftVal := evaluate(monkeys, monkey.left, evalMemo)
		var newTarget int64

		switch monkey.op {
		case "+":
			// left + right = target => right = target - left
			newTarget = target - leftVal
		case "-":
			// left - right = target => right = left - target
			newTarget = leftVal - target
		case "*":
			// left * right = target => right = target / left
			newTarget = target / leftVal
		case "/":
			// left / right = target => right = left / target
			newTarget = leftVal / target
		}
		return solveForHumn(monkeys, monkey.right, newTarget, humnMemo, evalMemo)
	}
}

func part1(text string) int64 {
	monkeys := parseInput(text)
	memo := make(map[string]int64)
	return evaluate(monkeys, "root", memo)
}

func part2(text string) int64 {
	monkeys := parseInput(text)

	// root checks equality between its two children
	root := monkeys["root"]
	left := root.left
	right := root.right

	humnMemo := make(map[string]bool)
	evalMemo := make(map[string]int64)

	// Find which side contains humn
	leftHasHumn := containsHumn(monkeys, left, humnMemo)

	if leftHasHumn {
		// Right side gives us the target value
		target := evaluate(monkeys, right, evalMemo)
		return solveForHumn(monkeys, left, target, humnMemo, evalMemo)
	} else {
		// Left side gives us the target value
		target := evaluate(monkeys, left, evalMemo)
		return solveForHumn(monkeys, right, target, humnMemo, evalMemo)
	}
}

func main() {
	_, filename, _, _ := runtime.Caller(0)
	inputPath := filepath.Join(filepath.Dir(filename), "..", "input.txt")

	content, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	text := string(content)

	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
