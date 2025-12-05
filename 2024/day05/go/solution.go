package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	// Read input file
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		panic(err)
	}
	inputText := strings.TrimSpace(string(data))

	// Parse input - split into rules and updates sections
	sections := strings.Split(inputText, "\n\n")
	rulesSection := strings.Split(sections[0], "\n")
	updatesSection := strings.Split(sections[1], "\n")

	// Parse rules: X|Y means X must come before Y
	// Store as: rules[X] = set of pages that must come AFTER X
	rules := make(map[int]map[int]bool)
	for _, rule := range rulesSection {
		parts := strings.Split(rule, "|")
		before, err := strconv.Atoi(parts[0])
		if err != nil {
			panic(err)
		}
		after, err := strconv.Atoi(parts[1])
		if err != nil {
			panic(err)
		}
		if rules[before] == nil {
			rules[before] = make(map[int]bool)
		}
		rules[before][after] = true
	}

	// Parse updates
	var updates [][]int
	for _, line := range updatesSection {
		parts := strings.Split(line, ",")
		var update []int
		for _, p := range parts {
			num, err := strconv.Atoi(p)
			if err != nil {
				panic(err)
			}
			update = append(update, num)
		}
		updates = append(updates, update)
	}

	fmt.Printf("Part 1: %d\n", part1(updates, rules))
	fmt.Printf("Part 2: %d\n", part2(updates, rules))
}

func isValidOrder(update []int, rules map[int]map[int]bool) bool {
	// Create position map
	pagePositions := make(map[int]int)
	for i, page := range update {
		pagePositions[page] = i
	}

	// Check all pages
	for i, page := range update {
		// Check all pages that must come after this page
		if mustBeAfter, ok := rules[page]; ok {
			for afterPage := range mustBeAfter {
				if pos, exists := pagePositions[afterPage]; exists {
					if pos < i {
						return false
					}
				}
			}
		}
	}
	return true
}

func part1(updates [][]int, rules map[int]map[int]bool) int {
	total := 0
	for _, update := range updates {
		if isValidOrder(update, rules) {
			middleIdx := len(update) / 2
			total += update[middleIdx]
		}
	}
	return total
}

func fixOrder(update []int, rules map[int]map[int]bool) []int {
	// Create a copy to sort
	fixed := make([]int, len(update))
	copy(fixed, update)

	// Custom comparator
	sort.Slice(fixed, func(i, j int) bool {
		a := fixed[i]
		b := fixed[j]

		// If a must come before b, return true
		if rules[a] != nil && rules[a][b] {
			return true
		}
		// If b must come before a, return false
		if rules[b] != nil && rules[b][a] {
			return false
		}
		return false
	})

	return fixed
}

func part2(updates [][]int, rules map[int]map[int]bool) int {
	total := 0
	for _, update := range updates {
		if !isValidOrder(update, rules) {
			fixed := fixOrder(update, rules)
			middleIdx := len(fixed) / 2
			total += fixed[middleIdx]
		}
	}
	return total
}
