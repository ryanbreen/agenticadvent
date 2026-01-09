package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// compare compares two values recursively
// Returns: -1 if left < right (correct order)
//
//	1 if left > right (wrong order)
//	0 if equal (continue)
func compare(left, right interface{}) int {
	// Both are numbers (json.Unmarshal uses float64 for numbers)
	leftNum, leftIsNum := left.(float64)
	rightNum, rightIsNum := right.(float64)

	if leftIsNum && rightIsNum {
		if leftNum < rightNum {
			return -1
		} else if leftNum > rightNum {
			return 1
		}
		return 0
	}

	// Both are lists
	leftList, leftIsList := left.([]interface{})
	rightList, rightIsList := right.([]interface{})

	if leftIsList && rightIsList {
		minLen := len(leftList)
		if len(rightList) < minLen {
			minLen = len(rightList)
		}

		for i := 0; i < minLen; i++ {
			result := compare(leftList[i], rightList[i])
			if result != 0 {
				return result
			}
		}

		// Check lengths
		if len(leftList) < len(rightList) {
			return -1
		} else if len(leftList) > len(rightList) {
			return 1
		}
		return 0
	}

	// Mixed types - convert integer to list
	if leftIsNum {
		return compare([]interface{}{left}, right)
	}
	return compare(left, []interface{}{right})
}

func part1(text string) int {
	pairs := strings.Split(strings.TrimSpace(text), "\n\n")
	total := 0

	for i, pair := range pairs {
		lines := strings.Split(strings.TrimSpace(pair), "\n")
		if len(lines) < 2 {
			continue
		}

		var left, right interface{}
		json.Unmarshal([]byte(lines[0]), &left)
		json.Unmarshal([]byte(lines[1]), &right)

		if compare(left, right) == -1 {
			total += i + 1 // 1-indexed
		}
	}

	return total
}

func part2(text string) int {
	lines := strings.Split(strings.TrimSpace(text), "\n")
	var packets []interface{}

	for _, line := range lines {
		if line == "" {
			continue
		}
		var packet interface{}
		json.Unmarshal([]byte(line), &packet)
		packets = append(packets, packet)
	}

	// Add divider packets
	var divider1, divider2 interface{}
	json.Unmarshal([]byte("[[2]]"), &divider1)
	json.Unmarshal([]byte("[[6]]"), &divider2)
	packets = append(packets, divider1)
	packets = append(packets, divider2)

	// Sort using comparison function
	sort.Slice(packets, func(i, j int) bool {
		return compare(packets[i], packets[j]) < 0
	})

	// Find positions of dividers (1-indexed)
	pos1, pos2 := 0, 0
	for i, packet := range packets {
		// Compare with divider packets
		if compare(packet, divider1) == 0 {
			pos1 = i + 1
		}
		if compare(packet, divider2) == 0 {
			pos2 = i + 1
		}
	}

	return pos1 * pos2
}

func main() {
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)
	inputPath := filepath.Join(dir, "..", "input.txt")

	// Also try relative to current working directory for go run
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = filepath.Join(".", "..", "input.txt")
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		// Try one more path for go run from go directory
		data, err = os.ReadFile("../input.txt")
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
			os.Exit(1)
		}
	}

	text := string(data)

	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
