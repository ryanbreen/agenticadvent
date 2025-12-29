package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Node represents a network node with left and right destinations
type Node struct {
	Left  string
	Right string
}

// parseInput parses the input into instructions and network map
func parseInput(filename string) (string, map[string]Node, error) {
	file, err := os.Open(filename)
	if err != nil {
		return "", nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	network := make(map[string]Node)

	// Read instructions (first line)
	scanner.Scan()
	instructions := scanner.Text()

	// Skip empty line
	scanner.Scan()

	// Parse network nodes
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		// Parse: AAA = (BBB, CCC)
		parts := strings.Split(line, " = ")
		nodeName := parts[0]
		// Remove parentheses and split by comma
		destinations := strings.Trim(parts[1], "()")
		destParts := strings.Split(destinations, ", ")
		network[nodeName] = Node{Left: destParts[0], Right: destParts[1]}
	}

	return instructions, network, scanner.Err()
}

// part1 navigates from AAA to ZZZ following L/R instructions
func part1(instructions string, network map[string]Node) int {
	current := "AAA"
	steps := 0
	instructionLen := len(instructions)

	for current != "ZZZ" {
		instruction := instructions[steps%instructionLen]
		if instruction == 'L' {
			current = network[current].Left
		} else {
			current = network[current].Right
		}
		steps++
	}

	return steps
}

// gcd calculates the greatest common divisor
func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// lcm calculates the least common multiple
func lcm(a, b int) int {
	return a * b / gcd(a, b)
}

// part2 navigates all nodes ending in A simultaneously to nodes ending in Z
func part2(instructions string, network map[string]Node) int {
	// Find all starting nodes (ending in A)
	var startingNodes []string
	for node := range network {
		if strings.HasSuffix(node, "A") {
			startingNodes = append(startingNodes, node)
		}
	}

	instructionLen := len(instructions)
	cycleLengths := make([]int, 0, len(startingNodes))

	// For each starting node, find the cycle length to reach a Z node
	for _, node := range startingNodes {
		current := node
		steps := 0
		for !strings.HasSuffix(current, "Z") {
			instruction := instructions[steps%instructionLen]
			if instruction == 'L' {
				current = network[current].Left
			} else {
				current = network[current].Right
			}
			steps++
		}
		cycleLengths = append(cycleLengths, steps)
	}

	// Find LCM of all cycle lengths
	result := cycleLengths[0]
	for _, length := range cycleLengths[1:] {
		result = lcm(result, length)
	}

	return result
}

func main() {
	instructions, network, err := parseInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(instructions, network))
	fmt.Println("Part 2:", part2(instructions, network))
}
