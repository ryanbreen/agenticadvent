package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Command struct {
	direction string
	value     int
}

func parseInput() ([]Command, error) {
	exePath, err := os.Executable()
	if err != nil {
		return nil, err
	}
	inputPath := filepath.Join(filepath.Dir(exePath), "..", "input.txt")

	file, err := os.Open(inputPath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var commands []Command
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Fields(line)
		if len(parts) != 2 {
			continue
		}
		val, err := strconv.Atoi(parts[1])
		if err != nil {
			return nil, err
		}
		commands = append(commands, Command{direction: parts[0], value: val})
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return commands, nil
}

func part1(commands []Command) int {
	horizontal := 0
	depth := 0

	for _, cmd := range commands {
		switch cmd.direction {
		case "forward":
			horizontal += cmd.value
		case "down":
			depth += cmd.value
		case "up":
			depth -= cmd.value
		}
	}

	return horizontal * depth
}

func part2(commands []Command) int {
	horizontal := 0
	depth := 0
	aim := 0

	for _, cmd := range commands {
		switch cmd.direction {
		case "forward":
			horizontal += cmd.value
			depth += aim * cmd.value
		case "down":
			aim += cmd.value
		case "up":
			aim -= cmd.value
		}
	}

	return horizontal * depth
}

func main() {
	commands, err := parseInput()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(commands))
	fmt.Printf("Part 2: %d\n", part2(commands))
}
