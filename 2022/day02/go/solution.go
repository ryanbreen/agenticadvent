package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type Round struct {
	opponent string
	second   string
}

func parseInput(filename string) ([]Round, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var rounds []Round
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Fields(line)
		if len(parts) == 2 {
			rounds = append(rounds, Round{opponent: parts[0], second: parts[1]})
		}
	}
	return rounds, scanner.Err()
}

func part1(rounds []Round) int {
	// X=Rock, Y=Paper, Z=Scissors
	// Shape scores: Rock=1, Paper=2, Scissors=3
	shapeScore := map[string]int{
		"X": 1,
		"Y": 2,
		"Z": 3,
	}

	// Outcome: 0=loss, 3=draw, 6=win
	// A=Rock, B=Paper, C=Scissors
	outcomes := map[string]int{
		"A X": 3, // Rock vs Rock = draw
		"A Y": 6, // Rock vs Paper = win
		"A Z": 0, // Rock vs Scissors = loss
		"B X": 0, // Paper vs Rock = loss
		"B Y": 3, // Paper vs Paper = draw
		"B Z": 6, // Paper vs Scissors = win
		"C X": 6, // Scissors vs Rock = win
		"C Y": 0, // Scissors vs Paper = loss
		"C Z": 3, // Scissors vs Scissors = draw
	}

	total := 0
	for _, r := range rounds {
		key := r.opponent + " " + r.second
		total += shapeScore[r.second] + outcomes[key]
	}
	return total
}

func part2(rounds []Round) int {
	// X=lose, Y=draw, Z=win
	// What shape to play given opponent and desired outcome
	// Returns the shape we play (1=Rock, 2=Paper, 3=Scissors)
	choices := map[string]int{
		"A X": 3, // Rock, need to lose -> Scissors
		"A Y": 1, // Rock, need to draw -> Rock
		"A Z": 2, // Rock, need to win -> Paper
		"B X": 1, // Paper, need to lose -> Rock
		"B Y": 2, // Paper, need to draw -> Paper
		"B Z": 3, // Paper, need to win -> Scissors
		"C X": 2, // Scissors, need to lose -> Paper
		"C Y": 3, // Scissors, need to draw -> Scissors
		"C Z": 1, // Scissors, need to win -> Rock
	}

	outcomeScore := map[string]int{
		"X": 0,
		"Y": 3,
		"Z": 6,
	}

	total := 0
	for _, r := range rounds {
		key := r.opponent + " " + r.second
		total += choices[key] + outcomeScore[r.second]
	}
	return total
}

func main() {
	// Get the directory of the executable
	execPath, err := os.Executable()
	if err != nil {
		// Fallback to current working directory approach
		execPath = "."
	}
	execDir := filepath.Dir(execPath)

	// Try different input file locations
	inputPaths := []string{
		filepath.Join(execDir, "..", "input.txt"),
		"../input.txt",
	}

	var rounds []Round
	var parseErr error
	for _, inputPath := range inputPaths {
		rounds, parseErr = parseInput(inputPath)
		if parseErr == nil {
			break
		}
	}

	if parseErr != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", parseErr)
		os.Exit(1)
	}

	fmt.Println("Part 1:", part1(rounds))
	fmt.Println("Part 2:", part2(rounds))
}
