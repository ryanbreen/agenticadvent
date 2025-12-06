package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Draw struct {
	red   int
	green int
	blue  int
}

type Game struct {
	id    int
	draws []Draw
}

func parseGame(line string) Game {
	// Split "Game X: ..." into parts
	parts := strings.Split(line, ": ")

	// Extract game ID
	gameIDStr := strings.TrimPrefix(parts[0], "Game ")
	gameID, _ := strconv.Atoi(gameIDStr)

	// Parse draws (semicolon-separated)
	drawStrs := strings.Split(parts[1], "; ")
	draws := make([]Draw, 0, len(drawStrs))

	for _, drawStr := range drawStrs {
		draw := Draw{}
		// Parse each color count (comma-separated)
		cubes := strings.Split(drawStr, ", ")
		for _, cube := range cubes {
			parts := strings.Split(strings.TrimSpace(cube), " ")
			count, _ := strconv.Atoi(parts[0])
			color := parts[1]

			switch color {
			case "red":
				draw.red = count
			case "green":
				draw.green = count
			case "blue":
				draw.blue = count
			}
		}
		draws = append(draws, draw)
	}

	return Game{id: gameID, draws: draws}
}

func isGamePossible(game Game, maxRed, maxGreen, maxBlue int) bool {
	for _, draw := range game.draws {
		if draw.red > maxRed || draw.green > maxGreen || draw.blue > maxBlue {
			return false
		}
	}
	return true
}

func getMinimumCubes(game Game) (int, int, int) {
	minRed := 0
	minGreen := 0
	minBlue := 0

	for _, draw := range game.draws {
		if draw.red > minRed {
			minRed = draw.red
		}
		if draw.green > minGreen {
			minGreen = draw.green
		}
		if draw.blue > minBlue {
			minBlue = draw.blue
		}
	}

	return minRed, minGreen, minBlue
}

func part1(games []Game) int {
	sum := 0
	for _, game := range games {
		if isGamePossible(game, 12, 13, 14) {
			sum += game.id
		}
	}
	return sum
}

func part2(games []Game) int {
	sum := 0
	for _, game := range games {
		minRed, minGreen, minBlue := getMinimumCubes(game)
		power := minRed * minGreen * minBlue
		sum += power
	}
	return sum
}

func main() {
	// Read input file
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
		os.Exit(1)
	}

	// Parse games
	input := strings.TrimSpace(string(data))
	lines := strings.Split(input, "\n")

	games := make([]Game, 0, len(lines))
	for _, line := range lines {
		if line != "" {
			games = append(games, parseGame(line))
		}
	}

	// Solve both parts
	fmt.Printf("Part 1: %d\n", part1(games))
	fmt.Printf("Part 2: %d\n", part2(games))
}
