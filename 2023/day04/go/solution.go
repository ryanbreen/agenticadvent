package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Card struct {
	winning map[int]struct{}
	have    []int
}

func main() {
	input, err := os.ReadFile("../input.txt")
	if err != nil {
		log.Fatal(err)
	}

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	cards := parseCards(lines)

	fmt.Println("Part 1:", part1(cards))
	fmt.Println("Part 2:", part2(cards))
}

func parseCards(lines []string) []Card {
	cards := make([]Card, len(lines))

	for i, line := range lines {
		// Split on ":" to separate card id from numbers
		parts := strings.Split(line, ":")
		numbers := parts[1]

		// Split on "|" to separate winning from have
		sections := strings.Split(numbers, "|")
		winningPart := sections[0]
		havePart := sections[1]

		// Parse winning numbers into a set
		winning := make(map[int]struct{})
		for _, numStr := range strings.Fields(winningPart) {
			num, _ := strconv.Atoi(numStr)
			winning[num] = struct{}{}
		}

		// Parse have numbers into a slice
		haveFields := strings.Fields(havePart)
		have := make([]int, len(haveFields))
		for j, numStr := range haveFields {
			have[j], _ = strconv.Atoi(numStr)
		}

		cards[i] = Card{winning: winning, have: have}
	}

	return cards
}

func countMatches(card Card) int {
	count := 0
	for _, num := range card.have {
		if _, exists := card.winning[num]; exists {
			count++
		}
	}
	return count
}

func part1(cards []Card) int {
	total := 0

	for _, card := range cards {
		matchCount := countMatches(card)
		if matchCount > 0 {
			// Score is 2^(matches-1)
			score := 1 << (matchCount - 1)
			total += score
		}
	}

	return total
}

func part2(cards []Card) int {
	// Calculate matches for each card
	matches := make([]int, len(cards))
	for i, card := range cards {
		matches[i] = countMatches(card)
	}

	// Track copy counts (start with 1 of each card)
	copies := make([]int, len(cards))
	for i := range copies {
		copies[i] = 1
	}

	// Cascade copies
	for i, matchCount := range matches {
		for j := i + 1; j < len(cards) && j <= i+matchCount; j++ {
			copies[j] += copies[i]
		}
	}

	// Sum all copies
	total := 0
	for _, copyCount := range copies {
		total += copyCount
	}

	return total
}
