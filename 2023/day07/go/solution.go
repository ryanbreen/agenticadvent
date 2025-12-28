package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

// Card strength orders (higher index = stronger)
const cardStrength = "23456789TJQKA"
const cardStrengthJoker = "J23456789TQKA" // J is weakest in Part 2

// Hand type constants (higher = stronger)
const (
	HighCard     = iota // 0 - [1,1,1,1,1]
	OnePair             // 1 - [2,1,1,1]
	TwoPair             // 2 - [2,2,1]
	ThreeOfKind         // 3 - [3,1,1]
	FullHouse           // 4 - [3,2]
	FourOfKind          // 5 - [4,1]
	FiveOfKind          // 6 - [5]
)

// Hand represents a Camel Cards hand with its cards and bid amount.
type Hand struct {
	cards string
	bid   int
}

// classifyFromCounts determines hand type from sorted card counts (descending).
func classifyFromCounts(values []int) int {
	switch {
	case len(values) == 1 || values[0] == 5:
		return FiveOfKind
	case values[0] == 4:
		return FourOfKind
	case values[0] == 3 && len(values) > 1 && values[1] == 2:
		return FullHouse
	case values[0] == 3:
		return ThreeOfKind
	case values[0] == 2 && len(values) > 1 && values[1] == 2:
		return TwoPair
	case values[0] == 2:
		return OnePair
	default:
		return HighCard
	}
}

// getHandType returns the hand type as an integer (higher = stronger).
func getHandType(hand string) int {
	counts := make(map[rune]int)
	for _, c := range hand {
		counts[c]++
	}

	// Get sorted counts (descending)
	var values []int
	for _, v := range counts {
		values = append(values, v)
	}
	sort.Sort(sort.Reverse(sort.IntSlice(values)))

	return classifyFromCounts(values)
}

// getHandTypeWithJokers returns hand type with J as wildcards.
func getHandTypeWithJokers(hand string) int {
	jokerCount := strings.Count(hand, "J")
	if jokerCount == 0 {
		return getHandType(hand)
	}
	if jokerCount == 5 {
		return FiveOfKind
	}

	// Count non-joker cards
	counts := make(map[rune]int)
	for _, c := range hand {
		if c != 'J' {
			counts[c]++
		}
	}

	// Get sorted counts (descending)
	var values []int
	for _, v := range counts {
		values = append(values, v)
	}
	sort.Sort(sort.Reverse(sort.IntSlice(values)))

	// Add jokers to the highest count (optimal strategy)
	values[0] += jokerCount

	return classifyFromCounts(values)
}

// handKey returns a sort key for comparing hands
func handKey(hand string, strengthOrder string) []int {
	key := make([]int, len(hand))
	for i, c := range hand {
		key[i] = strings.IndexRune(strengthOrder, c)
	}
	return key
}

// compareHands compares two hands for sorting (returns true if a < b)
func compareHands(a, b Hand, useJokers bool) bool {
	var typeA, typeB int
	var strengthOrder string

	if useJokers {
		typeA = getHandTypeWithJokers(a.cards)
		typeB = getHandTypeWithJokers(b.cards)
		strengthOrder = cardStrengthJoker
	} else {
		typeA = getHandType(a.cards)
		typeB = getHandType(b.cards)
		strengthOrder = cardStrength
	}

	if typeA != typeB {
		return typeA < typeB
	}

	// Compare card by card
	keyA := handKey(a.cards, strengthOrder)
	keyB := handKey(b.cards, strengthOrder)
	for i := range keyA {
		if keyA[i] != keyB[i] {
			return keyA[i] < keyB[i]
		}
	}
	return false
}

func parseInput(filename string) ([]Hand, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var hands []Hand
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		parts := strings.Fields(line)
		bid, err := strconv.Atoi(parts[1])
		if err != nil {
			return nil, fmt.Errorf("invalid bid value %q: %w", parts[1], err)
		}
		hands = append(hands, Hand{cards: parts[0], bid: bid})
	}
	return hands, scanner.Err()
}

func part1(hands []Hand) int {
	// Make a copy to avoid modifying original
	sorted := make([]Hand, len(hands))
	copy(sorted, hands)

	sort.Slice(sorted, func(i, j int) bool {
		return compareHands(sorted[i], sorted[j], false)
	})

	total := 0
	for rank, hand := range sorted {
		total += (rank + 1) * hand.bid
	}
	return total
}

func part2(hands []Hand) int {
	// Make a copy to avoid modifying original
	sorted := make([]Hand, len(hands))
	copy(sorted, hands)

	sort.Slice(sorted, func(i, j int) bool {
		return compareHands(sorted[i], sorted[j], true)
	})

	total := 0
	for rank, hand := range sorted {
		total += (rank + 1) * hand.bid
	}
	return total
}

func main() {
	hands, err := parseInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(hands))
	fmt.Printf("Part 2: %d\n", part2(hands))
}
