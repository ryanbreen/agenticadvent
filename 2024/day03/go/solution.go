package main

import (
	"fmt"
	"os"
	"regexp"
	"sort"
	"strconv"
)

// Event represents a parsed event (mul, do, or don't) with its position
type Event struct {
	pos       int
	eventType string
	x         int
	y         int
}

func part1(data string) int {
	// Find all valid mul(X,Y) instructions and sum their products
	pattern := regexp.MustCompile(`mul\((\d{1,3}),(\d{1,3})\)`)
	matches := pattern.FindAllStringSubmatch(data, -1)

	total := 0
	for _, match := range matches {
		x, _ := strconv.Atoi(match[1])
		y, _ := strconv.Atoi(match[2])
		total += x * y
	}

	return total
}

func part2(data string) int {
	// Like part1, but do() enables and don't() disables mul instructions
	mulPattern := regexp.MustCompile(`mul\((\d{1,3}),(\d{1,3})\)`)
	doPattern := regexp.MustCompile(`do\(\)`)
	dontPattern := regexp.MustCompile(`don't\(\)`)

	// Build a list of all events with positions
	var events []Event

	// Find all mul instructions
	mulMatches := mulPattern.FindAllStringSubmatchIndex(data, -1)
	for _, match := range mulMatches {
		pos := match[0]
		x, _ := strconv.Atoi(data[match[2]:match[3]])
		y, _ := strconv.Atoi(data[match[4]:match[5]])
		events = append(events, Event{pos, "mul", x, y})
	}

	// Find all do() instructions
	doMatches := doPattern.FindAllStringIndex(data, -1)
	for _, match := range doMatches {
		pos := match[0]
		events = append(events, Event{pos, "do", 0, 0})
	}

	// Find all don't() instructions
	dontMatches := dontPattern.FindAllStringIndex(data, -1)
	for _, match := range dontMatches {
		pos := match[0]
		events = append(events, Event{pos, "dont", 0, 0})
	}

	// Sort by position
	sort.Slice(events, func(i, j int) bool {
		return events[i].pos < events[j].pos
	})

	// Process events
	total := 0
	enabled := true

	for _, event := range events {
		switch event.eventType {
		case "do":
			enabled = true
		case "dont":
			enabled = false
		case "mul":
			if enabled {
				total += event.x * event.y
			}
		}
	}

	return total
}

func main() {
	// Read input file
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
		os.Exit(1)
	}

	input := string(data)

	fmt.Println("Part 1:", part1(input))
	fmt.Println("Part 2:", part2(input))
}
