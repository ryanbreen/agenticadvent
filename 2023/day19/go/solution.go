package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

// Rule represents a single rule within a workflow
type Rule struct {
	Attr        string // "x", "m", "a", "s" or "" for default
	Op          string // "<" or ">" or "" for default
	Value       int
	Destination string
}

// Part represents a machine part with x, m, a, s ratings
type Part struct {
	X, M, A, S int
}

// Range represents an inclusive range [Lo, Hi]
type Range struct {
	Lo, Hi int
}

// Ranges represents the possible ranges for all four attributes
type Ranges struct {
	X, M, A, S Range
}

func parseInput(filename string) (map[string][]Rule, []Part) {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	workflows := make(map[string][]Rule)
	var parts []Part

	scanner := bufio.NewScanner(file)
	parsingParts := false

	ruleRe := regexp.MustCompile(`([xmas])([<>])(\d+)`)
	partRe := regexp.MustCompile(`([xmas])=(\d+)`)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			parsingParts = true
			continue
		}

		if !parsingParts {
			// Parse workflow: name{rules...}
			idx := strings.Index(line, "{")
			name := line[:idx]
			rulesStr := line[idx+1 : len(line)-1]

			var rules []Rule
			for _, ruleStr := range strings.Split(rulesStr, ",") {
				if strings.Contains(ruleStr, ":") {
					colonIdx := strings.Index(ruleStr, ":")
					condition := ruleStr[:colonIdx]
					destination := ruleStr[colonIdx+1:]

					matches := ruleRe.FindStringSubmatch(condition)
					attr := matches[1]
					op := matches[2]
					value, _ := strconv.Atoi(matches[3])

					rules = append(rules, Rule{
						Attr:        attr,
						Op:          op,
						Value:       value,
						Destination: destination,
					})
				} else {
					// Default rule
					rules = append(rules, Rule{
						Destination: ruleStr,
					})
				}
			}
			workflows[name] = rules
		} else {
			// Parse part: {x=...,m=...,a=...,s=...}
			part := Part{}
			for _, match := range partRe.FindAllStringSubmatch(line, -1) {
				attr := match[1]
				value, _ := strconv.Atoi(match[2])
				switch attr {
				case "x":
					part.X = value
				case "m":
					part.M = value
				case "a":
					part.A = value
				case "s":
					part.S = value
				}
			}
			parts = append(parts, part)
		}
	}

	return workflows, parts
}

func (p Part) getAttr(attr string) int {
	switch attr {
	case "x":
		return p.X
	case "m":
		return p.M
	case "a":
		return p.A
	case "s":
		return p.S
	}
	return 0
}

func processPart(workflows map[string][]Rule, part Part) bool {
	current := "in"

	for current != "A" && current != "R" {
		for _, rule := range workflows[current] {
			if rule.Attr == "" {
				// Default rule
				current = rule.Destination
				break
			}

			val := part.getAttr(rule.Attr)
			matches := false
			if rule.Op == "<" && val < rule.Value {
				matches = true
			} else if rule.Op == ">" && val > rule.Value {
				matches = true
			}

			if matches {
				current = rule.Destination
				break
			}
		}
	}

	return current == "A"
}

func part1(workflows map[string][]Rule, parts []Part) int {
	total := 0
	for _, part := range parts {
		if processPart(workflows, part) {
			total += part.X + part.M + part.A + part.S
		}
	}
	return total
}

func (r Ranges) getAttr(attr string) Range {
	switch attr {
	case "x":
		return r.X
	case "m":
		return r.M
	case "a":
		return r.A
	case "s":
		return r.S
	}
	return Range{}
}

func (r Ranges) setAttr(attr string, rng Range) Ranges {
	result := r
	switch attr {
	case "x":
		result.X = rng
	case "m":
		result.M = rng
	case "a":
		result.A = rng
	case "s":
		result.S = rng
	}
	return result
}

func countAccepted(workflows map[string][]Rule, workflow string, ranges Ranges) int64 {
	if workflow == "R" {
		return 0
	}
	if workflow == "A" {
		// Count all combinations in current ranges
		result := int64(1)
		result *= int64(max(0, ranges.X.Hi-ranges.X.Lo+1))
		result *= int64(max(0, ranges.M.Hi-ranges.M.Lo+1))
		result *= int64(max(0, ranges.A.Hi-ranges.A.Lo+1))
		result *= int64(max(0, ranges.S.Hi-ranges.S.Lo+1))
		return result
	}

	var total int64 = 0

	for _, rule := range workflows[workflow] {
		if rule.Attr == "" {
			// Default rule
			total += countAccepted(workflows, rule.Destination, ranges)
		} else {
			rng := ranges.getAttr(rule.Attr)
			lo, hi := rng.Lo, rng.Hi

			if rule.Op == "<" {
				// Split: [lo, value-1] goes to destination, [value, hi] continues
				if lo < rule.Value {
					// Part that matches the condition
					newRanges := ranges.setAttr(rule.Attr, Range{lo, min(hi, rule.Value-1)})
					total += countAccepted(workflows, rule.Destination, newRanges)
				}
				// Remaining part continues to next rule
				if hi >= rule.Value {
					ranges = ranges.setAttr(rule.Attr, Range{max(lo, rule.Value), hi})
				} else {
					break // No remaining range
				}
			} else { // op == ">"
				// Split: [value+1, hi] goes to destination, [lo, value] continues
				if hi > rule.Value {
					// Part that matches the condition
					newRanges := ranges.setAttr(rule.Attr, Range{max(lo, rule.Value+1), hi})
					total += countAccepted(workflows, rule.Destination, newRanges)
				}
				// Remaining part continues to next rule
				if lo <= rule.Value {
					ranges = ranges.setAttr(rule.Attr, Range{lo, min(hi, rule.Value)})
				} else {
					break // No remaining range
				}
			}
		}
	}

	return total
}

func part2(workflows map[string][]Rule) int64 {
	initialRanges := Ranges{
		X: Range{1, 4000},
		M: Range{1, 4000},
		A: Range{1, 4000},
		S: Range{1, 4000},
	}
	return countAccepted(workflows, "in", initialRanges)
}

func main() {
	workflows, parts := parseInput("../input.txt")
	fmt.Printf("Part 1: %d\n", part1(workflows, parts))
	fmt.Printf("Part 2: %d\n", part2(workflows))
}
