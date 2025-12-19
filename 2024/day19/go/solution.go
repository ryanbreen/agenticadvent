package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

var patterns []string

func countWays(design string) int64 {
	n := len(design)
	memo := make(map[int]int64)

	var dp func(pos int) int64
	dp = func(pos int) int64 {
		if pos == n {
			return 1
		}
		if val, ok := memo[pos]; ok {
			return val
		}

		var total int64 = 0
		for _, pattern := range patterns {
			plen := len(pattern)
			if pos+plen <= n && design[pos:pos+plen] == pattern {
				total += dp(pos + plen)
			}
		}

		memo[pos] = total
		return total
	}

	return dp(0)
}

func part1(designs []string) int {
	count := 0
	for _, design := range designs {
		if countWays(design) > 0 {
			count++
		}
	}
	return count
}

func part2(designs []string) int64 {
	var total int64 = 0
	for _, design := range designs {
		total += countWays(design)
	}
	return total
}

func main() {
	exePath, _ := os.Executable()
	exeDir := filepath.Dir(exePath)
	inputPath := filepath.Join(exeDir, "..", "input.txt")

	// Fallback for running with go run
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = filepath.Join(filepath.Dir(os.Args[0]), "..", "input.txt")
		if _, err := os.Stat(inputPath); os.IsNotExist(err) {
			// Try relative to current working directory
			inputPath = "../input.txt"
		}
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	content := strings.TrimSpace(string(data))
	parts := strings.Split(content, "\n\n")

	// Parse patterns
	patternParts := strings.Split(parts[0], ",")
	patterns = make([]string, len(patternParts))
	for i, p := range patternParts {
		patterns[i] = strings.TrimSpace(p)
	}

	// Parse designs
	designs := strings.Split(strings.TrimSpace(parts[1]), "\n")

	fmt.Printf("Part 1: %d\n", part1(designs))
	fmt.Printf("Part 2: %d\n", part2(designs))
}
