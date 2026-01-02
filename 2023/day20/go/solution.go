// Day 20: Pulse Propagation - Module communication simulation
package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

type ModuleType int

const (
	Broadcaster ModuleType = iota
	FlipFlop
	Conjunction
)

type Module struct {
	moduleType   ModuleType
	destinations []string
	state        bool            // For flip-flops: on/off
	memory       map[string]bool // For conjunctions: last pulse from each input
}

type Pulse struct {
	source string
	dest   string
	high   bool
}

func parseInput(filename string) map[string]*Module {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	modules := make(map[string]*Module)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " -> ")
		namePart := parts[0]
		destPart := parts[1]

		destinations := make([]string, 0)
		for _, d := range strings.Split(destPart, ", ") {
			destinations = append(destinations, strings.TrimSpace(d))
		}

		if namePart == "broadcaster" {
			modules["broadcaster"] = &Module{
				moduleType:   Broadcaster,
				destinations: destinations,
			}
		} else if strings.HasPrefix(namePart, "%") {
			name := namePart[1:]
			modules[name] = &Module{
				moduleType:   FlipFlop,
				destinations: destinations,
				state:        false,
			}
		} else if strings.HasPrefix(namePart, "&") {
			name := namePart[1:]
			modules[name] = &Module{
				moduleType:   Conjunction,
				destinations: destinations,
				memory:       make(map[string]bool),
			}
		}
	}

	// Initialize conjunction memory for all inputs
	for name, module := range modules {
		for _, dest := range module.destinations {
			if destModule, exists := modules[dest]; exists && destModule.moduleType == Conjunction {
				destModule.memory[name] = false // false = low pulse
			}
		}
	}

	return modules
}

func resetState(modules map[string]*Module) {
	for _, module := range modules {
		if module.moduleType == FlipFlop {
			module.state = false
		} else if module.moduleType == Conjunction {
			for key := range module.memory {
				module.memory[key] = false
			}
		}
	}
}

func simulateButtonPress(modules map[string]*Module, watchNodes map[string]bool) (int, int, map[string]bool) {
	lowCount := 0
	highCount := 0
	highSenders := make(map[string]bool)

	// Queue for BFS
	queue := []Pulse{{source: "button", dest: "broadcaster", high: false}}

	for len(queue) > 0 {
		pulse := queue[0]
		queue = queue[1:]

		if pulse.high {
			highCount++
		} else {
			lowCount++
		}

		// Track if watched nodes send high pulses
		if watchNodes != nil && watchNodes[pulse.source] && pulse.high {
			highSenders[pulse.source] = true
		}

		module, exists := modules[pulse.dest]
		if !exists {
			continue
		}

		switch module.moduleType {
		case Broadcaster:
			for _, nextDest := range module.destinations {
				queue = append(queue, Pulse{source: pulse.dest, dest: nextDest, high: pulse.high})
			}

		case FlipFlop:
			if !pulse.high { // Only react to low pulses
				module.state = !module.state
				for _, nextDest := range module.destinations {
					queue = append(queue, Pulse{source: pulse.dest, dest: nextDest, high: module.state})
				}
			}

		case Conjunction:
			module.memory[pulse.source] = pulse.high
			// Send low if all inputs are high, otherwise send high
			output := false
			for _, val := range module.memory {
				if !val {
					output = true
					break
				}
			}
			for _, nextDest := range module.destinations {
				queue = append(queue, Pulse{source: pulse.dest, dest: nextDest, high: output})
			}
		}
	}

	return lowCount, highCount, highSenders
}

func part1(modules map[string]*Module) int {
	resetState(modules)

	totalLow := 0
	totalHigh := 0

	for i := 0; i < 1000; i++ {
		low, high, _ := simulateButtonPress(modules, nil)
		totalLow += low
		totalHigh += high
	}

	return totalLow * totalHigh
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int) int {
	return a / gcd(a, b) * b
}

func part2(modules map[string]*Module) int {
	resetState(modules)

	// Find the module that feeds into rx
	var rxInput string
	for name, module := range modules {
		for _, dest := range module.destinations {
			if dest == "rx" {
				rxInput = name
				break
			}
		}
		if rxInput != "" {
			break
		}
	}

	if rxInput == "" {
		return 0
	}

	// Find all modules that feed into rxInput
	watchNodes := make(map[string]bool)
	for name := range modules[rxInput].memory {
		watchNodes[name] = true
	}

	cycleLengths := make(map[string]int)

	buttonPress := 0
	for len(cycleLengths) < len(watchNodes) {
		buttonPress++
		_, _, highSenders := simulateButtonPress(modules, watchNodes)

		for node := range highSenders {
			if _, found := cycleLengths[node]; !found {
				cycleLengths[node] = buttonPress
			}
		}
	}

	// LCM of all cycle lengths
	result := 1
	for _, length := range cycleLengths {
		result = lcm(result, length)
	}

	return result
}

func main() {
	_, currentFile, _, _ := runtime.Caller(0)
	inputPath := filepath.Join(filepath.Dir(currentFile), "..", "input.txt")

	modules := parseInput(inputPath)
	fmt.Printf("Part 1: %d\n", part1(modules))

	// Re-parse for part 2 (fresh state)
	modules = parseInput(inputPath)
	fmt.Printf("Part 2: %d\n", part2(modules))
}
