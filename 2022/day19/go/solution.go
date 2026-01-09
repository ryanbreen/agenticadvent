package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
)

type Blueprint struct {
	id      int
	oreOre  int
	clayOre int
	obsOre  int
	obsClay int
	geoOre  int
	geoObs  int
}

// State packed into uint64 for efficient map keys
// Bits: time(6) | ore(8) | clay(8) | obs(8) | oreR(6) | clayR(6) | obsR(6) | geoR(6)
type State struct {
	time, ore, clay, obs int
	oreR, clayR, obsR, geoR int
}

func (s State) key() uint64 {
	return uint64(s.time)<<58 |
		uint64(s.ore&0xFF)<<50 |
		uint64(s.clay&0xFF)<<42 |
		uint64(s.obs&0xFF)<<34 |
		uint64(s.oreR&0x3F)<<28 |
		uint64(s.clayR&0x3F)<<22 |
		uint64(s.obsR&0x3F)<<16 |
		uint64(s.geoR&0x3F)<<10
}

func parseInput(filename string) []Blueprint {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	pattern := regexp.MustCompile(`Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.`)

	var blueprints []Blueprint
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		matches := pattern.FindStringSubmatch(line)
		if matches != nil {
			nums := make([]int, 7)
			for i := 0; i < 7; i++ {
				nums[i], _ = strconv.Atoi(matches[i+1])
			}
			blueprints = append(blueprints, Blueprint{
				id:      nums[0],
				oreOre:  nums[1],
				clayOre: nums[2],
				obsOre:  nums[3],
				obsClay: nums[4],
				geoOre:  nums[5],
				geoObs:  nums[6],
			})
		}
	}
	return blueprints
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func maxGeodes(bp Blueprint, timeLimit int) int {
	// Max robots needed per type
	maxOre := max(max(bp.oreOre, bp.clayOre), max(bp.obsOre, bp.geoOre))
	maxClay := bp.obsClay
	maxObs := bp.geoObs

	best := 0
	seen := make(map[uint64]int)

	var dfs func(time, ore, clay, obs, geodes, oreR, clayR, obsR, geoR int)
	dfs = func(time, ore, clay, obs, geodes, oreR, clayR, obsR, geoR int) {
		remaining := timeLimit - time

		// Pruning: upper bound on possible geodes
		upperBound := geodes + geoR*remaining + remaining*(remaining-1)/2
		if upperBound <= best {
			return
		}

		if time == timeLimit {
			if geodes > best {
				best = geodes
			}
			return
		}

		// Cap resources to max usable
		cappedOre := min(ore, remaining*maxOre)
		cappedClay := min(clay, remaining*maxClay)
		cappedObs := min(obs, remaining*maxObs)

		// State deduplication using packed key
		state := State{time, cappedOre, cappedClay, cappedObs, oreR, clayR, obsR, geoR}
		key := state.key()
		if prev, exists := seen[key]; exists && prev >= geodes {
			return
		}
		seen[key] = geodes

		// Collect resources
		newOre := cappedOre + oreR
		newClay := cappedClay + clayR
		newObs := cappedObs + obsR
		newGeodes := geodes + geoR

		// Try building geode robot (always do if possible - priority)
		if cappedOre >= bp.geoOre && cappedObs >= bp.geoObs {
			dfs(time+1, newOre-bp.geoOre, newClay, newObs-bp.geoObs, newGeodes,
				oreR, clayR, obsR, geoR+1)
			return // If we can build geode robot, always do it
		}

		// Try building obsidian robot
		if cappedOre >= bp.obsOre && cappedClay >= bp.obsClay && obsR < maxObs {
			dfs(time+1, newOre-bp.obsOre, newClay-bp.obsClay, newObs, newGeodes,
				oreR, clayR, obsR+1, geoR)
		}

		// Try building clay robot
		if cappedOre >= bp.clayOre && clayR < maxClay {
			dfs(time+1, newOre-bp.clayOre, newClay, newObs, newGeodes,
				oreR, clayR+1, obsR, geoR)
		}

		// Try building ore robot
		if cappedOre >= bp.oreOre && oreR < maxOre {
			dfs(time+1, newOre-bp.oreOre, newClay, newObs, newGeodes,
				oreR+1, clayR, obsR, geoR)
		}

		// Do nothing (wait)
		dfs(time+1, newOre, newClay, newObs, newGeodes,
			oreR, clayR, obsR, geoR)
	}

	dfs(0, 0, 0, 0, 0, 1, 0, 0, 0)
	return best
}

func part1(blueprints []Blueprint) int {
	total := 0
	for _, bp := range blueprints {
		geodes := maxGeodes(bp, 24)
		total += bp.id * geodes
	}
	return total
}

func part2(blueprints []Blueprint) int {
	result := 1
	count := min(3, len(blueprints))
	for i := 0; i < count; i++ {
		geodes := maxGeodes(blueprints[i], 32)
		result *= geodes
	}
	return result
}

func main() {
	_, filename, _, _ := runtime.Caller(0)
	dir := filepath.Dir(filename)
	inputPath := filepath.Join(dir, "..", "input.txt")

	blueprints := parseInput(inputPath)

	fmt.Println("Part 1:", part1(blueprints))
	fmt.Println("Part 2:", part2(blueprints))
}
