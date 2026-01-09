<?php
/**
 * Advent of Code 2022 - Day 19: Not Enough Minerals
 *
 * Robot factory optimization problem using DFS with aggressive pruning.
 */

$input = trim(file_get_contents(__DIR__ . '/../input.txt'));

function parseInput(string $text): array {
    $blueprints = [];
    $pattern = '/Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\./';

    foreach (explode("\n", $text) as $line) {
        if (preg_match($pattern, $line, $m)) {
            $blueprints[] = array_map('intval', array_slice($m, 1));
        }
    }
    return $blueprints;
}

function maxGeodes(array $bp, int $timeLimit): int {
    [$bpId, $oreOre, $clayOre, $obsOre, $obsClay, $geoOre, $geoObs] = $bp;

    // Max robots needed per type
    $maxOre = max($oreOre, $clayOre, $obsOre, $geoOre);
    $maxClay = $obsClay;
    $maxObs = $geoObs;

    $best = 0;

    // Use stack-based DFS
    // State: [time, ore, clay, obs, geodes, oreR, clayR, obsR, geoR]
    $stack = [[0, 0, 0, 0, 0, 1, 0, 0, 0]];
    $seen = [];

    while (!empty($stack)) {
        [$time, $ore, $clay, $obs, $geodes, $oreR, $clayR, $obsR, $geoR] = array_pop($stack);

        // Pruning: upper bound on possible geodes
        $remaining = $timeLimit - $time;
        $upperBound = $geodes + $geoR * $remaining + intdiv($remaining * ($remaining - 1), 2);
        if ($upperBound <= $best) {
            continue;
        }

        if ($time === $timeLimit) {
            $best = max($best, $geodes);
            continue;
        }

        // Cap resources to reduce state space
        $cappedOre = min($ore, $remaining * $maxOre);
        $cappedClay = min($clay, $remaining * $maxClay);
        $cappedObs = min($obs, $remaining * $maxObs);

        // State deduplication with geodes tracking
        $key = "$time,$cappedOre,$cappedClay,$cappedObs,$oreR,$clayR,$obsR,$geoR";
        if (isset($seen[$key]) && $seen[$key] >= $geodes) {
            continue;
        }
        $seen[$key] = $geodes;

        // Collect resources
        $newOre = $cappedOre + $oreR;
        $newClay = $cappedClay + $clayR;
        $newObs = $cappedObs + $obsR;
        $newGeodes = $geodes + $geoR;

        // Try building geode robot (always do if possible and return early)
        if ($cappedOre >= $geoOre && $cappedObs >= $geoObs) {
            $stack[] = [$time + 1, $newOre - $geoOre, $newClay, $newObs - $geoObs, $newGeodes,
                $oreR, $clayR, $obsR, $geoR + 1];
            continue; // If we can build geode, always do
        }

        // Do nothing (wait) - push first so it gets processed last
        $stack[] = [$time + 1, $newOre, $newClay, $newObs, $newGeodes,
            $oreR, $clayR, $obsR, $geoR];

        // Try building ore robot
        if ($cappedOre >= $oreOre && $oreR < $maxOre) {
            $stack[] = [$time + 1, $newOre - $oreOre, $newClay, $newObs, $newGeodes,
                $oreR + 1, $clayR, $obsR, $geoR];
        }

        // Try building clay robot
        if ($cappedOre >= $clayOre && $clayR < $maxClay) {
            $stack[] = [$time + 1, $newOre - $clayOre, $newClay, $newObs, $newGeodes,
                $oreR, $clayR + 1, $obsR, $geoR];
        }

        // Try building obsidian robot
        if ($cappedOre >= $obsOre && $cappedClay >= $obsClay && $obsR < $maxObs) {
            $stack[] = [$time + 1, $newOre - $obsOre, $newClay - $obsClay, $newObs, $newGeodes,
                $oreR, $clayR, $obsR + 1, $geoR];
        }
    }

    return $best;
}

function part1(string $input): int {
    $blueprints = parseInput($input);
    $total = 0;
    foreach ($blueprints as $bp) {
        $geodes = maxGeodes($bp, 24);
        $total += $bp[0] * $geodes;
    }
    return $total;
}

function part2(string $input): int {
    $blueprints = array_slice(parseInput($input), 0, 3);
    $result = 1;
    foreach ($blueprints as $bp) {
        $geodes = maxGeodes($bp, 32);
        $result *= $geodes;
    }
    return $result;
}

// Increase memory limit for this computationally intensive problem
ini_set('memory_limit', '512M');

echo "Part 1: " . part1($input) . "\n";
echo "Part 2: " . part2($input) . "\n";
