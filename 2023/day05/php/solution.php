<?php
/**
 * Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
 */

/**
 * Parse input into seeds and list of maps.
 */
function parseInput(string $text): array {
    $sections = explode("\n\n", trim($text));

    // Parse seeds
    $seedPart = explode(': ', $sections[0])[1];
    $seeds = array_map('intval', preg_split('/\s+/', trim($seedPart)));

    // Parse maps
    $maps = [];
    for ($i = 1; $i < count($sections); $i++) {
        $lines = explode("\n", trim($sections[$i]));
        $ranges = [];
        // Skip header line
        for ($j = 1; $j < count($lines); $j++) {
            $parts = array_map('intval', preg_split('/\s+/', trim($lines[$j])));
            $ranges[] = [
                'dst_start' => $parts[0],
                'src_start' => $parts[1],
                'length' => $parts[2]
            ];
        }
        $maps[] = $ranges;
    }

    return [$seeds, $maps];
}

/**
 * Apply a single map to transform a value.
 */
function applyMap(int $value, array $ranges): int {
    foreach ($ranges as $range) {
        $srcStart = $range['src_start'];
        $srcEnd = $srcStart + $range['length'];
        if ($value >= $srcStart && $value < $srcEnd) {
            return $range['dst_start'] + ($value - $srcStart);
        }
    }
    return $value;
}

/**
 * Convert a seed number to a location number through all maps.
 */
function seedToLocation(int $seed, array $maps): int {
    $value = $seed;
    foreach ($maps as $mapRanges) {
        $value = applyMap($value, $mapRanges);
    }
    return $value;
}

/**
 * Part 1: Find the lowest location number for any initial seed.
 */
function part1(array $seeds, array $maps): int {
    $minLocation = PHP_INT_MAX;
    foreach ($seeds as $seed) {
        $location = seedToLocation($seed, $maps);
        if ($location < $minLocation) {
            $minLocation = $location;
        }
    }
    return $minLocation;
}

/**
 * Apply a map to a list of ranges, returning new ranges.
 */
function applyMapToRanges(array $inputRanges, array $mapRanges): array {
    $result = [];

    foreach ($inputRanges as $inputRange) {
        $start = $inputRange[0];
        $end = $inputRange[1];
        $remaining = [[$start, $end]];

        foreach ($mapRanges as $range) {
            $dstStart = $range['dst_start'];
            $srcStart = $range['src_start'];
            $length = $range['length'];
            $srcEnd = $srcStart + $length;

            $newRemaining = [];

            foreach ($remaining as $r) {
                $rStart = $r[0];
                $rEnd = $r[1];

                // Part before the map range (unmapped)
                if ($rStart < $srcStart) {
                    $newRemaining[] = [$rStart, min($rEnd, $srcStart)];
                }

                // Part within the map range (mapped)
                $overlapStart = max($rStart, $srcStart);
                $overlapEnd = min($rEnd, $srcEnd);
                if ($overlapStart < $overlapEnd) {
                    $offset = $dstStart - $srcStart;
                    $result[] = [$overlapStart + $offset, $overlapEnd + $offset];
                }

                // Part after the map range (unmapped)
                if ($rEnd > $srcEnd) {
                    $newRemaining[] = [max($rStart, $srcEnd), $rEnd];
                }
            }

            $remaining = $newRemaining;
        }

        // Any remaining parts are unmapped (identity)
        foreach ($remaining as $r) {
            $result[] = $r;
        }
    }

    return $result;
}

/**
 * Part 2: Find the lowest location for seed ranges.
 */
function part2(array $seeds, array $maps): int {
    // Convert seeds to ranges: pairs of [start, start + length]
    $ranges = [];
    for ($i = 0; $i < count($seeds); $i += 2) {
        $start = $seeds[$i];
        $length = $seeds[$i + 1];
        $ranges[] = [$start, $start + $length];
    }

    // Apply each map to the ranges
    foreach ($maps as $mapRanges) {
        $ranges = applyMapToRanges($ranges, $mapRanges);
    }

    // Find minimum start of any range
    $minStart = PHP_INT_MAX;
    foreach ($ranges as $range) {
        if ($range[0] < $minStart) {
            $minStart = $range[0];
        }
    }

    return $minStart;
}

function main(): void {
    $inputPath = __DIR__ . '/../input.txt';
    $text = file_get_contents($inputPath);

    [$seeds, $maps] = parseInput($text);

    echo 'Part 1: ' . part1($seeds, $maps) . "\n";
    echo 'Part 2: ' . part2($seeds, $maps) . "\n";
}

main();
