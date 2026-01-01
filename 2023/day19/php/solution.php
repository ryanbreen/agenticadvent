#!/usr/bin/env php
<?php
declare(strict_types=1);

/**
 * Day 19: Aplenty - Workflow processing and range analysis.
 */

// Regex patterns for parsing
const PATTERN_CONDITION = '/([xmas])([<>])(\d+)/';
const PATTERN_PART_RATING = '/([xmas])=(\d+)/';

/**
 * Parse workflows and parts from input.
 */
function parseInput(string $filename): array {
    $text = trim(file_get_contents($filename));
    [$workflowSection, $partsSection] = explode("\n\n", $text);

    // Parse workflows
    $workflows = [];
    foreach (explode("\n", $workflowSection) as $line) {
        [$name, $rulesStr] = explode('{', $line);
        $rulesStr = rtrim($rulesStr, '}');
        $rules = [];
        foreach (explode(',', $rulesStr) as $rule) {
            if (strpos($rule, ':') !== false) {
                [$condition, $destination] = explode(':', $rule);
                preg_match(PATTERN_CONDITION, $condition, $matches);
                $attr = $matches[1];
                $op = $matches[2];
                $value = (int)$matches[3];
                $rules[] = [$attr, $op, $value, $destination];
            } else {
                $rules[] = [null, null, null, $rule];  // Default rule
            }
        }
        $workflows[$name] = $rules;
    }

    // Parse parts
    $parts = [];
    foreach (explode("\n", $partsSection) as $line) {
        $part = [];
        preg_match_all(PATTERN_PART_RATING, $line, $matches, PREG_SET_ORDER);
        foreach ($matches as $match) {
            $part[$match[1]] = (int)$match[2];
        }
        $parts[] = $part;
    }

    return [$workflows, $parts];
}

/**
 * Process a part through the workflows, return true if accepted.
 */
function processPart(array $workflows, array $part): bool {
    $current = 'in';

    while ($current !== 'A' && $current !== 'R') {
        foreach ($workflows[$current] as [$attr, $op, $value, $destination]) {
            if ($attr === null) {  // Default rule
                $current = $destination;
                break;
            } elseif ($op === '<' && $part[$attr] < $value) {
                $current = $destination;
                break;
            } elseif ($op === '>' && $part[$attr] > $value) {
                $current = $destination;
                break;
            }
        }
    }

    return $current === 'A';
}

/**
 * Part 1: Sum ratings of accepted parts.
 */
function part1(array $workflows, array $parts): int {
    $total = 0;
    foreach ($parts as $part) {
        if (processPart($workflows, $part)) {
            $total += $part['x'] + $part['m'] + $part['a'] + $part['s'];
        }
    }
    return $total;
}

/**
 * Count combinations of xmas values that lead to acceptance.
 * Uses range splitting to process all possible paths through workflows.
 *
 * ranges: array mapping 'x', 'm', 'a', 's' to [min, max] inclusive ranges
 * Returns a GMP number for the count.
 */
function countAccepted(array $workflows, string $workflow, array $ranges): \GMP {
    if ($workflow === 'R') {
        return gmp_init(0);
    }
    if ($workflow === 'A') {
        // Count all combinations in current ranges
        $result = gmp_init(1);
        foreach ($ranges as [$lo, $hi]) {
            $rangeSize = max(0, $hi - $lo + 1);
            $result = gmp_mul($result, $rangeSize);
        }
        return $result;
    }

    $total = gmp_init(0);

    foreach ($workflows[$workflow] as [$attr, $op, $value, $destination]) {
        if ($attr === null) {  // Default rule
            $total = gmp_add($total, countAccepted($workflows, $destination, $ranges));
        } else {
            [$lo, $hi] = $ranges[$attr];

            if ($op === '<') {
                // Split: [lo, value-1] goes to destination, [value, hi] continues
                if ($lo < $value) {
                    // Part that matches the condition
                    $newRanges = $ranges;
                    $newRanges[$attr] = [$lo, min($hi, $value - 1)];
                    $total = gmp_add($total, countAccepted($workflows, $destination, $newRanges));
                }
                // Remaining part continues to next rule
                if ($hi >= $value) {
                    $ranges[$attr] = [max($lo, $value), $hi];
                } else {
                    break;  // No remaining range
                }
            } else {  // op === '>'
                // Split: [value+1, hi] goes to destination, [lo, value] continues
                if ($hi > $value) {
                    // Part that matches the condition
                    $newRanges = $ranges;
                    $newRanges[$attr] = [max($lo, $value + 1), $hi];
                    $total = gmp_add($total, countAccepted($workflows, $destination, $newRanges));
                }
                // Remaining part continues to next rule
                if ($lo <= $value) {
                    $ranges[$attr] = [$lo, min($hi, $value)];
                } else {
                    break;  // No remaining range
                }
            }
        }
    }

    return $total;
}

/**
 * Part 2: Count all possible accepted combinations (1-4000 for each rating).
 */
function part2(array $workflows): string {
    $initialRanges = [
        'x' => [1, 4000],
        'm' => [1, 4000],
        'a' => [1, 4000],
        's' => [1, 4000]
    ];
    return gmp_strval(countAccepted($workflows, 'in', $initialRanges));
}

function main(): void {
    [$workflows, $parts] = parseInput(__DIR__ . '/../input.txt');
    echo "Part 1: " . part1($workflows, $parts) . "\n";
    echo "Part 2: " . part2($workflows) . "\n";
}

main();
