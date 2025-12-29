<?php
/**
 * Advent of Code 2023 Day 12: Hot Springs
 *
 * Uses memoized DP to count valid arrangements of operational and damaged springs.
 */

/**
 * Count valid arrangements using memoized dynamic programming.
 * State: (position, group_index, current_run_length)
 */
function countArrangements(string $pattern, array $groups): int {
    $memo = [];
    $patternLen = strlen($pattern);
    $groupsLen = count($groups);

    $dp = function(int $pos, int $groupIdx, int $currentRun) use (&$dp, &$memo, $pattern, $groups, $patternLen, $groupsLen): int {
        // Create memoization key
        $key = "$pos,$groupIdx,$currentRun";
        if (isset($memo[$key])) {
            return $memo[$key];
        }

        // Base case: reached end of pattern
        if ($pos === $patternLen) {
            // Valid if we've matched all groups and no partial run
            if ($groupIdx === $groupsLen && $currentRun === 0) {
                return 1;
            }
            // Or if we're on the last group and the run matches
            if ($groupIdx === $groupsLen - 1 && $groups[$groupIdx] === $currentRun) {
                return 1;
            }
            return 0;
        }

        $result = 0;
        $char = $pattern[$pos];

        // Option 1: Place operational spring (.)
        if ($char === '.' || $char === '?') {
            if ($currentRun === 0) {
                // No active run, just move forward
                $result += $dp($pos + 1, $groupIdx, 0);
            } elseif ($groupIdx < $groupsLen && $groups[$groupIdx] === $currentRun) {
                // End current run if it matches expected group size
                $result += $dp($pos + 1, $groupIdx + 1, 0);
            }
            // Otherwise invalid (run doesn't match group)
        }

        // Option 2: Place damaged spring (#)
        if ($char === '#' || $char === '?') {
            if ($groupIdx < $groupsLen && $currentRun < $groups[$groupIdx]) {
                // Can extend current run
                $result += $dp($pos + 1, $groupIdx, $currentRun + 1);
            }
            // Otherwise invalid (exceeds group size or no more groups)
        }

        $memo[$key] = $result;
        return $result;
    };

    return $dp(0, 0, 0);
}

/**
 * Parse a line into pattern and groups array.
 */
function parseLine(string $line): array {
    $parts = preg_split('/\s+/', trim($line));
    $pattern = $parts[0];
    $groups = array_map('intval', explode(',', $parts[1]));
    return [$pattern, $groups];
}

/**
 * Part 1: Sum of arrangement counts for all rows.
 */
function part1(array $lines): int {
    $total = 0;
    foreach ($lines as $line) {
        $line = trim($line);
        if (empty($line)) {
            continue;
        }
        [$pattern, $groups] = parseLine($line);
        $total += countArrangements($pattern, $groups);
    }
    return $total;
}

/**
 * Unfold pattern and groups by repeating them 5 times.
 */
function unfold(string $pattern, array $groups): array {
    $unfoldedPattern = implode('?', array_fill(0, 5, $pattern));
    $unfoldedGroups = [];
    for ($i = 0; $i < 5; $i++) {
        $unfoldedGroups = array_merge($unfoldedGroups, $groups);
    }
    return [$unfoldedPattern, $unfoldedGroups];
}

/**
 * Part 2: Sum of arrangement counts for all rows after unfolding.
 */
function part2(array $lines): int {
    $total = 0;
    foreach ($lines as $line) {
        $line = trim($line);
        if (empty($line)) {
            continue;
        }
        [$pattern, $groups] = parseLine($line);
        [$unfoldedPattern, $unfoldedGroups] = unfold($pattern, $groups);
        $total += countArrangements($unfoldedPattern, $unfoldedGroups);
    }
    return $total;
}

// Main execution
$input = file_get_contents(__DIR__ . '/../input.txt');
$lines = explode("\n", $input);

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
