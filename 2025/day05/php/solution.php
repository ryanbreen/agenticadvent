<?php

$input_text = file_get_contents(__DIR__ . '/../input.txt');
$lines = explode("\n", trim($input_text));

function parseInput($lines) {
    // Find the blank line separator
    $blank_idx = array_search("", $lines);

    // Parse ranges from the first section
    $ranges = [];
    for ($i = 0; $i < $blank_idx; $i++) {
        [$start, $end] = array_map('intval', explode("-", $lines[$i]));
        $ranges[] = [$start, $end];
    }

    // Parse ingredient IDs from the second section
    $ingredient_ids = [];
    for ($i = $blank_idx + 1; $i < count($lines); $i++) {
        if (!empty($lines[$i])) {
            $ingredient_ids[] = intval($lines[$i]);
        }
    }

    return [$ranges, $ingredient_ids];
}

function part1($lines) {
    [$ranges, $ingredient_ids] = parseInput($lines);

    // Count how many ingredient IDs fall within any range
    $fresh_count = 0;
    foreach ($ingredient_ids as $ingredient_id) {
        foreach ($ranges as [$start, $end]) {
            if ($ingredient_id >= $start && $ingredient_id <= $end) {
                $fresh_count++;
                break; // Found a match, no need to check other ranges
            }
        }
    }

    return $fresh_count;
}

function part2($lines) {
    [$ranges, $ingredient_ids] = parseInput($lines);

    // Sort ranges by start position
    usort($ranges, function($a, $b) {
        return $a[0] <=> $b[0];
    });

    // Merge overlapping ranges
    $merged = [];
    foreach ($ranges as [$start, $end]) {
        if (!empty($merged) && $start <= $merged[count($merged) - 1][1] + 1) {
            // Overlapping or adjacent - merge with the last range
            $last_idx = count($merged) - 1;
            $merged[$last_idx][1] = max($merged[$last_idx][1], $end);
        } else {
            // No overlap - add as new range
            $merged[] = [$start, $end];
        }
    }

    // Count total unique IDs covered by merged ranges
    $total_count = 0;
    foreach ($merged as [$start, $end]) {
        $total_count += ($end - $start + 1);
    }

    return $total_count;
}

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
