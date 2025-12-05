<?php

// Read input file
$inputPath = __DIR__ . '/../input.txt';
$inputText = trim(file_get_contents($inputPath));

// Parse input - split into rules and updates sections
$sections = explode("\n\n", $inputText);
$rulesSection = explode("\n", $sections[0]);
$updatesSection = explode("\n", $sections[1]);

// Parse rules: X|Y means X must come before Y
// Store as: rules[X] = array of pages that must come AFTER X
$rules = [];
foreach ($rulesSection as $rule) {
    list($before, $after) = explode('|', $rule);
    $before = (int)$before;
    $after = (int)$after;

    if (!isset($rules[$before])) {
        $rules[$before] = [];
    }
    $rules[$before][] = $after;
}

// Parse updates
$updates = [];
foreach ($updatesSection as $line) {
    $updates[] = array_map('intval', explode(',', $line));
}

/**
 * Check if an update is in valid order according to rules.
 */
function isValidOrder($update, $rules) {
    // Create a map of page positions
    $pagePositions = [];
    foreach ($update as $i => $page) {
        $pagePositions[$page] = $i;
    }

    foreach ($update as $i => $page) {
        // Check all pages that must come after this page
        if (isset($rules[$page])) {
            foreach ($rules[$page] as $mustBeAfter) {
                if (isset($pagePositions[$mustBeAfter])) {
                    if ($pagePositions[$mustBeAfter] < $i) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

/**
 * Part 1: Sum middle page numbers of correctly-ordered updates
 */
function part1($updates, $rules) {
    $total = 0;
    foreach ($updates as $update) {
        if (isValidOrder($update, $rules)) {
            $middleIdx = intdiv(count($update), 2);
            $total += $update[$middleIdx];
        }
    }
    return $total;
}

/**
 * Reorder an update to satisfy all rules using a custom comparator.
 */
function fixOrder($update, $rules) {
    usort($update, function($a, $b) use ($rules) {
        // If a must come before b, return -1
        if (isset($rules[$a]) && in_array($b, $rules[$a])) {
            return -1;
        }
        // If b must come before a, return 1
        if (isset($rules[$b]) && in_array($a, $rules[$b])) {
            return 1;
        }
        return 0;
    });
    return $update;
}

/**
 * Part 2: Fix incorrectly-ordered updates and sum their middle page numbers
 */
function part2($updates, $rules) {
    $total = 0;
    foreach ($updates as $update) {
        if (!isValidOrder($update, $rules)) {
            $fixed = fixOrder($update, $rules);
            $middleIdx = intdiv(count($fixed), 2);
            $total += $fixed[$middleIdx];
        }
    }
    return $total;
}

// Run both parts
echo "Part 1: " . part1($updates, $rules) . "\n";
echo "Part 2: " . part2($updates, $rules) . "\n";
