<?php

function parseInput($filename) {
    $content = file_get_contents($filename);
    $parts = explode("\n\n", $content);

    $stackLines = explode("\n", $parts[0]);
    $moveLines = array_filter(explode("\n", trim($parts[1])));

    // Find number of stacks from the last line (the numbers)
    $numStacks = count(preg_split('/\s+/', trim($stackLines[count($stackLines) - 1])));

    // Parse stacks (top-down, excluding the number line)
    $stacks = array_fill(0, $numStacks, []);
    for ($lineIdx = 0; $lineIdx < count($stackLines) - 1; $lineIdx++) {
        $line = $stackLines[$lineIdx];
        for ($i = 0; $i < $numStacks; $i++) {
            $pos = 1 + $i * 4;  // Position of crate letter
            if ($pos < strlen($line) && $line[$pos] !== ' ') {
                $stacks[$i][] = $line[$pos];
            }
        }
    }

    // Reverse so bottom is at index 0
    for ($i = 0; $i < $numStacks; $i++) {
        $stacks[$i] = array_reverse($stacks[$i]);
    }

    // Parse moves
    $moves = [];
    foreach ($moveLines as $line) {
        if (preg_match('/move (\d+) from (\d+) to (\d+)/', $line, $match)) {
            $count = (int)$match[1];
            $fromStack = (int)$match[2] - 1;  // 0-indexed
            $toStack = (int)$match[3] - 1;
            $moves[] = [$count, $fromStack, $toStack];
        }
    }

    return [$stacks, $moves];
}

function part1($stacks, $moves) {
    // Deep copy stacks
    $stacks = array_map(function($s) { return array_values($s); }, $stacks);

    foreach ($moves as $move) {
        list($count, $fromStack, $toStack) = $move;
        for ($i = 0; $i < $count; $i++) {
            $crate = array_pop($stacks[$fromStack]);
            $stacks[$toStack][] = $crate;
        }
    }

    $result = '';
    foreach ($stacks as $stack) {
        if (!empty($stack)) {
            $result .= $stack[count($stack) - 1];
        }
    }
    return $result;
}

function part2($stacks, $moves) {
    // Deep copy stacks
    $stacks = array_map(function($s) { return array_values($s); }, $stacks);

    foreach ($moves as $move) {
        list($count, $fromStack, $toStack) = $move;
        // Move multiple crates at once (preserve order)
        $crates = array_slice($stacks[$fromStack], -$count);
        $stacks[$fromStack] = array_slice($stacks[$fromStack], 0, -$count);
        $stacks[$toStack] = array_merge($stacks[$toStack], $crates);
    }

    $result = '';
    foreach ($stacks as $stack) {
        if (!empty($stack)) {
            $result .= $stack[count($stack) - 1];
        }
    }
    return $result;
}

function main() {
    $scriptDir = dirname(__FILE__);
    $inputFile = $scriptDir . '/../input.txt';

    list($stacks, $moves) = parseInput($inputFile);

    echo 'Part 1: ' . part1($stacks, $moves) . "\n";
    echo 'Part 2: ' . part2($stacks, $moves) . "\n";
}

main();
