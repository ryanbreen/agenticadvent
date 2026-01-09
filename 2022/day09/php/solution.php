<?php

$DIRECTIONS = [
    'U' => [0, 1],
    'D' => [0, -1],
    'L' => [-1, 0],
    'R' => [1, 0],
];

function sign($x) {
    if ($x == 0) return 0;
    return $x > 0 ? 1 : -1;
}

function moveTail($head, $tail) {
    $dx = $head[0] - $tail[0];
    $dy = $head[1] - $tail[1];

    // If adjacent or overlapping, don't move
    if (abs($dx) <= 1 && abs($dy) <= 1) {
        return $tail;
    }

    // Move toward head
    return [$tail[0] + sign($dx), $tail[1] + sign($dy)];
}

function simulateRope($moves, $ropeLength) {
    global $DIRECTIONS;

    // Initialize knots at origin
    $knots = array_fill(0, $ropeLength, [0, 0]);
    $visited = [];
    $visited["0,0"] = true;

    foreach ($moves as $line) {
        $parts = explode(' ', $line);
        $direction = $parts[0];
        $count = (int)$parts[1];
        $dx = $DIRECTIONS[$direction][0];
        $dy = $DIRECTIONS[$direction][1];

        for ($step = 0; $step < $count; $step++) {
            // Move head
            $knots[0] = [$knots[0][0] + $dx, $knots[0][1] + $dy];

            // Move each subsequent knot
            for ($i = 1; $i < $ropeLength; $i++) {
                $knots[$i] = moveTail($knots[$i-1], $knots[$i]);
            }

            // Track tail position
            $tailPos = $knots[$ropeLength - 1];
            $key = $tailPos[0] . "," . $tailPos[1];
            $visited[$key] = true;
        }
    }

    return count($visited);
}

function part1($moves) {
    return simulateRope($moves, 2);
}

function part2($moves) {
    return simulateRope($moves, 10);
}

function main() {
    $scriptDir = dirname(__FILE__);
    $inputFile = $scriptDir . '/../input.txt';

    $content = file_get_contents($inputFile);
    $moves = array_filter(explode("\n", trim($content)));

    echo "Part 1: " . part1($moves) . "\n";
    echo "Part 2: " . part2($moves) . "\n";
}

main();
