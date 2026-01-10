<?php

function parseInput(): array {
    $inputPath = __DIR__ . '/../input.txt';
    $lines = file($inputPath, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    $commands = [];
    foreach ($lines as $line) {
        $parts = explode(' ', trim($line));
        $commands[] = [$parts[0], (int)$parts[1]];
    }
    return $commands;
}

function part1(array $commands): int {
    $horizontal = 0;
    $depth = 0;
    foreach ($commands as [$cmd, $val]) {
        switch ($cmd) {
            case 'forward':
                $horizontal += $val;
                break;
            case 'down':
                $depth += $val;
                break;
            case 'up':
                $depth -= $val;
                break;
        }
    }
    return $horizontal * $depth;
}

function part2(array $commands): int {
    $horizontal = 0;
    $depth = 0;
    $aim = 0;
    foreach ($commands as [$cmd, $val]) {
        switch ($cmd) {
            case 'forward':
                $horizontal += $val;
                $depth += $aim * $val;
                break;
            case 'down':
                $aim += $val;
                break;
            case 'up':
                $aim -= $val;
                break;
        }
    }
    return $horizontal * $depth;
}

$commands = parseInput();
echo "Part 1: " . part1($commands) . "\n";
echo "Part 2: " . part2($commands) . "\n";
