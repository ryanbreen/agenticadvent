#!/usr/bin/env php
<?php

function parseInput(string $filename): array {
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    $rounds = [];
    foreach ($lines as $line) {
        $parts = explode(' ', trim($line));
        if (count($parts) === 2) {
            $rounds[] = $parts;
        }
    }
    return $rounds;
}

function part1(array $rounds): int {
    // X=Rock, Y=Paper, Z=Scissors
    // Shape scores: Rock=1, Paper=2, Scissors=3
    $shapeScore = ['X' => 1, 'Y' => 2, 'Z' => 3];

    // Outcome: 0=loss, 3=draw, 6=win
    // A=Rock, B=Paper, C=Scissors
    $outcomes = [
        'A' => ['X' => 3, 'Y' => 6, 'Z' => 0],  // Rock vs X/Y/Z
        'B' => ['X' => 0, 'Y' => 3, 'Z' => 6],  // Paper vs X/Y/Z
        'C' => ['X' => 6, 'Y' => 0, 'Z' => 3],  // Scissors vs X/Y/Z
    ];

    $total = 0;
    foreach ($rounds as [$opp, $me]) {
        $total += $shapeScore[$me] + $outcomes[$opp][$me];
    }
    return $total;
}

function part2(array $rounds): int {
    // X=lose, Y=draw, Z=win
    // What shape to play given opponent and desired outcome
    // Returns the shape we play (1=Rock, 2=Paper, 3=Scissors)
    $choices = [
        'A' => ['X' => 3, 'Y' => 1, 'Z' => 2],  // Rock: lose->Scissors, draw->Rock, win->Paper
        'B' => ['X' => 1, 'Y' => 2, 'Z' => 3],  // Paper: lose->Rock, draw->Paper, win->Scissors
        'C' => ['X' => 2, 'Y' => 3, 'Z' => 1],  // Scissors: lose->Paper, draw->Scissors, win->Rock
    ];

    $outcomeScore = ['X' => 0, 'Y' => 3, 'Z' => 6];

    $total = 0;
    foreach ($rounds as [$opp, $outcome]) {
        $total += $choices[$opp][$outcome] + $outcomeScore[$outcome];
    }
    return $total;
}

function main(): void {
    $scriptDir = dirname(__FILE__);
    $inputFile = $scriptDir . '/../input.txt';

    $rounds = parseInput($inputFile);

    echo 'Part 1: ' . part1($rounds) . PHP_EOL;
    echo 'Part 2: ' . part2($rounds) . PHP_EOL;
}

main();
