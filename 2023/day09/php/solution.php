<?php

$inputText = trim(file_get_contents(__DIR__ . '/../input.txt'));

function parseInput(string $text): array {
    $lines = explode("\n", $text);
    return array_map(function($line) {
        return array_map('intval', preg_split('/\s+/', trim($line)));
    }, $lines);
}

function getDifferences(array $seq): array {
    $diffs = [];
    for ($i = 0; $i < count($seq) - 1; $i++) {
        $diffs[] = $seq[$i + 1] - $seq[$i];
    }
    return $diffs;
}

function allZeros(array $seq): bool {
    foreach ($seq as $val) {
        if ($val !== 0) {
            return false;
        }
    }
    return true;
}

function extrapolateNext(array $seq): int {
    $sequences = [$seq];
    $current = $seq;

    while (!allZeros($current)) {
        $current = getDifferences($current);
        $sequences[] = $current;
    }

    for ($i = count($sequences) - 2; $i >= 0; $i--) {
        $sequences[$i][] = $sequences[$i][count($sequences[$i]) - 1] + $sequences[$i + 1][count($sequences[$i + 1]) - 1];
    }

    return $sequences[0][count($sequences[0]) - 1];
}

function extrapolatePrev(array $seq): int {
    $sequences = [$seq];
    $current = $seq;

    while (!allZeros($current)) {
        $current = getDifferences($current);
        $sequences[] = $current;
    }

    for ($i = count($sequences) - 2; $i >= 0; $i--) {
        array_unshift($sequences[$i], $sequences[$i][0] - $sequences[$i + 1][0]);
    }

    return $sequences[0][0];
}

$histories = parseInput($inputText);

function part1(array $histories): int {
    $sum = 0;
    foreach ($histories as $history) {
        $sum += extrapolateNext($history);
    }
    return $sum;
}

function part2(array $histories): int {
    $sum = 0;
    foreach ($histories as $history) {
        $sum += extrapolatePrev($history);
    }
    return $sum;
}

echo "Part 1: " . part1($histories) . "\n";
echo "Part 2: " . part2($histories) . "\n";
