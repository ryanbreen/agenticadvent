#!/usr/bin/env php
<?php

function parseInput($text) {
    $equations = [];
    $lines = array_filter(explode("\n", trim($text)));

    foreach ($lines as $line) {
        list($target, $nums) = explode(': ', $line);
        $numbers = array_map('intval', explode(' ', $nums));
        $equations[] = [(int)$target, $numbers];
    }

    return $equations;
}

function evaluate($nums, $ops) {
    $result = $nums[0];

    for ($i = 0; $i < count($ops); $i++) {
        $op = $ops[$i];

        if ($op === '+') {
            $result += $nums[$i + 1];
        } elseif ($op === '*') {
            $result *= $nums[$i + 1];
        } elseif ($op === '||') {
            $result = (int)($result . $nums[$i + 1]);
        }
    }

    return $result;
}

function generateOperatorCombinations($operators, $count) {
    if ($count === 0) {
        return [[]];
    }

    $result = [];
    $subCombinations = generateOperatorCombinations($operators, $count - 1);

    foreach ($operators as $op) {
        foreach ($subCombinations as $subCombo) {
            $result[] = array_merge([$op], $subCombo);
        }
    }

    return $result;
}

function canMakeTarget($target, $nums, $operators) {
    $nOps = count($nums) - 1;
    $combinations = generateOperatorCombinations($operators, $nOps);

    foreach ($combinations as $ops) {
        if (evaluate($nums, $ops) === $target) {
            return true;
        }
    }

    return false;
}

function part1($equations) {
    $operators = ['+', '*'];
    $total = 0;

    foreach ($equations as list($target, $nums)) {
        if (canMakeTarget($target, $nums, $operators)) {
            $total += $target;
        }
    }

    return $total;
}

function part2($equations) {
    $operators = ['+', '*', '||'];
    $total = 0;

    foreach ($equations as list($target, $nums)) {
        if (canMakeTarget($target, $nums, $operators)) {
            $total += $target;
        }
    }

    return $total;
}

// Main execution
$inputFile = __DIR__ . '/../input.txt';
$text = file_get_contents($inputFile);
$equations = parseInput($text);

echo 'Part 1: ' . part1($equations) . "\n";
echo 'Part 2: ' . part2($equations) . "\n";
