#!/usr/bin/env php
<?php

function parseInput(string $text): array {
    $monkeys = [];
    foreach (explode("\n", trim($text)) as $line) {
        [$name, $job] = explode(': ', $line);
        $parts = explode(' ', $job);
        if (count($parts) === 1) {
            $monkeys[$name] = (int)$parts[0];
        } else {
            $monkeys[$name] = [$parts[0], $parts[1], $parts[2]];
        }
    }
    return $monkeys;
}

function evaluate(array $monkeys, string $name, array &$memo = []): int {
    if (isset($memo[$name])) {
        return $memo[$name];
    }

    $job = $monkeys[$name];
    if (is_int($job)) {
        return $job;
    }

    [$left, $op, $right] = $job;
    $leftVal = evaluate($monkeys, $left, $memo);
    $rightVal = evaluate($monkeys, $right, $memo);

    $result = match($op) {
        '+' => $leftVal + $rightVal,
        '-' => $leftVal - $rightVal,
        '*' => $leftVal * $rightVal,
        '/' => intdiv($leftVal, $rightVal),
    };

    $memo[$name] = $result;
    return $result;
}

function part1(string $text): int {
    $monkeys = parseInput($text);
    $memo = [];
    return evaluate($monkeys, 'root', $memo);
}

function containsHumn(array $monkeys, string $name, array &$memo = []): bool {
    if (isset($memo[$name])) {
        return $memo[$name];
    }
    if ($name === 'humn') {
        return true;
    }

    $job = $monkeys[$name];
    if (is_int($job)) {
        $memo[$name] = false;
        return false;
    }

    [$left, , $right] = $job;
    $result = containsHumn($monkeys, $left, $memo) || containsHumn($monkeys, $right, $memo);
    $memo[$name] = $result;
    return $result;
}

function solveForHumn(array $monkeys, string $name, int $target): int {
    if ($name === 'humn') {
        return $target;
    }

    $job = $monkeys[$name];
    if (is_int($job)) {
        throw new Exception("Cannot solve for a constant");
    }

    [$left, $op, $right] = $job;

    $humnMemo = [];
    $leftHasHumn = containsHumn($monkeys, $left, $humnMemo);

    if ($leftHasHumn) {
        $evalMemo = [];
        $rightVal = evaluate($monkeys, $right, $evalMemo);

        $newTarget = match($op) {
            '+' => $target - $rightVal,       // left + right = target => left = target - right
            '-' => $target + $rightVal,       // left - right = target => left = target + right
            '*' => intdiv($target, $rightVal),// left * right = target => left = target / right
            '/' => $target * $rightVal,       // left / right = target => left = target * right
        };

        return solveForHumn($monkeys, $left, $newTarget);
    } else {
        $evalMemo = [];
        $leftVal = evaluate($monkeys, $left, $evalMemo);

        $newTarget = match($op) {
            '+' => $target - $leftVal,       // left + right = target => right = target - left
            '-' => $leftVal - $target,       // left - right = target => right = left - target
            '*' => intdiv($target, $leftVal),// left * right = target => right = target / left
            '/' => intdiv($leftVal, $target),// left / right = target => right = left / target
        };

        return solveForHumn($monkeys, $right, $newTarget);
    }
}

function part2(string $text): int {
    $monkeys = parseInput($text);

    [$left, , $right] = $monkeys['root'];

    $humnMemo = [];
    $leftHasHumn = containsHumn($monkeys, $left, $humnMemo);

    if ($leftHasHumn) {
        $evalMemo = [];
        $target = evaluate($monkeys, $right, $evalMemo);
        return solveForHumn($monkeys, $left, $target);
    } else {
        $evalMemo = [];
        $target = evaluate($monkeys, $left, $evalMemo);
        return solveForHumn($monkeys, $right, $target);
    }
}

$scriptDir = dirname(__FILE__);
$inputFile = $scriptDir . '/../input.txt';
$text = file_get_contents($inputFile);

echo "Part 1: " . part1($text) . "\n";
echo "Part 2: " . part2($text) . "\n";
