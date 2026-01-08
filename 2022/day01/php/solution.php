<?php

/**
 * Parse input into array of calorie totals per elf.
 */
function parseInput(string $filename): array
{
    $content = trim(file_get_contents($filename));
    $groups = explode("\n\n", $content);

    $elves = [];
    foreach ($groups as $group) {
        $lines = explode("\n", $group);
        $total = 0;
        foreach ($lines as $line) {
            if ($line !== '') {
                $total += (int)$line;
            }
        }
        $elves[] = $total;
    }

    return $elves;
}

/**
 * Part 1: Find the Elf carrying the most Calories.
 */
function part1(array $elves): int
{
    return max($elves);
}

/**
 * Part 2: Find total calories carried by top three Elves.
 */
function part2(array $elves): int
{
    rsort($elves);
    return $elves[0] + $elves[1] + $elves[2];
}

function main(): void
{
    $inputFile = __DIR__ . '/../input.txt';

    $elves = parseInput($inputFile);

    echo 'Part 1: ' . part1($elves) . PHP_EOL;
    echo 'Part 2: ' . part2($elves) . PHP_EOL;
}

main();
