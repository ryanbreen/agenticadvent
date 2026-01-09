<?php
/**
 * Advent of Code 2022 Day 10: Cathode-Ray Tube
 */

/**
 * Generator that simulates CPU and yields [cycle, X] for each cycle.
 */
function simulateCpu(array $instructions): Generator {
    $x = 1;
    $cycle = 0;

    foreach ($instructions as $line) {
        if ($line === 'noop') {
            $cycle++;
            yield [$cycle, $x];
        } else {
            // addx V
            $v = (int)explode(' ', $line)[1];
            $cycle++;
            yield [$cycle, $x];
            $cycle++;
            yield [$cycle, $x];
            $x += $v;
        }
    }
}

/**
 * Part 1: Sum signal strengths at cycles 20, 60, 100, 140, 180, 220.
 */
function part1(array $instructions): int {
    $targetCycles = [20, 60, 100, 140, 180, 220];
    $total = 0;

    foreach (simulateCpu($instructions) as [$cycle, $x]) {
        if (in_array($cycle, $targetCycles)) {
            $total += $cycle * $x;
        }
    }

    return $total;
}

/**
 * Part 2: Render CRT display. Sprite is 3 pixels wide centered at X.
 */
function part2(array $instructions): string {
    $screen = [];
    $row = '';

    foreach (simulateCpu($instructions) as [$cycle, $x]) {
        $pos = ($cycle - 1) % 40; // CRT position in current row
        if (abs($pos - $x) <= 1) {
            $row .= '#';
        } else {
            $row .= '.';
        }

        if ($cycle % 40 === 0) {
            $screen[] = $row;
            $row = '';
        }
    }

    return implode("\n", $screen);
}

function main(): void {
    $inputFile = dirname(__FILE__) . '/../input.txt';
    $content = trim(file_get_contents($inputFile));
    $instructions = explode("\n", $content);

    echo 'Part 1: ' . part1($instructions) . "\n";
    echo "Part 2:\n";
    echo part2($instructions) . "\n";
}

main();
