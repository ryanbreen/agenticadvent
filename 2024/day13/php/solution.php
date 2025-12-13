<?php

$input_text = trim(file_get_contents(__DIR__ . '/../input.txt'));

/**
 * Parse claw machine configurations.
 * @return array Array of machine configurations [ax, ay, bx, by, px, py]
 */
function parse_machines(string $text): array {
    $machines = [];
    $blocks = explode("\n\n", $text);

    foreach ($blocks as $block) {
        $lines = explode("\n", trim($block));

        // Button A: X+ax, Y+ay
        preg_match('/Button A: X\+(\d+), Y\+(\d+)/', $lines[0], $a_match);
        $ax = (int)$a_match[1];
        $ay = (int)$a_match[2];

        // Button B: X+bx, Y+by
        preg_match('/Button B: X\+(\d+), Y\+(\d+)/', $lines[1], $b_match);
        $bx = (int)$b_match[1];
        $by = (int)$b_match[2];

        // Prize: X=px, Y=py
        preg_match('/Prize: X=(\d+), Y=(\d+)/', $lines[2], $p_match);
        $px = (int)$p_match[1];
        $py = (int)$p_match[2];

        $machines[] = [$ax, $ay, $bx, $by, $px, $py];
    }

    return $machines;
}

/**
 * Solve for button presses using Cramer's rule.
 *
 * System of equations:
 *   a*ax + b*bx = px
 *   a*ay + b*by = py
 *
 * Solution:
 *   det = ax*by - ay*bx
 *   a = (px*by - py*bx) / det
 *   b = (ax*py - ay*px) / det
 *
 * @param int $ax Button A X movement
 * @param int $ay Button A Y movement
 * @param int $bx Button B X movement
 * @param int $by Button B Y movement
 * @param int $px Prize X coordinate
 * @param int $py Prize Y coordinate
 * @param int|null $max_presses Maximum button presses allowed (null for no limit)
 * @return int|null Token cost (3*a + b) or null if no valid solution
 */
function solve_machine(int $ax, int $ay, int $bx, int $by, int $px, int $py, ?int $max_presses = null): ?int {
    $det = $ax * $by - $ay * $bx;

    if ($det == 0) {
        return null; // No unique solution
    }

    // Calculate using integer arithmetic
    $a_num = $px * $by - $py * $bx;
    $b_num = $ax * $py - $ay * $px;

    // Check if solutions are integers
    if ($a_num % $det != 0 || $b_num % $det != 0) {
        return null;
    }

    $a = intdiv($a_num, $det);
    $b = intdiv($b_num, $det);

    // Check non-negative
    if ($a < 0 || $b < 0) {
        return null;
    }

    // Check max presses constraint (Part 1)
    if ($max_presses !== null && ($a > $max_presses || $b > $max_presses)) {
        return null;
    }

    return 3 * $a + $b;
}

/**
 * Part 1: Max 100 presses per button.
 */
function part1(): int {
    global $input_text;
    $machines = parse_machines($input_text);
    $total = 0;

    foreach ($machines as $machine) {
        list($ax, $ay, $bx, $by, $px, $py) = $machine;
        $cost = solve_machine($ax, $ay, $bx, $by, $px, $py, 100);
        if ($cost !== null) {
            $total += $cost;
        }
    }

    return $total;
}

/**
 * Part 2: Prize coordinates shifted by 10^13, no press limit.
 */
function part2(): int {
    global $input_text;
    $machines = parse_machines($input_text);
    $offset = 10000000000000;
    $total = 0;

    foreach ($machines as $machine) {
        list($ax, $ay, $bx, $by, $px, $py) = $machine;
        // Shift prize coordinates
        $cost = solve_machine($ax, $ay, $bx, $by, $px + $offset, $py + $offset, null);
        if ($cost !== null) {
            $total += $cost;
        }
    }

    return $total;
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
