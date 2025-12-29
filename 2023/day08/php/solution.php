<?php
/**
 * Advent of Code 2023 - Day 8: Haunted Wasteland
 *
 * Navigate a network following L/R instructions.
 * Part 1: Go from AAA to ZZZ
 * Part 2: Start from all nodes ending in 'A' simultaneously,
 *         find when all reach nodes ending in 'Z'
 */

/**
 * Parse the input into instructions and network.
 */
function parseInput(string $text): array {
    $lines = explode("\n", trim($text));
    $instructions = $lines[0];

    $network = [];
    $lineCount = count($lines);
    for ($i = 2; $i < $lineCount; $i++) {
        $line = trim($lines[$i]);
        if (empty($line)) {
            continue;
        }
        // Parse: AAA = (BBB, CCC)
        preg_match('/^(\w+) = \((\w+), (\w+)\)$/', $line, $matches);
        $network[$matches[1]] = [$matches[2], $matches[3]];
    }

    return [$instructions, $network];
}

/**
 * Part 1: Navigate from AAA to ZZZ following L/R instructions.
 */
function part1(string $instructions, array $network): int {
    $current = 'AAA';
    $steps = 0;
    $instructionLen = strlen($instructions);

    while ($current !== 'ZZZ') {
        $instruction = $instructions[$steps % $instructionLen];
        $current = $instruction === 'L' ? $network[$current][0] : $network[$current][1];
        $steps++;
    }

    return $steps;
}

/**
 * Part 2: Navigate all nodes ending in A simultaneously to nodes ending in Z.
 */
function part2(string $instructions, array $network): string {
    // Find all starting nodes (ending in A)
    $startNodes = array_filter(array_keys($network), fn($node) => str_ends_with($node, 'A'));

    $instructionLen = strlen($instructions);
    $cycleLengths = [];

    // For each starting node, find how many steps to reach a Z node
    foreach ($startNodes as $node) {
        $current = $node;
        $steps = 0;

        while (!str_ends_with($current, 'Z')) {
            $instruction = $instructions[$steps % $instructionLen];
            $current = $instruction === 'L' ? $network[$current][0] : $network[$current][1];
            $steps++;
        }

        $cycleLengths[] = $steps;
    }

    // Find LCM of all cycle lengths using GMP
    $result = array_reduce(
        $cycleLengths,
        fn($carry, $item) => gmp_lcm($carry, $item),
        gmp_init(1)
    );

    return gmp_strval($result);
}

// Main execution
$inputPath = __DIR__ . '/../input.txt';
$text = file_get_contents($inputPath);

[$instructions, $network] = parseInput($text);

echo "Part 1: " . part1($instructions, $network) . "\n";
echo "Part 2: " . part2($instructions, $network) . "\n";
