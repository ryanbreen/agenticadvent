#!/usr/bin/env php
<?php
/**
 * Test with the example from the problem.
 */

require_once __DIR__ . '/solution.php';

$example = <<<'EOD'
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
EOD;

[$locks, $keys] = parseInput($example);

echo "Locks:\n";
foreach ($locks as $lock) {
    echo "  " . implode(',', $lock) . "\n";
}

echo "\nKeys:\n";
foreach ($keys as $key) {
    echo "  " . implode(',', $key) . "\n";
}

$result = part1($locks, $keys);
echo "\nFitting pairs: $result\n";
echo "Expected: 3\n";
echo ($result === 3 ? "✓ PASS" : "✗ FAIL") . "\n";
