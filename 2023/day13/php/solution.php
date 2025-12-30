<?php

declare(strict_types=1);

/**
 * Parse input text into pattern arrays.
 *
 * @param string $text Raw input text
 * @return array<int, array<int, string>> Array of patterns (each pattern is array of strings)
 */
function parseInput(string $text): array
{
    $blocks = array_filter(explode("\n\n", trim($text)));
    return array_map(fn(string $block): array => explode("\n", $block), $blocks);
}

/**
 * Count character differences between two strings.
 *
 * @param string $s1 First string
 * @param string $s2 Second string
 * @return int Number of differing characters
 */
function countDifferences(string $s1, string $s2): int
{
    $len = min(strlen($s1), strlen($s2));
    $diff = 0;
    for ($i = 0; $i < $len; $i++) {
        if ($s1[$i] !== $s2[$i]) {
            $diff++;
        }
    }
    return $diff;
}

/**
 * Find vertical reflection column with exactly $targetDiff differences.
 *
 * @param array<int, string> $pattern The pattern to analyze
 * @param int $targetDiff Required total differences (0 for Part 1, 1 for Part 2)
 * @return int Column number of reflection, or 0 if not found
 */
function findVerticalReflection(array $pattern, int $targetDiff): int
{
    if (empty($pattern)) {
        return 0;
    }

    $width = strlen($pattern[0]);

    for ($col = 1; $col < $width; $col++) {
        $totalDiff = 0;

        foreach ($pattern as $row) {
            $left = strrev(substr($row, 0, $col));
            $right = substr($row, $col);
            $minLen = min(strlen($left), strlen($right));

            $totalDiff += countDifferences(substr($left, 0, $minLen), substr($right, 0, $minLen));

            if ($totalDiff > $targetDiff) {
                break;
            }
        }

        if ($totalDiff === $targetDiff) {
            return $col;
        }
    }

    return 0;
}

/**
 * Find horizontal reflection row with exactly $targetDiff differences.
 *
 * @param array<int, string> $pattern The pattern to analyze
 * @param int $targetDiff Required total differences (0 for Part 1, 1 for Part 2)
 * @return int Row number of reflection, or 0 if not found
 */
function findHorizontalReflection(array $pattern, int $targetDiff): int
{
    if (empty($pattern)) {
        return 0;
    }

    $height = count($pattern);

    for ($row = 1; $row < $height; $row++) {
        $top = array_reverse(array_slice($pattern, 0, $row));
        $bottom = array_slice($pattern, $row);
        $minLen = min(count($top), count($bottom));

        $totalDiff = 0;
        for ($i = 0; $i < $minLen; $i++) {
            $totalDiff += countDifferences($top[$i], $bottom[$i]);

            if ($totalDiff > $targetDiff) {
                break;
            }
        }

        if ($totalDiff === $targetDiff) {
            return $row;
        }
    }

    return 0;
}

/**
 * Summarize a pattern by finding its reflection value.
 *
 * @param array<int, string> $pattern The pattern to analyze
 * @param int $targetDiff Required differences (0 for Part 1, 1 for Part 2)
 * @return int Reflection value (column number or row number * 100)
 */
function summarizePattern(array $pattern, int $targetDiff): int
{
    $vertical = findVerticalReflection($pattern, $targetDiff);
    if ($vertical > 0) {
        return $vertical;
    }

    return findHorizontalReflection($pattern, $targetDiff) * 100;
}

/**
 * Solve Part 1: Find perfect reflections (0 differences).
 *
 * @param array<int, array<int, string>> $patterns All patterns
 * @return int Sum of all reflection values
 */
function part1(array $patterns): int
{
    return array_reduce(
        $patterns,
        fn(int $sum, array $pattern): int => $sum + summarizePattern($pattern, 0),
        0
    );
}

/**
 * Solve Part 2: Find reflections with exactly 1 smudge difference.
 *
 * @param array<int, array<int, string>> $patterns All patterns
 * @return int Sum of all reflection values
 */
function part2(array $patterns): int
{
    return array_reduce(
        $patterns,
        fn(int $sum, array $pattern): int => $sum + summarizePattern($pattern, 1),
        0
    );
}

// Main execution
$inputFile = __DIR__ . '/../input.txt';
$text = file_get_contents($inputFile);

if ($text === false) {
    throw new RuntimeException("Failed to read input file: {$inputFile}");
}

$patterns = parseInput($text);

echo "Part 1: " . part1($patterns) . "\n";
echo "Part 2: " . part2($patterns) . "\n";
