<?php
declare(strict_types=1);

/**
 * Parse a space-separated string of numbers into an associative array.
 * Uses keys for O(1) lookup during intersection counting.
 *
 * @param string $str Space-separated numbers
 * @return array<int, bool> Numbers as keys with true values
 */
function parseNumbers(string $str): array
{
    $result = [];
    foreach (preg_split('/\s+/', trim($str)) as $num) {
        if ($num !== '') {
            $result[(int)$num] = true;
        }
    }
    return $result;
}

/**
 * Parse all scratchcard lines into structured card data.
 *
 * @param array<string> $lines Input lines
 * @return array<array{0: array<int, bool>, 1: array<int, bool>}> Parsed cards
 */
function parseCards(array $lines): array
{
    $cards = [];
    foreach ($lines as $line) {
        [, $numbers] = explode(':', $line);
        [$winningPart, $havePart] = explode('|', $numbers);

        $cards[] = [
            parseNumbers($winningPart),
            parseNumbers($havePart),
        ];
    }
    return $cards;
}

/**
 * Count matching numbers between winning numbers and numbers we have.
 *
 * @param array<int, bool> $winning Winning numbers
 * @param array<int, bool> $have Numbers we have
 * @return int Number of matches
 */
function countMatches(array $winning, array $have): int
{
    return count(array_intersect_key($have, $winning));
}

/**
 * Part 1: Calculate total points from scratchcards.
 * Each card scores 2^(matches-1) points if it has any matches.
 *
 * @param array<array{0: array<int, bool>, 1: array<int, bool>}> $cards Parsed cards
 * @return int Total points
 */
function part1(array $cards): int
{
    $total = 0;

    foreach ($cards as [$winning, $have]) {
        $matches = countMatches($winning, $have);
        if ($matches > 0) {
            $total += 2 ** ($matches - 1);
        }
    }

    return $total;
}

/**
 * Part 2: Calculate total number of scratchcards including copies.
 * Each match on card N wins a copy of cards N+1 through N+matches.
 *
 * @param array<array{0: array<int, bool>, 1: array<int, bool>}> $cards Parsed cards
 * @return int Total number of cards
 */
function part2(array $cards): int
{
    $numCards = count($cards);

    // Pre-calculate matches for each card
    $matchCounts = array_map(
        fn(array $card): int => countMatches($card[0], $card[1]),
        $cards
    );

    // Track copies of each card (start with 1 original each)
    $copies = array_fill(0, $numCards, 1);

    // Process each card and add copies to subsequent cards
    for ($i = 0; $i < $numCards; $i++) {
        $matches = $matchCounts[$i];
        for ($j = $i + 1; $j < min($i + 1 + $matches, $numCards); $j++) {
            $copies[$j] += $copies[$i];
        }
    }

    return array_sum($copies);
}

// Read input
$input = file_get_contents(__DIR__ . '/../input.txt');
$lines = explode("\n", trim($input));

// Parse cards once and pass to both parts
$cards = parseCards($lines);

echo "Part 1: " . part1($cards) . "\n";
echo "Part 2: " . part2($cards) . "\n";
