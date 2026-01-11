<?php

function parseInput(): array {
    $content = trim(file_get_contents(__DIR__ . '/../input.txt'));
    $sections = explode("\n\n", $content);

    $numbers = array_map('intval', explode(',', $sections[0]));

    $boards = [];
    for ($i = 1; $i < count($sections); $i++) {
        $board = [];
        $lines = explode("\n", trim($sections[$i]));
        foreach ($lines as $line) {
            $row = array_map('intval', preg_split('/\s+/', trim($line)));
            $board[] = $row;
        }
        $boards[] = $board;
    }

    return [$numbers, $boards];
}

function checkWinner(array $marked): bool {
    // Check rows
    for ($row = 0; $row < 5; $row++) {
        $rowWin = true;
        for ($col = 0; $col < 5; $col++) {
            if (!$marked[$row][$col]) {
                $rowWin = false;
                break;
            }
        }
        if ($rowWin) return true;
    }

    // Check columns
    for ($col = 0; $col < 5; $col++) {
        $colWin = true;
        for ($row = 0; $row < 5; $row++) {
            if (!$marked[$row][$col]) {
                $colWin = false;
                break;
            }
        }
        if ($colWin) return true;
    }

    return false;
}

function calculateScore(array $board, array $marked, int $lastNumber): int {
    $unmarkedSum = 0;
    for ($row = 0; $row < 5; $row++) {
        for ($col = 0; $col < 5; $col++) {
            if (!$marked[$row][$col]) {
                $unmarkedSum += $board[$row][$col];
            }
        }
    }
    return $unmarkedSum * $lastNumber;
}

function markNumber(array $board, array &$marked, int $number): void {
    for ($row = 0; $row < 5; $row++) {
        for ($col = 0; $col < 5; $col++) {
            if ($board[$row][$col] === $number) {
                $marked[$row][$col] = true;
            }
        }
    }
}

function part1(array $numbers, array $boards): int {
    $marked = [];
    foreach ($boards as $i => $board) {
        $marked[$i] = array_fill(0, 5, array_fill(0, 5, false));
    }

    foreach ($numbers as $number) {
        foreach ($boards as $i => $board) {
            markNumber($board, $marked[$i], $number);
            if (checkWinner($marked[$i])) {
                return calculateScore($board, $marked[$i], $number);
            }
        }
    }

    return 0;
}

function part2(array $numbers, array $boards): int {
    $marked = [];
    $won = [];
    foreach ($boards as $i => $board) {
        $marked[$i] = array_fill(0, 5, array_fill(0, 5, false));
        $won[$i] = false;
    }

    $lastScore = 0;

    foreach ($numbers as $number) {
        foreach ($boards as $i => $board) {
            if ($won[$i]) {
                continue;
            }
            markNumber($board, $marked[$i], $number);
            if (checkWinner($marked[$i])) {
                $won[$i] = true;
                $lastScore = calculateScore($board, $marked[$i], $number);
            }
        }
    }

    return $lastScore;
}

[$numbers, $boards] = parseInput();
echo "Part 1: " . part1($numbers, $boards) . "\n";
echo "Part 2: " . part2($numbers, $boards) . "\n";
