<?php

$inputText = file_get_contents(__DIR__ . '/../input.txt');
$inputText = rtrim($inputText);
$lines = explode("\n", $inputText);

/**
 * Parse the worksheet into a list of [numbers, operator] pairs.
 */
function parseProblems($lines) {
    if (empty($lines)) {
        return [];
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    $opRowIdx = count($lines) - 1;
    while ($opRowIdx >= 0) {
        $trimmed = trim($lines[$opRowIdx]);
        if (empty($trimmed)) {
            $opRowIdx--;
            continue;
        }

        $allValidChars = true;
        for ($i = 0; $i < strlen($lines[$opRowIdx]); $i++) {
            $c = $lines[$opRowIdx][$i];
            if ($c !== '+' && $c !== '*' && $c !== ' ') {
                $allValidChars = false;
                break;
            }
        }

        if ($allValidChars) {
            break;
        }
        $opRowIdx--;
    }

    if ($opRowIdx < 0) {
        return [];
    }

    $opRow = $lines[$opRowIdx];
    $numberRows = array_slice($lines, 0, $opRowIdx);

    // Find max width
    $maxWidth = 0;
    foreach ($lines as $line) {
        $maxWidth = max($maxWidth, strlen($line));
    }

    // Pad all rows to the same width
    $paddedNumberRows = [];
    foreach ($numberRows as $row) {
        $paddedNumberRows[] = str_pad($row, $maxWidth);
    }
    $paddedOpRow = str_pad($opRow, $maxWidth);

    // Find problem boundaries by looking for columns that are all spaces
    $problems = [];
    $col = 0;

    while ($col < $maxWidth) {
        // Skip separator columns (all spaces)
        while ($col < $maxWidth) {
            $allSpaces = true;
            foreach ($paddedNumberRows as $row) {
                if ($row[$col] !== ' ') {
                    $allSpaces = false;
                    break;
                }
            }
            if (!$allSpaces || $paddedOpRow[$col] !== ' ') {
                break;
            }
            $col++;
        }

        if ($col >= $maxWidth) {
            break;
        }

        // Find the end of this problem
        $startCol = $col;
        while ($col < $maxWidth) {
            // Check if this is a separator column
            $isSeparator = true;
            foreach ($paddedNumberRows as $row) {
                if ($row[$col] !== ' ') {
                    $isSeparator = false;
                    break;
                }
            }
            if ($isSeparator && $paddedOpRow[$col] === ' ') {
                break;
            }
            $col++;
        }

        $endCol = $col;

        // Extract numbers and operator for this problem
        $numbers = [];
        foreach ($paddedNumberRows as $row) {
            $numStr = trim(substr($row, $startCol, $endCol - $startCol));
            if ($numStr !== '') {
                $numbers[] = $numStr;
            }
        }

        $opStr = trim(substr($paddedOpRow, $startCol, $endCol - $startCol));
        if ($opStr !== '' && !empty($numbers)) {
            $problems[] = [$numbers, $opStr];
        }
    }

    return $problems;
}

/**
 * Solve a single problem given numbers and operator.
 * Using bcmath for big number arithmetic.
 */
function solveProblem($numbers, $op) {
    if ($op === '+') {
        $result = '0';
        foreach ($numbers as $n) {
            $result = bcadd($result, $n);
        }
        return $result;
    } elseif ($op === '*') {
        $result = '1';
        foreach ($numbers as $n) {
            $result = bcmul($result, $n);
        }
        return $result;
    }
    return '0';
}

function part1() {
    global $lines;
    $problems = parseProblems($lines);
    $total = '0';
    foreach ($problems as list($numbers, $op)) {
        $result = solveProblem($numbers, $op);
        $total = bcadd($total, $result);
    }
    return $total;
}

/**
 * Parse the worksheet for part 2 - reading right-to-left columns.
 */
function parseProblemsPart2($lines) {
    if (empty($lines)) {
        return [];
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    $opRowIdx = count($lines) - 1;
    while ($opRowIdx >= 0) {
        $trimmed = trim($lines[$opRowIdx]);
        if (empty($trimmed)) {
            $opRowIdx--;
            continue;
        }

        $allValidChars = true;
        for ($i = 0; $i < strlen($lines[$opRowIdx]); $i++) {
            $c = $lines[$opRowIdx][$i];
            if ($c !== '+' && $c !== '*' && $c !== ' ') {
                $allValidChars = false;
                break;
            }
        }

        if ($allValidChars) {
            break;
        }
        $opRowIdx--;
    }

    if ($opRowIdx < 0) {
        return [];
    }

    $opRow = $lines[$opRowIdx];
    $numberRows = array_slice($lines, 0, $opRowIdx);

    // Find max width
    $maxWidth = 0;
    foreach ($lines as $line) {
        $maxWidth = max($maxWidth, strlen($line));
    }

    // Pad all rows to the same width
    $paddedNumberRows = [];
    foreach ($numberRows as $row) {
        $paddedNumberRows[] = str_pad($row, $maxWidth);
    }
    $paddedOpRow = str_pad($opRow, $maxWidth);

    // Find problem boundaries by looking for columns that are all spaces
    $problems = [];
    $col = 0;

    while ($col < $maxWidth) {
        // Skip separator columns (all spaces)
        while ($col < $maxWidth) {
            $allSpaces = true;
            foreach ($paddedNumberRows as $row) {
                if ($row[$col] !== ' ') {
                    $allSpaces = false;
                    break;
                }
            }
            if (!$allSpaces || $paddedOpRow[$col] !== ' ') {
                break;
            }
            $col++;
        }

        if ($col >= $maxWidth) {
            break;
        }

        // Find the end of this problem
        $startCol = $col;
        while ($col < $maxWidth) {
            // Check if this is a separator column
            $isSeparator = true;
            foreach ($paddedNumberRows as $row) {
                if ($row[$col] !== ' ') {
                    $isSeparator = false;
                    break;
                }
            }
            if ($isSeparator && $paddedOpRow[$col] === ' ') {
                break;
            }
            $col++;
        }

        $endCol = $col;

        // For Part 2: Read columns right-to-left, each column forms a number
        // reading top-to-bottom as most-to-least significant digit
        $numbers = [];
        for ($c = $endCol - 1; $c >= $startCol; $c--) {  // Right to left
            $digits = [];
            foreach ($paddedNumberRows as $row) {
                $ch = $row[$c];
                if (ctype_digit($ch)) {
                    $digits[] = $ch;
                }
            }
            if (!empty($digits)) {
                // Join digits to form number (top=most significant, bottom=least)
                $num = implode('', $digits);
                $numbers[] = $num;
            }
        }

        $opStr = trim(substr($paddedOpRow, $startCol, $endCol - $startCol));
        if ($opStr !== '' && !empty($numbers)) {
            $problems[] = [$numbers, $opStr];
        }
    }

    return $problems;
}

function part2() {
    global $lines;
    $problems = parseProblemsPart2($lines);
    $total = '0';
    foreach ($problems as list($numbers, $op)) {
        $result = solveProblem($numbers, $op);
        $total = bcadd($total, $result);
    }
    return $total;
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
