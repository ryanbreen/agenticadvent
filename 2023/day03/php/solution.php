<?php

// Read input
$input = file_get_contents(__DIR__ . '/../input.txt');
$lines = explode("\n", trim($input));

function part1($lines) {
    $height = count($lines);
    $width = strlen($lines[0]);
    $partNumberSum = 0;

    // For each line, find all numbers
    for ($row = 0; $row < $height; $row++) {
        preg_match_all('/\d+/', $lines[$row], $matches, PREG_OFFSET_CAPTURE);

        foreach ($matches[0] as $match) {
            $number = (int)$match[0];
            $startCol = $match[1];
            $endCol = $startCol + strlen($match[0]) - 1;

            // Check if this number is adjacent to any symbol
            $isPartNumber = false;

            // Check all positions around the number
            for ($col = $startCol; $col <= $endCol; $col++) {
                // Check 8 directions around this digit
                $directions = [
                    [-1, -1], [-1, 0], [-1, 1],
                    [0, -1],           [0, 1],
                    [1, -1],  [1, 0],  [1, 1]
                ];

                foreach ($directions as $dir) {
                    $newRow = $row + $dir[0];
                    $newCol = $col + $dir[1];

                    if ($newRow >= 0 && $newRow < $height &&
                        $newCol >= 0 && $newCol < $width) {
                        $char = $lines[$newRow][$newCol];

                        // A symbol is anything that's not a digit and not a period
                        if ($char !== '.' && !ctype_digit($char)) {
                            $isPartNumber = true;
                            break 2; // Break out of both loops
                        }
                    }
                }
            }

            if ($isPartNumber) {
                $partNumberSum += $number;
            }
        }
    }

    return $partNumberSum;
}

function part2($lines) {
    $height = count($lines);
    $width = strlen($lines[0]);

    // Track which numbers are adjacent to each gear (*)
    // Key: "row,col" of the gear, Value: array of adjacent numbers
    $gearNumbers = [];

    // For each line, find all numbers
    for ($row = 0; $row < $height; $row++) {
        preg_match_all('/\d+/', $lines[$row], $matches, PREG_OFFSET_CAPTURE);

        foreach ($matches[0] as $match) {
            $number = (int)$match[0];
            $startCol = $match[1];
            $endCol = $startCol + strlen($match[0]) - 1;

            // Track which gears this number is adjacent to (using a set to avoid duplicates)
            $adjacentGears = [];

            // Check all positions around the number
            for ($col = $startCol; $col <= $endCol; $col++) {
                // Check 8 directions around this digit
                $directions = [
                    [-1, -1], [-1, 0], [-1, 1],
                    [0, -1],           [0, 1],
                    [1, -1],  [1, 0],  [1, 1]
                ];

                foreach ($directions as $dir) {
                    $newRow = $row + $dir[0];
                    $newCol = $col + $dir[1];

                    if ($newRow >= 0 && $newRow < $height &&
                        $newCol >= 0 && $newCol < $width) {
                        $char = $lines[$newRow][$newCol];

                        // Check if it's a gear (*)
                        if ($char === '*') {
                            $gearKey = "$newRow,$newCol";
                            $adjacentGears[$gearKey] = true;
                        }
                    }
                }
            }

            // Add this number to all adjacent gears
            foreach (array_keys($adjacentGears) as $gearKey) {
                if (!isset($gearNumbers[$gearKey])) {
                    $gearNumbers[$gearKey] = [];
                }
                $gearNumbers[$gearKey][] = $number;
            }
        }
    }

    // Calculate gear ratios for gears with exactly 2 adjacent numbers
    $gearRatioSum = 0;
    foreach ($gearNumbers as $numbers) {
        if (count($numbers) === 2) {
            $gearRatioSum += $numbers[0] * $numbers[1];
        }
    }

    return $gearRatioSum;
}

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
