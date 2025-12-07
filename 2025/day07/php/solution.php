<?php

$input = file_get_contents(__DIR__ . '/../input.txt');
$lines = explode("\n", trim($input));

function part1($lines) {
    $rows = count($lines);
    $cols = $rows > 0 ? strlen($lines[0]) : 0;

    // Find starting position S
    $start_col = null;
    for ($col = 0; $col < $cols; $col++) {
        if ($lines[0][$col] === 'S') {
            $start_col = $col;
            break;
        }
    }

    if ($start_col === null) {
        return 0;
    }

    // Track active beam columns at each row
    // Use array as set to handle beam merging
    $active_beams = [$start_col => true];
    $split_count = 0;

    // Process row by row starting from row 1 (below S)
    for ($row = 1; $row < $rows; $row++) {
        $new_beams = [];

        foreach (array_keys($active_beams) as $col) {
            if ($col >= 0 && $col < $cols) {
                $cell = $lines[$row][$col];
                if ($cell === '^') {
                    // Beam hits splitter - count it and emit left/right
                    $split_count++;
                    // Left beam goes to col-1, right beam goes to col+1
                    if ($col - 1 >= 0) {
                        $new_beams[$col - 1] = true;
                    }
                    if ($col + 1 < $cols) {
                        $new_beams[$col + 1] = true;
                    }
                } elseif ($cell === '.') {
                    // Beam continues straight down
                    $new_beams[$col] = true;
                } else {
                    // If cell is something else (like S), beam continues
                    $new_beams[$col] = true;
                }
            }
        }

        $active_beams = $new_beams;

        // If no more beams, stop
        if (empty($active_beams)) {
            break;
        }
    }

    return $split_count;
}

function part2($lines) {
    $rows = count($lines);
    $cols = $rows > 0 ? strlen($lines[0]) : 0;

    // Find starting position S
    $start_col = null;
    for ($col = 0; $col < $cols; $col++) {
        if ($lines[0][$col] === 'S') {
            $start_col = $col;
            break;
        }
    }

    if ($start_col === null) {
        return gmp_init(0);
    }

    // Track number of timelines at each column position
    // Use array: col -> count of timelines at that position (using GMP for large numbers)
    $timelines = [];
    $timelines[$start_col] = gmp_init(1);

    // Process row by row starting from row 1 (below S)
    for ($row = 1; $row < $rows; $row++) {
        $new_timelines = [];

        foreach ($timelines as $col => $count) {
            if ($col >= 0 && $col < $cols) {
                $cell = $lines[$row][$col];
                if ($cell === '^') {
                    // Each timeline splits into 2 (left and right)
                    if ($col - 1 >= 0) {
                        if (!isset($new_timelines[$col - 1])) {
                            $new_timelines[$col - 1] = gmp_init(0);
                        }
                        $new_timelines[$col - 1] = gmp_add($new_timelines[$col - 1], $count);
                    }
                    if ($col + 1 < $cols) {
                        if (!isset($new_timelines[$col + 1])) {
                            $new_timelines[$col + 1] = gmp_init(0);
                        }
                        $new_timelines[$col + 1] = gmp_add($new_timelines[$col + 1], $count);
                    }
                } elseif ($cell === '.') {
                    // Timelines continue straight down
                    if (!isset($new_timelines[$col])) {
                        $new_timelines[$col] = gmp_init(0);
                    }
                    $new_timelines[$col] = gmp_add($new_timelines[$col], $count);
                } else {
                    // Other characters - timelines continue
                    if (!isset($new_timelines[$col])) {
                        $new_timelines[$col] = gmp_init(0);
                    }
                    $new_timelines[$col] = gmp_add($new_timelines[$col], $count);
                }
            }
        }

        $timelines = $new_timelines;

        // If no more timelines, stop
        if (empty($timelines)) {
            break;
        }
    }

    // Total number of timelines
    $total = gmp_init(0);
    foreach ($timelines as $count) {
        $total = gmp_add($total, $count);
    }

    return $total;
}

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . gmp_strval(part2($lines)) . "\n";
