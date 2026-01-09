<?php
/**
 * Advent of Code 2022 - Day 22: Monkey Map
 *
 * Part 1: Navigate 2D grid with flat edge wrapping
 * Part 2: Navigate treating map as cube faces with cube wrapping
 */

function parseInput(string $text): array {
    $parts = explode("\n\n", $text);
    $gridLines = explode("\n", $parts[0]);
    $path = trim($parts[1]);

    // Find dimensions
    $height = count($gridLines);
    $width = max(array_map('strlen', $gridLines));

    // Pad lines to consistent width
    $grid = [];
    foreach ($gridLines as $line) {
        $grid[] = str_pad($line, $width, ' ');
    }

    // Parse path into moves and turns
    $instructions = [];
    $i = 0;
    $len = strlen($path);
    while ($i < $len) {
        if (ctype_digit($path[$i])) {
            $j = $i;
            while ($j < $len && ctype_digit($path[$j])) {
                $j++;
            }
            $instructions[] = (int)substr($path, $i, $j - $i);
            $i = $j;
        } else {
            $instructions[] = $path[$i];
            $i++;
        }
    }

    return [$grid, $instructions];
}

function part1(string $text): int {
    [$grid, $instructions] = parseInput($text);
    $height = count($grid);
    $width = strlen($grid[0]);

    // Directions: 0=right, 1=down, 2=left, 3=up
    $DR = [0, 1, 0, -1];
    $DC = [1, 0, -1, 0];

    // Find starting position (leftmost open tile on top row)
    $row = 0;
    $col = strpos($grid[0], '.');
    $facing = 0; // Start facing right

    foreach ($instructions as $instr) {
        if (is_int($instr)) {
            // Move forward
            for ($step = 0; $step < $instr; $step++) {
                $dr = $DR[$facing];
                $dc = $DC[$facing];
                $nr = $row + $dr;
                $nc = $col + $dc;

                // Wrap around if needed
                if ($facing === 0) { // Right
                    if ($nc >= $width || $grid[$nr][$nc] === ' ') {
                        $nc = 0;
                        while ($grid[$nr][$nc] === ' ') {
                            $nc++;
                        }
                    }
                } elseif ($facing === 2) { // Left
                    if ($nc < 0 || $grid[$nr][$nc] === ' ') {
                        $nc = $width - 1;
                        while ($grid[$nr][$nc] === ' ') {
                            $nc--;
                        }
                    }
                } elseif ($facing === 1) { // Down
                    if ($nr >= $height || $grid[$nr][$nc] === ' ') {
                        $nr = 0;
                        while ($grid[$nr][$nc] === ' ') {
                            $nr++;
                        }
                    }
                } elseif ($facing === 3) { // Up
                    if ($nr < 0 || $grid[$nr][$nc] === ' ') {
                        $nr = $height - 1;
                        while ($grid[$nr][$nc] === ' ') {
                            $nr--;
                        }
                    }
                }

                // Check if we hit a wall
                if ($grid[$nr][$nc] === '#') {
                    break;
                }

                // Move to new position
                $row = $nr;
                $col = $nc;
            }
        } else {
            // Turn
            if ($instr === 'R') {
                $facing = ($facing + 1) % 4;
            } else {
                $facing = ($facing + 3) % 4; // -1 mod 4 = 3
            }
        }
    }

    // Calculate password: 1000*row + 4*col + facing (1-indexed)
    return 1000 * ($row + 1) + 4 * ($col + 1) + $facing;
}

function getCubeFaceAndLocal(int $row, int $col, int $faceSize): array {
    /**
     * Determine which face and local coordinates based on the specific cube layout:
     *   12
     *   3
     *  45
     *  6
     */
    $faceRow = intdiv($row, $faceSize);
    $faceCol = intdiv($col, $faceSize);
    $localR = $row % $faceSize;
    $localC = $col % $faceSize;

    $face = -1;
    if ($faceRow === 0 && $faceCol === 1) {
        $face = 1;
    } elseif ($faceRow === 0 && $faceCol === 2) {
        $face = 2;
    } elseif ($faceRow === 1 && $faceCol === 1) {
        $face = 3;
    } elseif ($faceRow === 2 && $faceCol === 0) {
        $face = 4;
    } elseif ($faceRow === 2 && $faceCol === 1) {
        $face = 5;
    } elseif ($faceRow === 3 && $faceCol === 0) {
        $face = 6;
    }

    return [$face, $localR, $localC];
}

function wrapCube(int $row, int $col, int $facing, int $faceSize): array {
    /**
     * Handle cube wrapping for the actual input layout:
     *    12
     *    3
     *   45
     *   6
     * Returns [new_row, new_col, new_facing]
     */
    $S = $faceSize;
    [$face, $lr, $lc] = getCubeFaceAndLocal($row, $col, $S);

    if ($face === 1) {
        if ($facing === 3) { // Up: goes to face 6, from left, facing right
            return [3*$S + $lc, 0, 0];
        } elseif ($facing === 2) { // Left: goes to face 4, from left, facing right (inverted)
            return [3*$S - 1 - $lr, 0, 0];
        }
    } elseif ($face === 2) {
        if ($facing === 0) { // Right: goes to face 5, from right, facing left (inverted)
            return [3*$S - 1 - $lr, 2*$S - 1, 2];
        } elseif ($facing === 1) { // Down: goes to face 3, from right, facing left
            return [$S + $lc, 2*$S - 1, 2];
        } elseif ($facing === 3) { // Up: goes to face 6, from bottom, facing up
            return [4*$S - 1, $lc, 3];
        }
    } elseif ($face === 3) {
        if ($facing === 0) { // Right: goes to face 2, from bottom, facing up
            return [$S - 1, 2*$S + $lr, 3];
        } elseif ($facing === 2) { // Left: goes to face 4, from top, facing down
            return [2*$S, $lr, 1];
        }
    } elseif ($face === 4) {
        if ($facing === 3) { // Up: goes to face 3, from left, facing right
            return [$S + $lc, $S, 0];
        } elseif ($facing === 2) { // Left: goes to face 1, from left, facing right (inverted)
            return [$S - 1 - $lr, $S, 0];
        }
    } elseif ($face === 5) {
        if ($facing === 0) { // Right: goes to face 2, from right, facing left (inverted)
            return [$S - 1 - $lr, 3*$S - 1, 2];
        } elseif ($facing === 1) { // Down: goes to face 6, from right, facing left
            return [3*$S + $lc, $S - 1, 2];
        }
    } elseif ($face === 6) {
        if ($facing === 0) { // Right: goes to face 5, from bottom, facing up
            return [3*$S - 1, $S + $lr, 3];
        } elseif ($facing === 1) { // Down: goes to face 2, from top, facing down
            return [0, 2*$S + $lc, 1];
        } elseif ($facing === 2) { // Left: goes to face 1, from top, facing down
            return [0, $S + $lr, 1];
        }
    }

    // Shouldn't reach here
    return [$row, $col, $facing];
}

function part2(string $text): int {
    [$grid, $instructions] = parseInput($text);
    $height = count($grid);
    $width = strlen($grid[0]);

    // Determine face size (50 for actual input, 4 for example)
    $faceSize = $height > 50 ? 50 : 4;

    // Directions: 0=right, 1=down, 2=left, 3=up
    $DR = [0, 1, 0, -1];
    $DC = [1, 0, -1, 0];

    // Find starting position
    $row = 0;
    $col = strpos($grid[0], '.');
    $facing = 0;

    foreach ($instructions as $instr) {
        if (is_int($instr)) {
            for ($step = 0; $step < $instr; $step++) {
                $dr = $DR[$facing];
                $dc = $DC[$facing];
                $nr = $row + $dr;
                $nc = $col + $dc;
                $nf = $facing;

                // Check if we need to wrap
                $needWrap = false;
                if ($nr < 0 || $nr >= $height || $nc < 0 || $nc >= $width) {
                    $needWrap = true;
                } elseif ($grid[$nr][$nc] === ' ') {
                    $needWrap = true;
                }

                if ($needWrap) {
                    [$nr, $nc, $nf] = wrapCube($row, $col, $facing, $faceSize);
                }

                // Check for wall
                if ($grid[$nr][$nc] === '#') {
                    break;
                }

                $row = $nr;
                $col = $nc;
                $facing = $nf;
            }
        } else {
            if ($instr === 'R') {
                $facing = ($facing + 1) % 4;
            } else {
                $facing = ($facing + 3) % 4;
            }
        }
    }

    return 1000 * ($row + 1) + 4 * ($col + 1) + $facing;
}

function main(): void {
    $scriptDir = dirname(__FILE__);
    $inputFile = $scriptDir . '/../input.txt';

    $text = file_get_contents($inputFile);

    echo "Part 1: " . part1($text) . "\n";
    echo "Part 2: " . part2($text) . "\n";
}

main();
