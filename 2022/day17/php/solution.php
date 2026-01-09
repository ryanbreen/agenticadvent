#!/usr/bin/env php
<?php

// Rock shapes as list of [dx, dy] offsets from bottom-left
$ROCKS = [
    [[0, 0], [1, 0], [2, 0], [3, 0]],           // Horizontal line
    [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]],   // Plus
    [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],   // L shape
    [[0, 0], [0, 1], [0, 2], [0, 3]],           // Vertical line
    [[0, 0], [1, 0], [0, 1], [1, 1]]            // Square
];

define('WIDTH', 7);

function simulate(string $jets, int $numRocks): string {
    global $ROCKS;

    $occupied = [];
    $height = 0;
    $jetIdx = 0;
    $jetLen = strlen($jets);

    // For cycle detection
    $states = [];
    $heights = [];

    for ($rockNum = 0; $rockNum < $numRocks; $rockNum++) {
        $rockType = $rockNum % 5;
        $rock = $ROCKS[$rockType];

        // Starting position: left edge at x=2, bottom at y=height+3
        $x = 2;
        $y = $height + 3;

        while (true) {
            // Jet push
            $jet = $jets[$jetIdx];
            $jetIdx = ($jetIdx + 1) % $jetLen;

            $dx = ($jet === '>') ? 1 : -1;

            // Check if can move horizontally
            $canMove = true;
            foreach ($rock as $offset) {
                $nx = $x + $offset[0] + $dx;
                $ny = $y + $offset[1];
                if ($nx < 0 || $nx >= WIDTH || isset($occupied["$nx,$ny"])) {
                    $canMove = false;
                    break;
                }
            }

            if ($canMove) {
                $x += $dx;
            }

            // Fall down
            $canFall = true;
            foreach ($rock as $offset) {
                $nx = $x + $offset[0];
                $ny = $y + $offset[1] - 1;
                if ($ny < 0 || isset($occupied["$nx,$ny"])) {
                    $canFall = false;
                    break;
                }
            }

            if ($canFall) {
                $y -= 1;
            } else {
                // Rock stops
                foreach ($rock as $offset) {
                    $fx = $x + $offset[0];
                    $fy = $y + $offset[1];
                    $occupied["$fx,$fy"] = true;
                    if ($fy + 1 > $height) {
                        $height = $fy + 1;
                    }
                }
                break;
            }
        }

        $heights[] = $height;

        // Cycle detection for Part 2
        if ($numRocks > 10000) {
            // Create state key from surface profile
            $profileDepth = 30;
            $profile = [];
            for ($col = 0; $col < WIDTH; $col++) {
                $found = false;
                for ($row = 0; $row < $profileDepth; $row++) {
                    $checkY = $height - 1 - $row;
                    if (isset($occupied["$col,$checkY"])) {
                        $profile[] = [$col, $row];
                        $found = true;
                        break;
                    }
                }
                if (!$found) {
                    $profile[] = [$col, $profileDepth];
                }
            }

            $stateKey = $rockType . ',' . $jetIdx . ',' . json_encode($profile);

            if (isset($states[$stateKey])) {
                // Found cycle
                $cycleStart = $states[$stateKey];
                $cycleLen = $rockNum - $cycleStart;
                $cycleHeight = $height - $heights[$cycleStart];

                // Calculate final height using bcmath for large numbers
                $remaining = bcsub((string)$numRocks, (string)($rockNum + 1));
                $fullCycles = bcdiv($remaining, (string)$cycleLen, 0);
                $leftover = bcmod($remaining, (string)$cycleLen);

                $finalHeight = bcadd((string)$height, bcmul($fullCycles, (string)$cycleHeight));

                if (bccomp($leftover, '0') > 0) {
                    $leftoverInt = (int)$leftover;
                    $additionalHeight = $heights[$cycleStart + $leftoverInt] - $heights[$cycleStart];
                    $finalHeight = bcadd($finalHeight, (string)$additionalHeight);
                }

                return $finalHeight;
            }

            $states[$stateKey] = $rockNum;
        }
    }

    return (string)$height;
}

function part1(string $jets): string {
    return simulate($jets, 2022);
}

function part2(string $jets): string {
    return simulate($jets, 1000000000000);
}

function main(): void {
    $inputFile = __DIR__ . '/../input.txt';
    $jets = trim(file_get_contents($inputFile));

    echo 'Part 1: ' . part1($jets) . "\n";
    echo 'Part 2: ' . part2($jets) . "\n";
}

main();
