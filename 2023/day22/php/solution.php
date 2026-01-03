<?php
/**
 * Day 22: Sand Slabs - 3D falling bricks simulation.
 */

/**
 * Parse brick coordinates from input.
 */
function parseInput(string $filename): array {
    $bricks = [];
    $lines = explode("\n", trim(file_get_contents($filename)));

    foreach ($lines as $line) {
        $parts = explode('~', $line);
        list($x1, $y1, $z1) = array_map('intval', explode(',', $parts[0]));
        list($x2, $y2, $z2) = array_map('intval', explode(',', $parts[1]));

        // Ensure z1 <= z2 for consistent processing
        if ($z1 > $z2) {
            list($x1, $y1, $z1, $x2, $y2, $z2) = [$x2, $y2, $z2, $x1, $y1, $z1];
        }

        $bricks[] = [$x1, $y1, $z1, $x2, $y2, $z2];
    }

    return $bricks;
}

/**
 * Simulate bricks falling and settling.
 * Returns settled bricks and support relationships.
 */
function settleBricks(array $bricks): array {
    // Create indexed array for sorting
    $indexed = [];
    foreach ($bricks as $idx => $brick) {
        $indexed[] = [$idx, $brick];
    }

    // Sort by minimum z coordinate
    usort($indexed, function($a, $b) {
        return min($a[1][2], $a[1][5]) <=> min($b[1][2], $b[1][5]);
    });

    // Track occupied cells: "x,y,z" => brick index
    $occupied = [];
    $settled = array_fill(0, count($bricks), null);

    // supports[i] = set of brick indices that brick i supports (bricks above)
    // supporters[i] = set of brick indices that support brick i (bricks below)
    $supports = [];
    $supporters = [];

    foreach ($indexed as list($origIdx, $brick)) {
        list($x1, $y1, $z1, $x2, $y2, $z2) = $brick;

        // Find the lowest z where this brick can rest
        $drop = $z1 - 1; // Maximum drop (to z=1)

        // Get xy footprint of this brick
        for ($x = min($x1, $x2); $x <= max($x1, $x2); $x++) {
            for ($y = min($y1, $y2); $y <= max($y1, $y2); $y++) {
                // Check each z level below the brick
                for ($z = $z1 - 1; $z >= 1; $z--) {
                    $key = "$x,$y,$z";
                    if (isset($occupied[$key])) {
                        $drop = min($drop, $z1 - $z - 1);
                        break;
                    }
                }
            }
        }

        // Drop the brick
        $newZ1 = $z1 - $drop;
        $newZ2 = $z2 - $drop;
        $newBrick = [$x1, $y1, $newZ1, $x2, $y2, $newZ2];
        $settled[$origIdx] = $newBrick;

        // Initialize sets if not exist
        if (!isset($supports[$origIdx])) {
            $supports[$origIdx] = [];
        }
        if (!isset($supporters[$origIdx])) {
            $supporters[$origIdx] = [];
        }

        // Mark cells as occupied and find supporters
        for ($x = min($x1, $x2); $x <= max($x1, $x2); $x++) {
            for ($y = min($y1, $y2); $y <= max($y1, $y2); $y++) {
                // Check if there's a brick directly below
                $belowKey = "$x,$y," . ($newZ1 - 1);
                if (isset($occupied[$belowKey])) {
                    $supporterIdx = $occupied[$belowKey];
                    $supporters[$origIdx][$supporterIdx] = true;
                    if (!isset($supports[$supporterIdx])) {
                        $supports[$supporterIdx] = [];
                    }
                    $supports[$supporterIdx][$origIdx] = true;
                }

                // Mark all cells of this brick as occupied
                for ($z = $newZ1; $z <= $newZ2; $z++) {
                    $occupied["$x,$y,$z"] = $origIdx;
                }
            }
        }
    }

    return [$settled, $supports, $supporters];
}

/**
 * Part 1: Count bricks that can be safely disintegrated.
 */
function part1(array $bricks): int {
    list($settled, $supports, $supporters) = settleBricks($bricks);

    $safeCount = 0;
    for ($i = 0; $i < count($bricks); $i++) {
        // Brick i can be safely removed if every brick it supports
        // has at least one other supporter
        $canRemove = true;
        $supportedBricks = $supports[$i] ?? [];

        foreach (array_keys($supportedBricks) as $supported) {
            if (count($supporters[$supported] ?? []) === 1) {
                $canRemove = false;
                break;
            }
        }

        if ($canRemove) {
            $safeCount++;
        }
    }

    return $safeCount;
}

/**
 * Part 2: Count total bricks that would fall for each disintegration.
 */
function part2(array $bricks): int {
    list($settled, $supports, $supporters) = settleBricks($bricks);

    $totalFalls = 0;

    for ($i = 0; $i < count($bricks); $i++) {
        // Simulate removing brick i and count chain reaction
        // BFS to find all bricks that would fall
        $falling = [$i => true];
        $queue = [$i];
        $front = 0;

        while ($front < count($queue)) {
            $brick = $queue[$front++];

            // Check all bricks that this brick supports
            $supportedBricks = $supports[$brick] ?? [];
            foreach (array_keys($supportedBricks) as $supported) {
                if (isset($falling[$supported])) {
                    continue;
                }

                // This brick falls if all its supporters have fallen
                $allSupportersFallen = true;
                foreach (array_keys($supporters[$supported] ?? []) as $supporter) {
                    if (!isset($falling[$supporter])) {
                        $allSupportersFallen = false;
                        break;
                    }
                }

                if ($allSupportersFallen) {
                    $falling[$supported] = true;
                    $queue[] = $supported;
                }
            }
        }

        // Don't count the initial brick we removed
        $totalFalls += count($falling) - 1;
    }

    return $totalFalls;
}

// Main execution
$inputPath = __DIR__ . '/../input.txt';
$bricks = parseInput($inputPath);

echo "Part 1: " . part1($bricks) . "\n";
echo "Part 2: " . part2($bricks) . "\n";
