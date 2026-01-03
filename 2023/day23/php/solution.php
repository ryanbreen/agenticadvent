<?php
/**
 * Day 23: A Long Walk - Longest path through hiking trails.
 */

/**
 * Parse the grid from input file.
 */
function parseInput(string $filename): array {
    $content = trim(file_get_contents($filename));
    return explode("\n", $content);
}

/**
 * Find all junction points (start, end, and intersections with 3+ neighbors).
 */
function findJunctions(array $grid): array {
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $junctions = [];

    // Start point
    $startCol = strpos($grid[0], '.');
    $junctions["0,$startCol"] = [0, $startCol];

    // End point
    $endCol = strpos($grid[$rows - 1], '.');
    $junctions[($rows - 1) . ",$endCol"] = [$rows - 1, $endCol];

    // Find intersections (cells with 3+ walkable neighbors)
    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($grid[$r][$c] === '#') {
                continue;
            }

            $neighbors = 0;
            foreach ($directions as [$dr, $dc]) {
                $nr = $r + $dr;
                $nc = $c + $dc;
                if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && $grid[$nr][$nc] !== '#') {
                    $neighbors++;
                }
            }

            if ($neighbors >= 3) {
                $junctions["$r,$c"] = [$r, $c];
            }
        }
    }

    return $junctions;
}

/**
 * Build a graph of junctions with edge weights (distances).
 */
function buildGraph(array $grid, array $junctions, bool $respectSlopes): array {
    $rows = count($grid);
    $cols = strlen($grid[0]);

    // Direction mappings for slopes
    $slopeDirs = [
        '^' => [-1, 0],
        'v' => [1, 0],
        '<' => [0, -1],
        '>' => [0, 1]
    ];

    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    $graph = [];

    foreach ($junctions as $key => $startJunction) {
        $graph[$key] = [];

        // DFS from each junction to find reachable junctions
        $stack = [[$startJunction, 0]];
        $visited = [$key => true];

        while (!empty($stack)) {
            [$pos, $dist] = array_pop($stack);
            [$r, $c] = $pos;
            $posKey = "$r,$c";

            if ($dist > 0 && isset($junctions[$posKey])) {
                // Found another junction
                $graph[$key][$posKey] = $dist;
                continue;
            }

            // Explore neighbors
            foreach ($directions as [$dr, $dc]) {
                $nr = $r + $dr;
                $nc = $c + $dc;

                if ($nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols) {
                    continue;
                }
                if ($grid[$nr][$nc] === '#') {
                    continue;
                }

                $neighborKey = "$nr,$nc";
                if (isset($visited[$neighborKey])) {
                    continue;
                }

                // Check slope constraints for Part 1
                if ($respectSlopes) {
                    $cell = $grid[$r][$c];
                    if (isset($slopeDirs[$cell])) {
                        [$reqDr, $reqDc] = $slopeDirs[$cell];
                        if ($dr !== $reqDr || $dc !== $reqDc) {
                            continue;
                        }
                    }
                }

                $visited[$neighborKey] = true;
                $stack[] = [[$nr, $nc], $dist + 1];
            }
        }
    }

    return $graph;
}

/**
 * Find longest path using DFS with backtracking.
 */
function longestPathDfs(array $graph, string $start, string $end): int {
    $visited = [];

    $dfs = function(string $node) use (&$dfs, &$visited, $graph, $end): int {
        if ($node === $end) {
            return 0;
        }

        $visited[$node] = true;
        $maxDist = PHP_INT_MIN;

        if (isset($graph[$node])) {
            foreach ($graph[$node] as $neighbor => $dist) {
                if (!isset($visited[$neighbor])) {
                    $result = $dfs($neighbor);
                    if ($result !== PHP_INT_MIN) {
                        $maxDist = max($maxDist, $dist + $result);
                    }
                }
            }
        }

        unset($visited[$node]);
        return $maxDist;
    };

    return $dfs($start);
}

/**
 * Solve for either part.
 */
function solve(array $grid, bool $respectSlopes): int {
    $rows = count($grid);
    $startCol = strpos($grid[0], '.');
    $endCol = strpos($grid[$rows - 1], '.');

    $start = "0,$startCol";
    $end = ($rows - 1) . ",$endCol";

    $junctions = findJunctions($grid);
    $graph = buildGraph($grid, $junctions, $respectSlopes);

    return longestPathDfs($graph, $start, $end);
}

/**
 * Part 1: Respect slope directions.
 */
function part1(array $grid): int {
    return solve($grid, true);
}

/**
 * Part 2: Ignore slopes (treat as regular paths).
 */
function part2(array $grid): int {
    return solve($grid, false);
}

// Main execution
$inputPath = dirname(__DIR__) . '/input.txt';
$grid = parseInput($inputPath);

echo "Part 1: " . part1($grid) . "\n";
echo "Part 2: " . part2($grid) . "\n";
