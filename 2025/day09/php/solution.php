<?php

$input = file_get_contents(__DIR__ . '/../input.txt');
$lines = array_filter(array_map('trim', explode("\n", trim($input))));

// Parse input - each line is "x,y"
$points = [];
foreach ($lines as $line) {
    list($x, $y) = array_map('intval', explode(',', $line));
    $points[] = [$x, $y];
}

function part1($points) {
    $max_area = 0;
    $n = count($points);

    // Check all pairs of points as opposite corners
    for ($i = 0; $i < $n; $i++) {
        list($x1, $y1) = $points[$i];
        for ($j = $i + 1; $j < $n; $j++) {
            list($x2, $y2) = $points[$j];
            // Rectangle area = width * height (inclusive of both corners)
            $width = abs($x2 - $x1) + 1;
            $height = abs($y2 - $y1) + 1;
            $area = $width * $height;
            $max_area = max($max_area, $area);
        }
    }

    return $max_area;
}

function part2($points) {
    $n = count($points);
    $horizontal_edges = [];
    $vertical_edges = [];

    // Build edges from consecutive points
    for ($i = 0; $i < $n; $i++) {
        list($x1, $y1) = $points[$i];
        list($x2, $y2) = $points[($i + 1) % $n];

        if ($y1 == $y2) {  // Horizontal edge
            $horizontal_edges[] = [$y1, min($x1, $x2), max($x1, $x2)];
        } else {  // Vertical edge
            $vertical_edges[] = [$x1, min($y1, $y2), max($y1, $y2)];
        }
    }

    // Sort vertical edges by x coordinate
    usort($vertical_edges, function($a, $b) {
        return $a[0] <=> $b[0];
    });

    // Build maps for efficient lookup
    $vert_by_x = [];
    foreach ($vertical_edges as $edge) {
        list($x, $y_min, $y_max) = $edge;
        if (!isset($vert_by_x[$x])) {
            $vert_by_x[$x] = [];
        }
        $vert_by_x[$x][] = [$y_min, $y_max];
    }

    $horiz_by_y = [];
    foreach ($horizontal_edges as $edge) {
        list($y, $x_min, $x_max) = $edge;
        if (!isset($horiz_by_y[$y])) {
            $horiz_by_y[$y] = [];
        }
        $horiz_by_y[$y][] = [$x_min, $x_max];
    }

    // Check if point is inside polygon using ray casting
    $is_inside_polygon = function($x, $y) use ($vert_by_x) {
        $crossings = 0;
        $x_coords = array_keys($vert_by_x);
        sort($x_coords);

        foreach ($x_coords as $vx) {
            if ($vx <= $x) {
                continue;
            }
            foreach ($vert_by_x[$vx] as $edge) {
                list($y_min, $y_max) = $edge;
                if ($y_min < $y && $y < $y_max) {  // Strict inequality
                    $crossings++;
                } elseif ($y == $y_min || $y == $y_max) {
                    // On corner - count as 0.5 crossing
                    $crossings += 0.5;
                }
            }
        }
        return fmod($crossings, 2) == 1;
    };

    // Check if rectangle is entirely inside polygon
    $rectangle_valid = function($x1, $y1, $x2, $y2) use ($vert_by_x, $horiz_by_y, $is_inside_polygon) {
        $min_x = min($x1, $x2);
        $max_x = max($x1, $x2);
        $min_y = min($y1, $y2);
        $max_y = max($y1, $y2);

        // Check if any vertical edge crosses through the rectangle interior
        foreach ($vert_by_x as $vx => $edges) {
            if ($min_x < $vx && $vx < $max_x) {
                foreach ($edges as $edge) {
                    list($y_min, $y_max) = $edge;
                    // Check if this edge segment overlaps with rectangle's y range
                    if (!($y_max <= $min_y || $y_min >= $max_y)) {
                        return false;
                    }
                }
            }
        }

        // Check if any horizontal edge crosses through the rectangle interior
        foreach ($horiz_by_y as $hy => $edges) {
            if ($min_y < $hy && $hy < $max_y) {
                foreach ($edges as $edge) {
                    list($x_min, $x_max) = $edge;
                    // Check if this edge segment overlaps with rectangle's x range
                    if (!($x_max <= $min_x || $x_min >= $max_x)) {
                        return false;
                    }
                }
            }
        }

        // Check that center point is inside the polygon
        $center_x = ($min_x + $max_x) / 2.0;
        $center_y = ($min_y + $max_y) / 2.0;
        return $is_inside_polygon($center_x, $center_y);
    };

    // Find largest valid rectangle with red corners
    $max_area = 0;

    for ($i = 0; $i < count($points); $i++) {
        list($x1, $y1) = $points[$i];
        for ($j = $i + 1; $j < count($points); $j++) {
            list($x2, $y2) = $points[$j];

            if ($rectangle_valid($x1, $y1, $x2, $y2)) {
                $width = abs($x2 - $x1) + 1;
                $height = abs($y2 - $y1) + 1;
                $area = $width * $height;
                $max_area = max($max_area, $area);
            }
        }
    }

    return $max_area;
}

echo "Part 1: " . part1($points) . "\n";
echo "Part 2: " . part2($points) . "\n";
