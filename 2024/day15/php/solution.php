<?php

$input_text = trim(file_get_contents(__DIR__ . '/../input.txt'));

function parse_input($text) {
    $parts = explode("\n\n", $text);
    $grid_lines = explode("\n", $parts[0]);
    $grid = array_map('str_split', $grid_lines);
    $moves = str_replace("\n", '', $parts[1]);
    return [$grid, $moves];
}

function find_robot($grid) {
    foreach ($grid as $r => $row) {
        foreach ($row as $c => $cell) {
            if ($cell === '@') {
                return [$r, $c];
            }
        }
    }
    return null;
}

function move_robot(&$grid, $robot_pos, $direction) {
    $directions = [
        '<' => [0, -1],
        '>' => [0, 1],
        '^' => [-1, 0],
        'v' => [1, 0]
    ];

    [$dr, $dc] = $directions[$direction];
    [$r, $c] = $robot_pos;
    $nr = $r + $dr;
    $nc = $c + $dc;

    if ($grid[$nr][$nc] === '#') {
        return $robot_pos;
    }

    if ($grid[$nr][$nc] === '.') {
        $grid[$r][$c] = '.';
        $grid[$nr][$nc] = '@';
        return [$nr, $nc];
    }

    if ($grid[$nr][$nc] === 'O') {
        $check_r = $nr;
        $check_c = $nc;

        while ($grid[$check_r][$check_c] === 'O') {
            $check_r += $dr;
            $check_c += $dc;
        }

        if ($grid[$check_r][$check_c] === '#') {
            return $robot_pos;
        }

        $grid[$check_r][$check_c] = 'O';
        $grid[$r][$c] = '.';
        $grid[$nr][$nc] = '@';
        return [$nr, $nc];
    }

    return $robot_pos;
}

function calculate_gps($grid, $box_char = 'O') {
    $total = 0;
    foreach ($grid as $r => $row) {
        foreach ($row as $c => $cell) {
            if ($cell === $box_char) {
                $total += 100 * $r + $c;
            }
        }
    }
    return $total;
}

function part1() {
    global $input_text;
    [$grid, $moves] = parse_input($input_text);
    $robot_pos = find_robot($grid);

    for ($i = 0; $i < strlen($moves); $i++) {
        $robot_pos = move_robot($grid, $robot_pos, $moves[$i]);
    }

    return calculate_gps($grid);
}

function scale_grid($grid) {
    $new_grid = [];
    foreach ($grid as $row) {
        $new_row = [];
        foreach ($row as $cell) {
            if ($cell === '#') {
                $new_row[] = '#';
                $new_row[] = '#';
            } elseif ($cell === 'O') {
                $new_row[] = '[';
                $new_row[] = ']';
            } elseif ($cell === '.') {
                $new_row[] = '.';
                $new_row[] = '.';
            } elseif ($cell === '@') {
                $new_row[] = '@';
                $new_row[] = '.';
            }
        }
        $new_grid[] = $new_row;
    }
    return $new_grid;
}

function can_move_box_vertical($grid, $box_left_c, $r, $dr) {
    $nr = $r + $dr;
    $left_c = $box_left_c;
    $right_c = $box_left_c + 1;

    $left_target = $grid[$nr][$left_c];
    $right_target = $grid[$nr][$right_c];

    if ($left_target === '#' || $right_target === '#') {
        return false;
    }

    $boxes_to_check = [];

    if ($left_target === '[') {
        $boxes_to_check["{$nr},{$left_c}"] = [$nr, $left_c];
    } elseif ($left_target === ']') {
        $boxes_to_check["{$nr}," . ($left_c - 1)] = [$nr, $left_c - 1];
    }

    if ($right_target === '[') {
        $boxes_to_check["{$nr},{$right_c}"] = [$nr, $right_c];
    } elseif ($right_target === ']') {
        $boxes_to_check["{$nr}," . ($right_c - 1)] = [$nr, $right_c - 1];
    }

    foreach ($boxes_to_check as $box) {
        [$box_r, $box_c] = $box;
        if (!can_move_box_vertical($grid, $box_c, $box_r, $dr)) {
            return false;
        }
    }

    return true;
}

function collect_boxes_vertical($grid, $box_left_c, $r, $dr, &$collected) {
    $key = "{$r},{$box_left_c}";
    $collected[$key] = [$r, $box_left_c];

    $nr = $r + $dr;
    $left_c = $box_left_c;
    $right_c = $box_left_c + 1;

    $left_target = $grid[$nr][$left_c];
    $right_target = $grid[$nr][$right_c];

    $boxes_to_check = [];

    if ($left_target === '[') {
        $boxes_to_check["{$nr},{$left_c}"] = [$nr, $left_c];
    } elseif ($left_target === ']') {
        $boxes_to_check["{$nr}," . ($left_c - 1)] = [$nr, $left_c - 1];
    }

    if ($right_target === '[') {
        $boxes_to_check["{$nr},{$right_c}"] = [$nr, $right_c];
    } elseif ($right_target === ']') {
        $boxes_to_check["{$nr}," . ($right_c - 1)] = [$nr, $right_c - 1];
    }

    foreach ($boxes_to_check as $key => $box) {
        if (!isset($collected[$key])) {
            [$box_r, $box_c] = $box;
            collect_boxes_vertical($grid, $box_c, $box_r, $dr, $collected);
        }
    }
}

function move_robot_wide(&$grid, $robot_pos, $direction) {
    $directions = [
        '<' => [0, -1],
        '>' => [0, 1],
        '^' => [-1, 0],
        'v' => [1, 0]
    ];

    [$dr, $dc] = $directions[$direction];
    [$r, $c] = $robot_pos;
    $nr = $r + $dr;
    $nc = $c + $dc;

    $target = $grid[$nr][$nc];

    if ($target === '#') {
        return $robot_pos;
    }

    if ($target === '.') {
        $grid[$r][$c] = '.';
        $grid[$nr][$nc] = '@';
        return [$nr, $nc];
    }

    if ($target === '[' || $target === ']') {
        if ($dc !== 0) {
            // Horizontal movement
            $check_c = $nc;
            while ($grid[$r][$check_c] === '[' || $grid[$r][$check_c] === ']') {
                $check_c += $dc;
            }

            if ($grid[$r][$check_c] === '#') {
                return $robot_pos;
            }

            // Shift all boxes
            if ($dc > 0) {
                for ($col = $check_c; $col > $nc; $col--) {
                    $grid[$r][$col] = $grid[$r][$col - 1];
                }
            } else {
                for ($col = $check_c; $col < $nc; $col++) {
                    $grid[$r][$col] = $grid[$r][$col + 1];
                }
            }

            $grid[$r][$c] = '.';
            $grid[$nr][$nc] = '@';
            return [$nr, $nc];
        } else {
            // Vertical movement
            $box_left_c = ($target === '[') ? $nc : $nc - 1;

            if (!can_move_box_vertical($grid, $box_left_c, $nr, $dr)) {
                return $robot_pos;
            }

            $boxes_to_move = [];
            collect_boxes_vertical($grid, $box_left_c, $nr, $dr, $boxes_to_move);

            // Sort boxes by row
            $sorted_boxes = array_values($boxes_to_move);
            usort($sorted_boxes, function($a, $b) use ($dr) {
                if ($dr > 0) {
                    return $b[0] - $a[0];
                } else {
                    return $a[0] - $b[0];
                }
            });

            // Move all boxes
            foreach ($sorted_boxes as $box) {
                [$box_r, $box_c] = $box;
                $grid[$box_r][$box_c] = '.';
                $grid[$box_r][$box_c + 1] = '.';
                $grid[$box_r + $dr][$box_c] = '[';
                $grid[$box_r + $dr][$box_c + 1] = ']';
            }

            // Move robot
            $grid[$r][$c] = '.';
            $grid[$nr][$nc] = '@';
            return [$nr, $nc];
        }
    }

    return $robot_pos;
}

function part2() {
    global $input_text;
    [$grid, $moves] = parse_input($input_text);
    $grid = scale_grid($grid);
    $robot_pos = find_robot($grid);

    for ($i = 0; $i < strlen($moves); $i++) {
        $robot_pos = move_robot_wide($grid, $robot_pos, $moves[$i]);
    }

    return calculate_gps($grid, '[');
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
