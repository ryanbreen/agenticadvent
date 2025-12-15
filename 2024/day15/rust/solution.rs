use std::collections::HashSet;
use std::fs;

fn parse_input(text: &str) -> (Vec<Vec<char>>, Vec<char>) {
    let parts: Vec<&str> = text.split("\n\n").collect();
    let grid: Vec<Vec<char>> = parts[0].lines().map(|line| line.chars().collect()).collect();
    let moves: Vec<char> = parts[1].chars().filter(|&c| c != '\n').collect();
    (grid, moves)
}

fn find_robot(grid: &[Vec<char>]) -> Option<(usize, usize)> {
    for (r, row) in grid.iter().enumerate() {
        for (c, &cell) in row.iter().enumerate() {
            if cell == '@' {
                return Some((r, c));
            }
        }
    }
    None
}

fn move_robot(grid: &mut [Vec<char>], robot_pos: (usize, usize), direction: char) -> (usize, usize) {
    let (dr, dc) = match direction {
        '<' => (0, -1),
        '>' => (0, 1),
        '^' => (-1, 0),
        'v' => (1, 0),
        _ => return robot_pos,
    };

    let r = robot_pos.0 as isize;
    let c = robot_pos.1 as isize;
    let nr = (r + dr) as usize;
    let nc = (c + dc) as usize;

    if grid[nr][nc] == '#' {
        return robot_pos;
    }

    if grid[nr][nc] == '.' {
        grid[robot_pos.0][robot_pos.1] = '.';
        grid[nr][nc] = '@';
        return (nr, nc);
    }

    if grid[nr][nc] == 'O' {
        let mut check_r = nr as isize;
        let mut check_c = nc as isize;

        while grid[check_r as usize][check_c as usize] == 'O' {
            check_r += dr;
            check_c += dc;
        }

        if grid[check_r as usize][check_c as usize] == '#' {
            return robot_pos;
        }

        grid[check_r as usize][check_c as usize] = 'O';
        grid[robot_pos.0][robot_pos.1] = '.';
        grid[nr][nc] = '@';
        return (nr, nc);
    }

    robot_pos
}

fn calculate_gps(grid: &[Vec<char>], box_char: char) -> usize {
    let mut total = 0;
    for (r, row) in grid.iter().enumerate() {
        for (c, &cell) in row.iter().enumerate() {
            if cell == box_char {
                total += 100 * r + c;
            }
        }
    }
    total
}

fn part1(text: &str) -> usize {
    let (mut grid, moves) = parse_input(text);
    let mut robot_pos = find_robot(&grid).unwrap();

    for direction in moves {
        robot_pos = move_robot(&mut grid, robot_pos, direction);
    }

    calculate_gps(&grid, 'O')
}

fn scale_grid(grid: &[Vec<char>]) -> Vec<Vec<char>> {
    let mut new_grid = Vec::new();
    for row in grid {
        let mut new_row = Vec::new();
        for &cell in row {
            match cell {
                '#' => {
                    new_row.push('#');
                    new_row.push('#');
                }
                'O' => {
                    new_row.push('[');
                    new_row.push(']');
                }
                '.' => {
                    new_row.push('.');
                    new_row.push('.');
                }
                '@' => {
                    new_row.push('@');
                    new_row.push('.');
                }
                _ => {}
            }
        }
        new_grid.push(new_row);
    }
    new_grid
}

fn can_move_box_vertical(
    grid: &[Vec<char>],
    box_left_c: usize,
    r: usize,
    dr: isize,
) -> bool {
    let nr = (r as isize + dr) as usize;
    let left_c = box_left_c;
    let right_c = box_left_c + 1;

    let left_target = grid[nr][left_c];
    let right_target = grid[nr][right_c];

    if left_target == '#' || right_target == '#' {
        return false;
    }

    let mut boxes_to_check = HashSet::new();

    if left_target == '[' {
        boxes_to_check.insert((nr, left_c));
    } else if left_target == ']' {
        boxes_to_check.insert((nr, left_c - 1));
    }

    if right_target == '[' {
        boxes_to_check.insert((nr, right_c));
    } else if right_target == ']' {
        boxes_to_check.insert((nr, right_c - 1));
    }

    for (box_r, box_c) in boxes_to_check {
        if !can_move_box_vertical(grid, box_c, box_r, dr) {
            return false;
        }
    }

    true
}

fn collect_boxes_vertical(
    grid: &[Vec<char>],
    box_left_c: usize,
    r: usize,
    dr: isize,
    collected: &mut HashSet<(usize, usize)>,
) {
    collected.insert((r, box_left_c));
    let nr = (r as isize + dr) as usize;
    let left_c = box_left_c;
    let right_c = box_left_c + 1;

    let left_target = grid[nr][left_c];
    let right_target = grid[nr][right_c];

    let mut boxes_to_check = HashSet::new();

    if left_target == '[' {
        boxes_to_check.insert((nr, left_c));
    } else if left_target == ']' {
        boxes_to_check.insert((nr, left_c - 1));
    }

    if right_target == '[' {
        boxes_to_check.insert((nr, right_c));
    } else if right_target == ']' {
        boxes_to_check.insert((nr, right_c - 1));
    }

    for (box_r, box_c) in boxes_to_check {
        if !collected.contains(&(box_r, box_c)) {
            collect_boxes_vertical(grid, box_c, box_r, dr, collected);
        }
    }
}

fn move_robot_wide(
    grid: &mut [Vec<char>],
    robot_pos: (usize, usize),
    direction: char,
) -> (usize, usize) {
    let (dr, dc) = match direction {
        '<' => (0, -1),
        '>' => (0, 1),
        '^' => (-1, 0),
        'v' => (1, 0),
        _ => return robot_pos,
    };

    let r = robot_pos.0 as isize;
    let c = robot_pos.1 as isize;
    let nr = (r + dr) as usize;
    let nc = (c + dc) as usize;

    let target = grid[nr][nc];

    if target == '#' {
        return robot_pos;
    }

    if target == '.' {
        grid[robot_pos.0][robot_pos.1] = '.';
        grid[nr][nc] = '@';
        return (nr, nc);
    }

    if target == '[' || target == ']' {
        if dc != 0 {
            // Horizontal movement
            let mut check_c = nc as isize;
            while grid[r as usize][check_c as usize] == '[' || grid[r as usize][check_c as usize] == ']' {
                check_c += dc;
            }

            if grid[r as usize][check_c as usize] == '#' {
                return robot_pos;
            }

            // Shift all boxes
            if dc > 0 {
                // Moving right
                let mut col = check_c as usize;
                while col > nc {
                    grid[r as usize][col] = grid[r as usize][col - 1];
                    col -= 1;
                }
            } else {
                // Moving left
                let mut col = check_c as usize;
                while col < nc {
                    grid[r as usize][col] = grid[r as usize][col + 1];
                    col += 1;
                }
            }

            grid[robot_pos.0][robot_pos.1] = '.';
            grid[nr][nc] = '@';
            return (nr, nc);
        } else {
            // Vertical movement
            let box_left_c = if target == '[' { nc } else { nc - 1 };

            if !can_move_box_vertical(grid, box_left_c, nr, dr) {
                return robot_pos;
            }

            let mut boxes_to_move = HashSet::new();
            collect_boxes_vertical(grid, box_left_c, nr, dr, &mut boxes_to_move);

            let mut sorted_boxes: Vec<(usize, usize)> = boxes_to_move.into_iter().collect();
            sorted_boxes.sort_by(|a, b| {
                if dr > 0 {
                    b.0.cmp(&a.0)
                } else {
                    a.0.cmp(&b.0)
                }
            });

            for (box_r, box_c) in sorted_boxes {
                grid[box_r][box_c] = '.';
                grid[box_r][box_c + 1] = '.';
                grid[(box_r as isize + dr) as usize][box_c] = '[';
                grid[(box_r as isize + dr) as usize][box_c + 1] = ']';
            }

            grid[robot_pos.0][robot_pos.1] = '.';
            grid[nr][nc] = '@';
            return (nr, nc);
        }
    }

    robot_pos
}

fn part2(text: &str) -> usize {
    let (grid, moves) = parse_input(text);
    let mut grid = scale_grid(&grid);
    let mut robot_pos = find_robot(&grid).unwrap();

    for direction in moves {
        robot_pos = move_robot_wide(&mut grid, robot_pos, direction);
    }

    calculate_gps(&grid, '[')
}

fn main() {
    let input_text = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");
    let input_text = input_text.trim();

    println!("Part 1: {}", part1(input_text));
    println!("Part 2: {}", part2(input_text));
}
