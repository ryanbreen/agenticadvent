use std::fs;
use std::path::Path;

fn parse_input(text: &str) -> (Vec<Vec<char>>, Vec<Instruction>) {
    let parts: Vec<&str> = text.split("\n\n").collect();
    let grid_lines: Vec<&str> = parts[0].lines().collect();
    let path = parts[1].trim();

    // Find dimensions
    let height = grid_lines.len();
    let width = grid_lines.iter().map(|l| l.len()).max().unwrap_or(0);

    // Create grid (pad lines to consistent width)
    let mut grid: Vec<Vec<char>> = Vec::with_capacity(height);
    for line in &grid_lines {
        let mut row: Vec<char> = line.chars().collect();
        row.resize(width, ' ');
        grid.push(row);
    }

    // Parse path into moves and turns
    let mut instructions = Vec::new();
    let mut i = 0;
    let path_chars: Vec<char> = path.chars().collect();

    while i < path_chars.len() {
        if path_chars[i].is_ascii_digit() {
            let mut j = i;
            while j < path_chars.len() && path_chars[j].is_ascii_digit() {
                j += 1;
            }
            let num: i32 = path_chars[i..j].iter().collect::<String>().parse().unwrap();
            instructions.push(Instruction::Move(num));
            i = j;
        } else {
            instructions.push(Instruction::Turn(path_chars[i]));
            i += 1;
        }
    }

    (grid, instructions)
}

#[derive(Clone, Copy)]
enum Instruction {
    Move(i32),
    Turn(char),
}

fn part1(text: &str) -> i32 {
    let (grid, instructions) = parse_input(text);
    let height = grid.len() as i32;
    let width = grid[0].len() as i32;

    // Directions: 0=right, 1=down, 2=left, 3=up
    let dr = [0, 1, 0, -1];
    let dc = [1, 0, -1, 0];

    // Find starting position (leftmost open tile on top row)
    let mut row: i32 = 0;
    let mut col: i32 = grid[0].iter().position(|&c| c == '.').unwrap() as i32;
    let mut facing: i32 = 0; // Start facing right

    for instr in &instructions {
        match instr {
            Instruction::Move(steps) => {
                for _ in 0..*steps {
                    let mut nr = row + dr[facing as usize];
                    let mut nc = col + dc[facing as usize];

                    // Wrap around if needed
                    match facing {
                        0 => {
                            // Right
                            if nc >= width || grid[nr as usize][nc as usize] == ' ' {
                                nc = 0;
                                while grid[nr as usize][nc as usize] == ' ' {
                                    nc += 1;
                                }
                            }
                        }
                        2 => {
                            // Left
                            if nc < 0 || grid[nr as usize][nc as usize] == ' ' {
                                nc = width - 1;
                                while grid[nr as usize][nc as usize] == ' ' {
                                    nc -= 1;
                                }
                            }
                        }
                        1 => {
                            // Down
                            if nr >= height || grid[nr as usize][nc as usize] == ' ' {
                                nr = 0;
                                while grid[nr as usize][nc as usize] == ' ' {
                                    nr += 1;
                                }
                            }
                        }
                        3 => {
                            // Up
                            if nr < 0 || grid[nr as usize][nc as usize] == ' ' {
                                nr = height - 1;
                                while grid[nr as usize][nc as usize] == ' ' {
                                    nr -= 1;
                                }
                            }
                        }
                        _ => {}
                    }

                    // Check if we hit a wall
                    if grid[nr as usize][nc as usize] == '#' {
                        break;
                    }

                    // Move to new position
                    row = nr;
                    col = nc;
                }
            }
            Instruction::Turn(dir) => {
                if *dir == 'R' {
                    facing = (facing + 1) % 4;
                } else {
                    facing = (facing + 3) % 4;
                }
            }
        }
    }

    // Calculate password: 1000*row + 4*col + facing (1-indexed)
    1000 * (row + 1) + 4 * (col + 1) + facing
}

fn get_cube_face_and_local(row: i32, col: i32, face_size: i32) -> (i32, i32, i32) {
    let s = face_size;
    let face_row = row / s;
    let face_col = col / s;
    let local_r = row % s;
    let local_c = col % s;

    let face = if face_row == 0 && face_col == 1 {
        1
    } else if face_row == 0 && face_col == 2 {
        2
    } else if face_row == 1 && face_col == 1 {
        3
    } else if face_row == 2 && face_col == 0 {
        4
    } else if face_row == 2 && face_col == 1 {
        5
    } else if face_row == 3 && face_col == 0 {
        6
    } else {
        -1
    };

    (face, local_r, local_c)
}

fn wrap_cube(row: i32, col: i32, facing: i32, face_size: i32) -> (i32, i32, i32) {
    let s = face_size;
    let (face, lr, lc) = get_cube_face_and_local(row, col, s);

    match face {
        1 => {
            if facing == 3 {
                // Up: goes to face 6, from left, facing right
                return (3 * s + lc, 0, 0);
            } else if facing == 2 {
                // Left: goes to face 4, from left, facing right (inverted)
                return (3 * s - 1 - lr, 0, 0);
            }
        }
        2 => {
            if facing == 0 {
                // Right: goes to face 5, from right, facing left (inverted)
                return (3 * s - 1 - lr, 2 * s - 1, 2);
            } else if facing == 1 {
                // Down: goes to face 3, from right, facing left
                return (s + lc, 2 * s - 1, 2);
            } else if facing == 3 {
                // Up: goes to face 6, from bottom, facing up
                return (4 * s - 1, lc, 3);
            }
        }
        3 => {
            if facing == 0 {
                // Right: goes to face 2, from bottom, facing up
                return (s - 1, 2 * s + lr, 3);
            } else if facing == 2 {
                // Left: goes to face 4, from top, facing down
                return (2 * s, lr, 1);
            }
        }
        4 => {
            if facing == 3 {
                // Up: goes to face 3, from left, facing right
                return (s + lc, s, 0);
            } else if facing == 2 {
                // Left: goes to face 1, from left, facing right (inverted)
                return (s - 1 - lr, s, 0);
            }
        }
        5 => {
            if facing == 0 {
                // Right: goes to face 2, from right, facing left (inverted)
                return (s - 1 - lr, 3 * s - 1, 2);
            } else if facing == 1 {
                // Down: goes to face 6, from right, facing left
                return (3 * s + lc, s - 1, 2);
            }
        }
        6 => {
            if facing == 0 {
                // Right: goes to face 5, from bottom, facing up
                return (3 * s - 1, s + lr, 3);
            } else if facing == 1 {
                // Down: goes to face 2, from top, facing down
                return (0, 2 * s + lc, 1);
            } else if facing == 2 {
                // Left: goes to face 1, from top, facing down
                return (0, s + lr, 1);
            }
        }
        _ => {}
    }

    (row, col, facing)
}

fn part2(text: &str) -> i32 {
    let (grid, instructions) = parse_input(text);
    let height = grid.len() as i32;
    let width = grid[0].len() as i32;

    // Determine face size
    let face_size = if height > 50 { 50 } else { 4 };

    // Directions: 0=right, 1=down, 2=left, 3=up
    let dr = [0, 1, 0, -1];
    let dc = [1, 0, -1, 0];

    // Find starting position
    let mut row: i32 = 0;
    let mut col: i32 = grid[0].iter().position(|&c| c == '.').unwrap() as i32;
    let mut facing: i32 = 0;

    for instr in &instructions {
        match instr {
            Instruction::Move(steps) => {
                for _ in 0..*steps {
                    let mut nr = row + dr[facing as usize];
                    let mut nc = col + dc[facing as usize];
                    let mut nf = facing;

                    // Check if we need to wrap
                    let need_wrap = nr < 0
                        || nr >= height
                        || nc < 0
                        || nc >= width
                        || grid[nr as usize][nc as usize] == ' ';

                    if need_wrap {
                        let (wr, wc, wf) = wrap_cube(row, col, facing, face_size);
                        nr = wr;
                        nc = wc;
                        nf = wf;
                    }

                    // Check for wall
                    if grid[nr as usize][nc as usize] == '#' {
                        break;
                    }

                    row = nr;
                    col = nc;
                    facing = nf;
                }
            }
            Instruction::Turn(dir) => {
                if *dir == 'R' {
                    facing = (facing + 1) % 4;
                } else {
                    facing = (facing + 3) % 4;
                }
            }
        }
    }

    1000 * (row + 1) + 4 * (col + 1) + facing
}

fn main() {
    let script_dir = Path::new(file!()).parent().unwrap();
    let input_path = script_dir.join("../input.txt");
    let text = fs::read_to_string(&input_path)
        .unwrap_or_else(|_| fs::read_to_string("../input.txt").expect("Failed to read input file"));

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
