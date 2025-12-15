#!/usr/bin/env ruby

def parse_input(text)
  parts = text.strip.split("\n\n")
  grid_lines = parts[0].split("\n")
  grid = grid_lines.map { |line| line.chars }
  moves = parts[1].gsub(/\s+/, '')
  [grid, moves]
end

def find_robot(grid)
  grid.each_with_index do |row, r|
    row.each_with_index do |cell, c|
      return [r, c] if cell == '@'
    end
  end
  nil
end

def move_robot(grid, robot_pos, direction)
  deltas = { '<' => [0, -1], '>' => [0, 1], '^' => [-1, 0], 'v' => [1, 0] }
  dr, dc = deltas[direction]
  r, c = robot_pos
  nr, nc = r + dr, c + dc

  return robot_pos if grid[nr][nc] == '#'

  if grid[nr][nc] == '.'
    grid[r][c] = '.'
    grid[nr][nc] = '@'
    return [nr, nc]
  end

  if grid[nr][nc] == 'O'
    check_r, check_c = nr, nc
    while grid[check_r][check_c] == 'O'
      check_r += dr
      check_c += dc
    end

    return robot_pos if grid[check_r][check_c] == '#'

    grid[check_r][check_c] = 'O'
    grid[r][c] = '.'
    grid[nr][nc] = '@'
    return [nr, nc]
  end

  robot_pos
end

def calculate_gps(grid, box_char = 'O')
  total = 0
  grid.each_with_index do |row, r|
    row.each_with_index do |cell, c|
      total += 100 * r + c if cell == box_char
    end
  end
  total
end

def part1(input_text)
  grid, moves = parse_input(input_text)
  robot_pos = find_robot(grid)

  moves.each_char do |move|
    robot_pos = move_robot(grid, robot_pos, move)
  end

  calculate_gps(grid)
end

def scale_grid(grid)
  new_grid = []
  grid.each do |row|
    new_row = []
    row.each do |cell|
      case cell
      when '#'
        new_row.concat(['#', '#'])
      when 'O'
        new_row.concat(['[', ']'])
      when '.'
        new_row.concat(['.', '.'])
      when '@'
        new_row.concat(['@', '.'])
      end
    end
    new_grid << new_row
  end
  new_grid
end

def can_move_box_vertical(grid, box_left_c, r, dr)
  nr = r + dr
  left_c = box_left_c
  right_c = box_left_c + 1

  left_target = grid[nr][left_c]
  right_target = grid[nr][right_c]

  return false if left_target == '#' || right_target == '#'

  boxes_to_check = Set.new

  if left_target == '['
    boxes_to_check.add([nr, left_c])
  elsif left_target == ']'
    boxes_to_check.add([nr, left_c - 1])
  end

  if right_target == '['
    boxes_to_check.add([nr, right_c])
  elsif right_target == ']'
    boxes_to_check.add([nr, right_c - 1])
  end

  boxes_to_check.each do |box_r, box_c|
    return false unless can_move_box_vertical(grid, box_c, box_r, dr)
  end

  true
end

def collect_boxes_vertical(grid, box_left_c, r, dr, collected)
  collected.add([r, box_left_c])
  nr = r + dr
  left_c = box_left_c
  right_c = box_left_c + 1

  left_target = grid[nr][left_c]
  right_target = grid[nr][right_c]

  boxes_to_check = Set.new

  if left_target == '['
    boxes_to_check.add([nr, left_c])
  elsif left_target == ']'
    boxes_to_check.add([nr, left_c - 1])
  end

  if right_target == '['
    boxes_to_check.add([nr, right_c])
  elsif right_target == ']'
    boxes_to_check.add([nr, right_c - 1])
  end

  boxes_to_check.each do |box_r, box_c|
    collect_boxes_vertical(grid, box_c, box_r, dr, collected) unless collected.include?([box_r, box_c])
  end
end

def move_robot_wide(grid, robot_pos, direction)
  deltas = { '<' => [0, -1], '>' => [0, 1], '^' => [-1, 0], 'v' => [1, 0] }
  dr, dc = deltas[direction]
  r, c = robot_pos
  nr, nc = r + dr, c + dc

  target = grid[nr][nc]

  return robot_pos if target == '#'

  if target == '.'
    grid[r][c] = '.'
    grid[nr][nc] = '@'
    return [nr, nc]
  end

  if ['[', ']'].include?(target)
    if dc != 0  # Horizontal movement
      check_c = nc
      while ['[', ']'].include?(grid[r][check_c])
        check_c += dc
      end

      return robot_pos if grid[r][check_c] == '#'

      # Shift all boxes
      if dc > 0  # Moving right
        check_c.downto(nc + 1) do |col|
          grid[r][col] = grid[r][col - 1]
        end
      else  # Moving left
        check_c.upto(nc - 1) do |col|
          grid[r][col] = grid[r][col + 1]
        end
      end

      grid[r][c] = '.'
      grid[nr][nc] = '@'
      return [nr, nc]
    else  # Vertical movement
      # Find the left edge of the box
      box_left_c = target == '[' ? nc : nc - 1

      # Check if we can move this box
      return robot_pos unless can_move_box_vertical(grid, box_left_c, nr, dr)

      # Collect all boxes that need to move
      boxes_to_move = Set.new
      collect_boxes_vertical(grid, box_left_c, nr, dr, boxes_to_move)

      # Sort boxes by row so we move them in the right order
      sorted_boxes = boxes_to_move.to_a.sort_by do |box_r, box_c|
        dr > 0 ? -box_r : box_r
      end

      # Move all boxes
      sorted_boxes.each do |box_r, box_c|
        grid[box_r][box_c] = '.'
        grid[box_r][box_c + 1] = '.'
        grid[box_r + dr][box_c] = '['
        grid[box_r + dr][box_c + 1] = ']'
      end

      # Move robot
      grid[r][c] = '.'
      grid[nr][nc] = '@'
      return [nr, nc]
    end
  end

  robot_pos
end

def part2(input_text)
  grid, moves = parse_input(input_text)
  grid = scale_grid(grid)
  robot_pos = find_robot(grid)

  moves.each_char do |move|
    robot_pos = move_robot_wide(grid, robot_pos, move)
  end

  calculate_gps(grid, '[')
end

if __FILE__ == $0
  require 'set'

  input_text = File.read(File.join(__dir__, '..', 'input.txt'))

  puts "Part 1: #{part1(input_text)}"
  puts "Part 2: #{part2(input_text)}"
end
