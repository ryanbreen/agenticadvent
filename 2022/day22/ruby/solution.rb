#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_input(text)
  parts = text.split("\n\n")
  grid_lines = parts[0].split("\n")
  path = parts[1].strip

  # Find dimensions
  height = grid_lines.length
  width = grid_lines.map(&:length).max

  # Create grid (pad lines to consistent width)
  grid = grid_lines.map { |line| line.ljust(width) }

  # Parse path into moves and turns
  instructions = []
  i = 0
  while i < path.length
    if path[i].match?(/\d/)
      j = i
      j += 1 while j < path.length && path[j].match?(/\d/)
      instructions << path[i...j].to_i
      i = j
    else
      instructions << path[i]
      i += 1
    end
  end

  [grid, instructions]
end

def part1(text)
  grid, instructions = parse_input(text)
  height = grid.length
  width = grid[0].length

  # Directions: 0=right, 1=down, 2=left, 3=up
  dr = [0, 1, 0, -1]
  dc = [1, 0, -1, 0]

  # Find starting position (leftmost open tile on top row)
  row = 0
  col = grid[0].index('.')
  facing = 0  # Start facing right

  instructions.each do |instr|
    if instr.is_a?(Integer)
      # Move forward
      instr.times do
        delta_r = dr[facing]
        delta_c = dc[facing]
        nr = row + delta_r
        nc = col + delta_c

        # Wrap around if needed
        case facing
        when 0  # Right
          if nc >= width || grid[nr][nc] == ' '
            nc = 0
            nc += 1 while grid[nr][nc] == ' '
          end
        when 2  # Left
          if nc < 0 || grid[nr][nc] == ' '
            nc = width - 1
            nc -= 1 while grid[nr][nc] == ' '
          end
        when 1  # Down
          if nr >= height || grid[nr][nc] == ' '
            nr = 0
            nr += 1 while grid[nr][nc] == ' '
          end
        when 3  # Up
          if nr < 0 || grid[nr][nc] == ' '
            nr = height - 1
            nr -= 1 while grid[nr][nc] == ' '
          end
        end

        # Check if we hit a wall
        break if grid[nr][nc] == '#'

        # Move to new position
        row = nr
        col = nc
      end
    else
      # Turn
      if instr == 'R'
        facing = (facing + 1) % 4
      else
        facing = (facing - 1) % 4
      end
    end
  end

  # Calculate password: 1000*row + 4*col + facing (1-indexed)
  1000 * (row + 1) + 4 * (col + 1) + facing
end

def get_cube_face_and_local(row, col, face_size)
  # For the actual input, the cube layout is:
  #   12
  #   3
  #  45
  #  6
  # With face_size = 50

  face_row = row / face_size
  face_col = col / face_size
  local_r = row % face_size
  local_c = col % face_size

  # Map face_row, face_col to face number
  face = case [face_row, face_col]
         when [0, 1] then 1
         when [0, 2] then 2
         when [1, 1] then 3
         when [2, 0] then 4
         when [2, 1] then 5
         when [3, 0] then 6
         else -1
         end

  [face, local_r, local_c]
end

def wrap_cube(row, col, facing, face_size)
  # Handle cube wrapping for the actual input layout:
  #    12
  #    3
  #   45
  #   6
  #
  # Each face is face_size x face_size (50 for the actual input).
  # Returns [new_row, new_col, new_facing].

  s = face_size
  face, lr, lc = get_cube_face_and_local(row, col, s)

  # Based on the direction and which edge we're leaving
  case face
  when 1
    case facing
    when 3  # Up: goes to face 6, from left, facing right
      return [3 * s + lc, 0, 0]
    when 2  # Left: goes to face 4, from left, facing right (inverted)
      return [3 * s - 1 - lr, 0, 0]
    end
  when 2
    case facing
    when 0  # Right: goes to face 5, from right, facing left (inverted)
      return [3 * s - 1 - lr, 2 * s - 1, 2]
    when 1  # Down: goes to face 3, from right, facing left
      return [s + lc, 2 * s - 1, 2]
    when 3  # Up: goes to face 6, from bottom, facing up
      return [4 * s - 1, lc, 3]
    end
  when 3
    case facing
    when 0  # Right: goes to face 2, from bottom, facing up
      return [s - 1, 2 * s + lr, 3]
    when 2  # Left: goes to face 4, from top, facing down
      return [2 * s, lr, 1]
    end
  when 4
    case facing
    when 3  # Up: goes to face 3, from left, facing right
      return [s + lc, s, 0]
    when 2  # Left: goes to face 1, from left, facing right (inverted)
      return [s - 1 - lr, s, 0]
    end
  when 5
    case facing
    when 0  # Right: goes to face 2, from right, facing left (inverted)
      return [s - 1 - lr, 3 * s - 1, 2]
    when 1  # Down: goes to face 6, from right, facing left
      return [3 * s + lc, s - 1, 2]
    end
  when 6
    case facing
    when 0  # Right: goes to face 5, from bottom, facing up
      return [3 * s - 1, s + lr, 3]
    when 1  # Down: goes to face 2, from top, facing down
      return [0, 2 * s + lc, 1]
    when 2  # Left: goes to face 1, from top, facing down
      return [0, s + lr, 1]
    end
  end

  # Shouldn't reach here
  [row, col, facing]
end

def part2(text)
  grid, instructions = parse_input(text)
  height = grid.length
  width = grid[0].length

  # Determine face size
  face_size = height > 50 ? 50 : 4  # 50 for actual input, 4 for example

  # Directions: 0=right, 1=down, 2=left, 3=up
  dr = [0, 1, 0, -1]
  dc = [1, 0, -1, 0]

  # Find starting position
  row = 0
  col = grid[0].index('.')
  facing = 0

  instructions.each do |instr|
    if instr.is_a?(Integer)
      instr.times do
        delta_r = dr[facing]
        delta_c = dc[facing]
        nr = row + delta_r
        nc = col + delta_c
        nf = facing

        # Check if we need to wrap (off grid or hitting a space)
        need_wrap = false
        if nr < 0 || nr >= height || nc < 0 || nc >= width
          need_wrap = true
        elsif grid[nr][nc] == ' '
          need_wrap = true
        end

        if need_wrap
          nr, nc, nf = wrap_cube(row, col, facing, face_size)
        end

        # Check for wall
        break if grid[nr][nc] == '#'

        row = nr
        col = nc
        facing = nf
      end
    else
      if instr == 'R'
        facing = (facing + 1) % 4
      else
        facing = (facing - 1) % 4
      end
    end
  end

  1000 * (row + 1) + 4 * (col + 1) + facing
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
