#!/usr/bin/env ruby

def parse_grid(input_text)
  grid = []
  start_pos = nil
  end_pos = nil

  input_text.strip.split("\n").each_with_index do |line, r|
    row = line.chars
    grid << row
    row.each_with_index do |ch, c|
      if ch == 'S'
        start_pos = [r, c]
      elsif ch == 'E'
        end_pos = [r, c]
      end
    end
  end

  [grid, start_pos, end_pos]
end

def trace_path(grid, start_pos, end_pos)
  # Trace the single path from start to end, returning distance from start for each cell
  rows = grid.length
  cols = grid[0].length
  dist = { start_pos => 0 }
  queue = [start_pos]

  while !queue.empty?
    r, c = queue.shift
    break if [r, c] == end_pos

    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
      nr, nc = r + dr, c + dc
      if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#'
        unless dist.key?([nr, nc])
          dist[[nr, nc]] = dist[[r, c]] + 1
          queue << [nr, nc]
        end
      end
    end
  end

  dist
end

def count_cheats(dist, max_cheat_time, min_savings)
  # Count cheats that save at least min_savings picoseconds
  count = 0
  track_positions = dist.keys

  track_positions.each do |pos1|
    r1, c1 = pos1
    d1 = dist[pos1]

    track_positions.each do |pos2|
      r2, c2 = pos2
      # Manhattan distance is the cheat cost
      cheat_cost = (r2 - r1).abs + (c2 - c1).abs

      if cheat_cost <= max_cheat_time
        d2 = dist[pos2]
        savings = d2 - d1 - cheat_cost
        count += 1 if savings >= min_savings
      end
    end
  end

  count
end

def part1(grid, start_pos, end_pos)
  dist = trace_path(grid, start_pos, end_pos)
  count_cheats(dist, 2, 100)
end

def part2(grid, start_pos, end_pos)
  dist = trace_path(grid, start_pos, end_pos)
  count_cheats(dist, 20, 100)
end

def main
  input_text = File.read('../input.txt')
  grid, start_pos, end_pos = parse_grid(input_text)

  puts "Part 1: #{part1(grid, start_pos, end_pos)}"
  puts "Part 2: #{part2(grid, start_pos, end_pos)}"
end

main
