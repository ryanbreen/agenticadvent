#!/usr/bin/env ruby
# Day 23: A Long Walk - Longest path through hiking trails.

require 'set'

def parse_input(filename)
  File.read(filename).strip.split("\n")
end

def find_junctions(grid)
  rows = grid.length
  cols = grid[0].length
  junctions = Set.new

  # Start and end points
  start_col = grid[0].index('.')
  end_col = grid[rows - 1].index('.')
  junctions.add([0, start_col])
  junctions.add([rows - 1, end_col])

  # Find intersections (cells with 3+ walkable neighbors)
  (0...rows).each do |r|
    (0...cols).each do |c|
      next if grid[r][c] == '#'

      neighbors = 0
      [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
        nr, nc = r + dr, c + dc
        if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#'
          neighbors += 1
        end
      end

      junctions.add([r, c]) if neighbors >= 3
    end
  end

  junctions
end

def build_graph(grid, junctions, respect_slopes)
  rows = grid.length
  cols = grid[0].length

  # Direction mappings for slopes
  slope_dirs = {
    '^' => [-1, 0],
    'v' => [1, 0],
    '<' => [0, -1],
    '>' => [0, 1]
  }

  graph = Hash.new { |h, k| h[k] = {} }

  junctions.each do |start_junction|
    # BFS/DFS from each junction to find reachable junctions
    stack = [[start_junction, 0]]
    visited = Set.new([start_junction])

    while !stack.empty?
      pos, dist = stack.pop
      r, c = pos

      if dist > 0 && junctions.include?([r, c])
        # Found another junction
        graph[start_junction][[r, c]] = dist
        next
      end

      # Explore neighbors
      [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
        nr, nc = r + dr, c + dc

        next unless nr >= 0 && nr < rows && nc >= 0 && nc < cols
        next if grid[nr][nc] == '#'
        next if visited.include?([nr, nc])

        # Check slope constraints for Part 1
        if respect_slopes
          cell = grid[r][c]
          if slope_dirs.key?(cell)
            req_dr, req_dc = slope_dirs[cell]
            next if dr != req_dr || dc != req_dc
          end
        end

        visited.add([nr, nc])
        stack.push([[nr, nc], dist + 1])
      end
    end
  end

  graph
end

def longest_path_dfs(graph, start_pos, end_pos)
  visited = Set.new

  dfs = lambda do |node|
    return 0 if node == end_pos

    visited.add(node)
    max_dist = nil

    graph[node].each do |neighbor, dist|
      unless visited.include?(neighbor)
        result = dfs.call(neighbor)
        if result
          candidate = dist + result
          max_dist = candidate if max_dist.nil? || candidate > max_dist
        end
      end
    end

    visited.delete(node)
    max_dist
  end

  dfs.call(start_pos)
end

def solve(grid, respect_slopes)
  rows = grid.length
  start_pos = [0, grid[0].index('.')]
  end_pos = [rows - 1, grid[rows - 1].index('.')]

  junctions = find_junctions(grid)
  graph = build_graph(grid, junctions, respect_slopes)

  longest_path_dfs(graph, start_pos, end_pos)
end

def part1(grid)
  solve(grid, true)
end

def part2(grid)
  solve(grid, false)
end

def main
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  grid = parse_input(input_path)
  puts "Part 1: #{part1(grid)}"
  puts "Part 2: #{part2(grid)}"
end

main if __FILE__ == $0
