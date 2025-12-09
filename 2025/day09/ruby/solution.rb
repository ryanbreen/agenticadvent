#!/usr/bin/env ruby

require 'pathname'

input_text = Pathname.new(__FILE__).parent.parent.join('input.txt').read.strip

# Parse input - each line is "x,y"
points = []
input_text.each_line do |line|
  x, y = line.split(',').map(&:to_i)
  points << [x, y]
end

def part1(points)
  """Find the largest rectangle area using two red tiles as opposite corners."""
  max_area = 0
  n = points.length

  # Check all pairs of points as opposite corners
  (0...n).each do |i|
    x1, y1 = points[i]
    ((i + 1)...n).each do |j|
      x2, y2 = points[j]
      # Rectangle area = width * height (inclusive of both corners)
      # Width includes both endpoints: |x2-x1| + 1
      # Height includes both endpoints: |y2-y1| + 1
      width = (x2 - x1).abs + 1
      height = (y2 - y1).abs + 1
      area = width * height
      max_area = [max_area, area].max
    end
  end

  max_area
end

def part2(points)
  """Find the largest rectangle using only red and green tiles."""

  # Build edges - list of (y, x_start, x_end) for horizontal or (x, y_start, y_end) for vertical
  # The polygon consists of horizontal and vertical edges connecting consecutive points

  n = points.length
  horizontal_edges = []  # [y, x_min, x_max]
  vertical_edges = []    # [x, y_min, y_max]

  n.times do |i|
    x1, y1 = points[i]
    x2, y2 = points[(i + 1) % n]

    if y1 == y2  # Horizontal edge
      horizontal_edges << [y1, [x1, x2].min, [x1, x2].max]
    else  # Vertical edge
      vertical_edges << [x1, [y1, y2].min, [y1, y2].max]
    end
  end

  # Sort vertical edges by x coordinate
  vertical_edges.sort!

  # Build a map of vertical edges by x-coordinate for efficient lookup
  vert_by_x = Hash.new { |h, k| h[k] = [] }
  vertical_edges.each do |x, y_min, y_max|
    vert_by_x[x] << [y_min, y_max]
  end

  # Build a map of horizontal edges by y-coordinate
  horiz_by_y = Hash.new { |h, k| h[k] = [] }
  horizontal_edges.each do |y, x_min, x_max|
    horiz_by_y[y] << [x_min, x_max]
  end

  is_inside_polygon = lambda do |x, y|
    """Check if point (x, y) is inside or on the polygon using ray casting."""
    crossings = 0.0
    # Cast ray to the right
    vert_by_x.keys.sort.each do |vx|
      next if vx <= x
      vert_by_x[vx].each do |y_min, y_max|
        if y_min < y && y < y_max  # Strict inequality - ray crosses edge
          crossings += 1
        elsif y == y_min || y == y_max
          # On corner - count as 0.5 crossing
          crossings += 0.5
        end
      end
    end
    (crossings % 2) == 1
  end

  rectangle_valid = lambda do |x1, y1, x2, y2|
    """Check if rectangle from (x1,y1) to (x2,y2) is entirely inside polygon."""
    min_x, max_x = [x1, x2].minmax
    min_y, max_y = [y1, y2].minmax

    # Check if any vertical edge crosses through the rectangle interior
    vert_by_x.each do |vx, edges|
      if min_x < vx && vx < max_x  # Vertical edge x is inside rectangle's x range
        edges.each do |y_min, y_max|
          # Check if this edge segment overlaps with rectangle's y range
          return false unless (y_max <= min_y || y_min >= max_y)
        end
      end
    end

    # Check if any horizontal edge crosses through the rectangle interior
    horiz_by_y.each do |hy, edges|
      if min_y < hy && hy < max_y  # Horizontal edge y is inside rectangle's y range
        edges.each do |x_min, x_max|
          # Check if this edge segment overlaps with rectangle's x range
          return false unless (x_max <= min_x || x_min >= max_x)
        end
      end
    end

    # Finally, check that we're inside the polygon (not outside)
    # Check center point
    center_x = (min_x + max_x) / 2.0
    center_y = (min_y + max_y) / 2.0
    is_inside_polygon.call(center_x, center_y)
  end

  # Find largest valid rectangle with red corners
  max_area = 0

  (0...points.length).each do |i|
    x1, y1 = points[i]
    ((i + 1)...points.length).each do |j|
      x2, y2 = points[j]

      if rectangle_valid.call(x1, y1, x2, y2)
        width = (x2 - x1).abs + 1
        height = (y2 - y1).abs + 1
        area = width * height
        max_area = [max_area, area].max
      end
    end
  end

  max_area
end

puts "Part 1: #{part1(points)}"
puts "Part 2: #{part2(points)}"
