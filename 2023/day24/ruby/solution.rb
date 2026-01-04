#!/usr/bin/env ruby

def parse_input(filename)
  hailstones = []
  File.readlines(filename).each do |line|
    pos, vel = line.strip.split('@')
    px, py, pz = pos.split(',').map(&:to_i)
    vx, vy, vz = vel.split(',').map(&:to_i)
    hailstones << [[px, py, pz], [vx, vy, vz]]
  end
  hailstones
end

def find_intersection_2d(h1, h2)
  # Find intersection of two hailstone paths in 2D (XY plane).
  # Returns [x, y, t1, t2] where t1 and t2 are times for each hailstone.
  # Returns nil if paths are parallel or don't intersect.
  px1, py1, _ = h1[0]
  vx1, vy1, _ = h1[1]
  px2, py2, _ = h2[0]
  vx2, vy2, _ = h2[1]

  # Using Cramer's rule:
  # vx1*t1 - vx2*t2 = px2 - px1
  # vy1*t1 - vy2*t2 = py2 - py1
  det = vx1 * (-vy2) - (-vx2) * vy1
  return nil if det == 0  # Parallel lines

  dx = px2 - px1
  dy = py2 - py1

  t1 = (dx * (-vy2) - (-vx2) * dy).to_f / det
  t2 = (vx1 * dy - dx * vy1).to_f / det

  # Calculate intersection point
  x = px1 + vx1 * t1
  y = py1 + vy1 * t1

  [x, y, t1, t2]
end

def part1(hailstones, min_coord = 200000000000000, max_coord = 400000000000000)
  # Count intersections within test area, in the future for both hailstones.
  count = 0

  hailstones.combination(2).each do |h1, h2|
    result = find_intersection_2d(h1, h2)
    next if result.nil?

    x, y, t1, t2 = result

    # Check if intersection is in the future for both hailstones
    next if t1 < 0 || t2 < 0

    # Check if intersection is within test area
    if x >= min_coord && x <= max_coord && y >= min_coord && y <= max_coord
      count += 1
    end
  end

  count
end

def solve_system(matrix, rhs)
  # Solve system of linear equations using Gaussian elimination with Rationals.
  n = matrix.length

  # Augment matrix with rhs
  aug = matrix.each_with_index.map do |row, i|
    row.map { |x| Rational(x) } + [Rational(rhs[i])]
  end

  # Forward elimination
  (0...n).each do |col|
    # Find pivot
    max_row = col
    ((col + 1)...n).each do |row|
      if aug[row][col].abs > aug[max_row][col].abs
        max_row = row
      end
    end
    aug[col], aug[max_row] = aug[max_row], aug[col]

    next if aug[col][col] == 0

    # Eliminate column
    ((col + 1)...n).each do |row|
      if aug[row][col] != 0
        factor = aug[row][col] / aug[col][col]
        (col..(n)).each do |j|
          aug[row][j] -= factor * aug[col][j]
        end
      end
    end
  end

  # Back substitution
  solution = Array.new(n, Rational(0))
  (n - 1).downto(0) do |i|
    solution[i] = aug[i][n]
    ((i + 1)...n).each do |j|
      solution[i] -= aug[i][j] * solution[j]
    end
    solution[i] /= aug[i][i]
  end

  solution
end

def part2(hailstones)
  # Find rock position and velocity that hits all hailstones.
  #
  # The key insight is that for hailstones i and j:
  # (vyi - vyj)*rx + (-vxi + vxj)*ry + (-pyi + pyj)*rvx + (pxi - pxj)*rvy = pxi*vyi - pyi*vxi - (pxj*vyj - pyj*vxj)
  #
  # This is a linear equation in rx, ry, rvx, rvy.
  # We need 4 equations to solve for these 4 unknowns.

  # Take first 5 hailstones to get 4 pairs
  h = hailstones[0, 5]

  # Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
  matrix_xy = []
  rhs_xy = []

  (0...4).each do |i|
    px1, py1, _ = h[i][0]
    vx1, vy1, _ = h[i][1]
    px2, py2, _ = h[i + 1][0]
    vx2, vy2, _ = h[i + 1][1]

    # Coefficients for rx, ry, rvx, rvy
    a = vy1 - vy2
    b = vx2 - vx1
    c = py2 - py1
    d = px1 - px2
    e = px1 * vy1 - py1 * vx1 - (px2 * vy2 - py2 * vx2)

    matrix_xy << [a, b, c, d]
    rhs_xy << e
  end

  rx, ry, rvx, rvy = solve_system(matrix_xy, rhs_xy)

  # Build system for XZ plane (4 equations, 4 unknowns: rx, rz, rvx, rvz)
  # We already know rx and rvx, so we solve the full system and use rz
  matrix_xz = []
  rhs_xz = []

  (0...4).each do |i|
    px1, _, pz1 = h[i][0]
    vx1, _, vz1 = h[i][1]
    px2, _, pz2 = h[i + 1][0]
    vx2, _, vz2 = h[i + 1][1]

    # Same structure as XY but with Z instead of Y
    a = vz1 - vz2
    b = vx2 - vx1
    c = pz2 - pz1
    d = px1 - px2
    e = px1 * vz1 - pz1 * vx1 - (px2 * vz2 - pz2 * vx2)

    matrix_xz << [a, b, c, d]
    rhs_xz << e
  end

  rx2, rz, rvx2, rvz = solve_system(matrix_xz, rhs_xz)

  (rx + ry + rz).to_i
end

def main
  input_file = ARGV[0] || '../input.txt'
  hailstones = parse_input(input_file)

  puts "Part 1: #{part1(hailstones)}"
  puts "Part 2: #{part2(hailstones)}"
end

main
