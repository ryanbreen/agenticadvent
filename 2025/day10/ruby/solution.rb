#!/usr/bin/env ruby
# Day 10: Factory - Linear algebra over GF(2) to minimize button presses.

require 'rational'
require 'set'

def parse_line(line)
  # Extract indicator pattern [.##.]
  indicator = line.match(/\[([.#]+)\]/)[1]
  n_lights = indicator.length

  # Target state: 1 where # appears
  target = 0
  indicator.each_char.with_index do |c, i|
    target |= (1 << i) if c == '#'
  end

  # Extract button schematics (0,1,2) etc.
  buttons = []
  line.scan(/\(([0-9,]+)\)/) do |match|
    indices = match[0].split(',').map(&:to_i)
    mask = 0
    indices.each { |idx| mask |= (1 << idx) }
    buttons << mask
  end

  [n_lights, target, buttons]
end

def parse_line_part2(line)
  # Extract joltage requirements {3,5,4,7}
  joltage = line.match(/\{([0-9,]+)\}/)[1].split(',').map(&:to_i)
  n_counters = joltage.length

  # Extract button schematics (0,1,2) etc.
  buttons = []
  line.scan(/\(([0-9,]+)\)/) do |match|
    buttons << match[0].split(',').map(&:to_i)
  end

  [n_counters, joltage, buttons]
end

def count_bits(n)
  count = 0
  while n > 0
    count += n & 1
    n >>= 1
  end
  count
end

def solve_machine_brute(n_lights, target, buttons)
  n_buttons = buttons.length
  min_presses = Float::INFINITY

  # Try all 2^n_buttons combinations
  (0...(1 << n_buttons)).each do |mask|
    state = 0
    presses = 0

    n_buttons.times do |i|
      if mask & (1 << i) != 0
        state ^= buttons[i]
        presses += 1
      end
    end

    min_presses = presses if state == target && presses < min_presses
  end

  min_presses == Float::INFINITY ? 0 : min_presses
end

def part1(lines)
  total = 0
  lines.each do |line|
    line = line.strip
    next if line.empty?

    n_lights, target, buttons = parse_line(line)
    min_presses = solve_machine_brute(n_lights, target, buttons)
    total += min_presses
  end
  total
end

def solve_machine_part2(n_counters, joltage, buttons)
  # Solve Part 2: minimize button presses to reach target joltage levels.
  # This is an integer linear programming problem: min sum(x) s.t. Ax = b, x >= 0.

  n_buttons = buttons.length

  if n_buttons == 0
    return joltage.all?(&:zero?) ? 0 : Float::INFINITY
  end

  # Build matrix A (n_counters x n_buttons) using Rational
  a = Array.new(n_counters) { Array.new(n_buttons, Rational(0)) }
  buttons.each_with_index do |indices, j|
    indices.each do |idx|
      a[idx][j] = Rational(1) if idx < n_counters
    end
  end

  b = joltage.map { |j| Rational(j) }

  # Augmented matrix [A | b]
  aug = a.map.with_index { |row, i| row + [b[i]] }
  n_rows = n_counters
  n_cols = n_buttons

  # Gaussian elimination with partial pivoting
  pivot_cols = []
  pivot_row = 0

  n_cols.times do |col|
    # Find non-zero entry in this column
    found = -1
    (pivot_row...n_rows).each do |row|
      if aug[row][col] != 0
        found = row
        break
      end
    end

    next if found == -1

    # Swap rows
    aug[pivot_row], aug[found] = aug[found], aug[pivot_row]
    pivot_cols << [col, pivot_row]

    # Scale pivot row
    scale = aug[pivot_row][col]
    (n_cols + 1).times do |c|
      aug[pivot_row][c] /= scale
    end

    # Eliminate column in other rows
    n_rows.times do |row|
      if row != pivot_row && aug[row][col] != 0
        factor = aug[row][col]
        (n_cols + 1).times do |c|
          aug[row][c] -= factor * aug[pivot_row][c]
        end
      end
    end

    pivot_row += 1
  end

  # Check for inconsistency (rows with all zeros but non-zero RHS)
  (pivot_row...n_rows).each do |row|
    return Float::INFINITY if aug[row][n_cols] != 0
  end

  # Identify free variables (columns not in pivot_cols)
  pivot_col_set = pivot_cols.map { |c, _| c }.to_set
  free_vars = (0...n_cols).reject { |c| pivot_col_set.include?(c) }

  # If no free variables, we have a unique solution
  if free_vars.empty?
    solution = Array.new(n_buttons, Rational(0))
    pivot_cols.each do |col, row|
      solution[col] = aug[row][n_cols]
    end

    # Check non-negative integers
    total = 0
    solution.each do |val|
      return Float::INFINITY if val < 0 || val.denominator != 1
      total += val.to_i
    end
    return total
  end

  # With free variables, we need to search for optimal solution
  # For each free variable, extract the null space vector
  null_vectors = []
  free_vars.each do |fv|
    vec = Array.new(n_buttons, Rational(0))
    vec[fv] = Rational(1)
    pivot_cols.each do |col, row|
      vec[col] = -aug[row][fv]
    end
    null_vectors << vec
  end

  # Extract particular solution (with free vars = 0)
  particular = Array.new(n_buttons, Rational(0))
  pivot_cols.each do |col, row|
    particular[col] = aug[row][n_cols]
  end

  # Search for optimal non-negative integer solution
  n_free = free_vars.length
  max_j = joltage.max || 100
  min_total = Float::INFINITY

  # For one free variable, solve directly
  if n_free == 1
    t_low = -Float::INFINITY
    t_high = Float::INFINITY

    n_buttons.times do |j|
      p = particular[j]
      nv = null_vectors[0][j]

      if nv == 0
        return Float::INFINITY if p < 0
      elsif nv > 0
        bound = -p / nv
        t_low = [t_low, bound].max
      else
        bound = -p / nv
        t_high = [t_high, bound].min
      end
    end

    return Float::INFINITY if t_low > t_high

    t_low_int = t_low.ceil
    t_high_int = t_high.floor

    (t_low_int..t_high_int).each do |t|
      t_frac = Rational(t)
      total = 0
      valid = true

      n_buttons.times do |j|
        val = particular[j] + t_frac * null_vectors[0][j]
        if val < 0 || val.denominator != 1
          valid = false
          break
        end
        total += val.to_i
      end

      min_total = total if valid && total < min_total
    end

    return min_total == Float::INFINITY ? 0 : min_total
  end

  # For 2 free variables
  if n_free == 2
    # Compute bounds for t0
    bounds = []
    2.times do |i|
      t_low = -Float::INFINITY
      t_high = Float::INFINITY
      n_buttons.times do |j|
        p = particular[j].to_f
        nv = null_vectors[i][j].to_f
        if nv > 0
          t_low = [t_low, -p / nv].max
        elsif nv < 0
          t_high = [t_high, -p / nv].min
        end
      end
      t_low = [(-max_j * 2), (t_low - max_j).floor].max
      t_high = [(max_j * 2), (t_high + max_j).ceil].min
      bounds << [t_low, t_high]
    end

    t0_low, t0_high = bounds[0]
    (t0_low..t0_high).each do |t0|
      t0_frac = Rational(t0)
      intermediate = n_buttons.times.map { |j| particular[j] + t0_frac * null_vectors[0][j] }

      # Compute bounds for t1 given t0
      t1_low_cur = -Float::INFINITY
      t1_high_cur = Float::INFINITY
      n_buttons.times do |j|
        p = intermediate[j].to_f
        nv = null_vectors[1][j].to_f
        if nv > 0
          t1_low_cur = [t1_low_cur, -p / nv].max
        elsif nv < 0
          t1_high_cur = [t1_high_cur, -p / nv].min
        end
      end

      t1_low_int = t1_low_cur.ceil
      t1_high_int = t1_high_cur.floor

      (t1_low_int..t1_high_int).each do |t1|
        t1_frac = Rational(t1)
        valid = true
        total = 0

        n_buttons.times do |j|
          val = intermediate[j] + t1_frac * null_vectors[1][j]
          if val < 0 || val.denominator != 1
            valid = false
            break
          end
          total += val.to_i
        end

        min_total = total if valid && total < min_total
      end
    end

    return min_total == Float::INFINITY ? 0 : min_total
  end

  # For 3 free variables
  if n_free == 3
    bound = max_j

    (-bound..bound).each do |t0|
      t0_frac = Rational(t0)
      inter0 = n_buttons.times.map { |j| particular[j] + t0_frac * null_vectors[0][j] }

      # Compute bounds for t1 given t0
      t1_low = -Float::INFINITY
      t1_high = Float::INFINITY
      n_buttons.times do |j|
        p = inter0[j].to_f
        nv = null_vectors[1][j].to_f
        if nv > 0
          t1_low = [t1_low, -p / nv - bound].max
        elsif nv < 0
          t1_high = [t1_high, -p / nv + bound].min
        end
      end

      t1_low_int = [t1_low.ceil, -bound].max
      t1_high_int = [t1_high.floor, bound].min

      (t1_low_int..t1_high_int).each do |t1|
        t1_frac = Rational(t1)
        inter1 = n_buttons.times.map { |j| inter0[j] + t1_frac * null_vectors[1][j] }

        # Compute bounds for t2 given t0, t1
        t2_low = -Float::INFINITY
        t2_high = Float::INFINITY
        n_buttons.times do |j|
          p = inter1[j].to_f
          nv = null_vectors[2][j].to_f
          if nv > 0
            t2_low = [t2_low, -p / nv].max
          elsif nv < 0
            t2_high = [t2_high, -p / nv].min
          end
        end

        t2_low_int = t2_low.ceil
        t2_high_int = t2_high.floor

        (t2_low_int..t2_high_int).each do |t2|
          t2_frac = Rational(t2)
          valid = true
          total = 0

          n_buttons.times do |j|
            val = inter1[j] + t2_frac * null_vectors[2][j]
            if val < 0 || val.denominator != 1
              valid = false
              break
            end
            total += val.to_i
          end

          min_total = total if valid && total < min_total
        end
      end
    end

    return min_total == Float::INFINITY ? 0 : min_total
  end

  # For more free variables, use recursive search
  if n_free <= 6
    search_recursive = lambda do |idx, partial|
      if idx == n_free
        valid = true
        total = 0
        partial.each do |val|
          if val < 0 || val.denominator != 1
            valid = false
            break
          end
          total += val.to_i
        end
        min_total = total if valid && total < min_total
        return
      end

      # Compute bounds for current free var
      t_low = -Float::INFINITY
      t_high = Float::INFINITY
      n_buttons.times do |j|
        p = partial[j].to_f
        nv = null_vectors[idx][j].to_f
        if nv > 0
          t_low = [t_low, -p / nv].max
        elsif nv < 0
          t_high = [t_high, -p / nv].min
        end
      end

      return if t_low > t_high || t_low.infinite? || t_high.infinite?

      t_low_int = [t_low.ceil - max_j, -max_j * 2].max
      t_high_int = [t_high.floor + max_j, max_j * 2].min

      (t_low_int..t_high_int).each do |t|
        t_frac = Rational(t)
        new_partial = n_buttons.times.map { |j| partial[j] + t_frac * null_vectors[idx][j] }
        search_recursive.call(idx + 1, new_partial)
      end
    end

    search_recursive.call(0, particular.dup)
    return min_total == Float::INFINITY ? 0 : min_total
  end

  0  # Fallback for large null spaces
end

def part2(lines)
  total = 0
  lines.each do |line|
    line = line.strip
    next if line.empty?

    n_counters, joltage, buttons = parse_line_part2(line)
    min_presses = solve_machine_part2(n_counters, joltage, buttons)
    total += min_presses
  end
  total
end

# Main execution
input_file = File.join(File.dirname(__FILE__), '..', 'input.txt')
lines = File.read(input_file).strip.split("\n")

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
