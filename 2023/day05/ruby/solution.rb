# Day 5: If You Give A Seed A Fertilizer

def parse_input(text)
  sections = text.strip.split("\n\n")

  # Parse seeds
  seeds = sections[0].split(': ')[1].split.map(&:to_i)

  # Parse maps
  maps = sections[1..].map do |section|
    lines = section.strip.split("\n")
    lines[1..].map do |line|
      dst_start, src_start, length = line.split.map(&:to_i)
      [dst_start, src_start, length]
    end
  end

  [seeds, maps]
end

def apply_map(value, ranges)
  ranges.each do |dst_start, src_start, length|
    if value >= src_start && value < src_start + length
      return dst_start + (value - src_start)
    end
  end
  value
end

def seed_to_location(seed, maps)
  value = seed
  maps.each do |map_ranges|
    value = apply_map(value, map_ranges)
  end
  value
end

def part1(seeds, maps)
  seeds.map { |seed| seed_to_location(seed, maps) }.min
end

def apply_map_to_ranges(input_ranges, map_ranges)
  result = []

  input_ranges.each do |start, range_end|
    remaining = [[start, range_end]]

    map_ranges.each do |dst_start, src_start, length|
      src_end = src_start + length
      new_remaining = []

      remaining.each do |r_start, r_end|
        # Part before the map range (unmapped)
        if r_start < src_start
          new_remaining << [r_start, [r_end, src_start].min]
        end

        # Part within the map range (mapped)
        overlap_start = [r_start, src_start].max
        overlap_end = [r_end, src_end].min
        if overlap_start < overlap_end
          offset = dst_start - src_start
          result << [overlap_start + offset, overlap_end + offset]
        end

        # Part after the map range (unmapped)
        if r_end > src_end
          new_remaining << [[r_start, src_end].max, r_end]
        end
      end

      remaining = new_remaining
    end

    # Any remaining parts are unmapped (identity)
    result.concat(remaining)
  end

  result
end

def part2(seeds, maps)
  # Convert seeds to ranges: pairs of [start, start + length]
  ranges = seeds.each_slice(2).map do |start, length|
    [start, start + length]
  end

  # Apply each map to the ranges
  maps.each do |map_ranges|
    ranges = apply_map_to_ranges(ranges, map_ranges)
  end

  # Find minimum start of any range
  ranges.map(&:first).min
end

def main
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  text = File.read(input_path)

  seeds, maps = parse_input(text)

  puts "Part 1: #{part1(seeds, maps)}"
  puts "Part 2: #{part2(seeds, maps)}"
end

main
