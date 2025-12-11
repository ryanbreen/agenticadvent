#!/usr/bin/env ruby
# Advent of Code 2024 Day 9: Disk Fragmenter
#
# Compact a fragmented disk by moving file blocks to fill gaps.
# Part 1: Move blocks one at a time from end to leftmost free space
# Part 2: Move whole files to leftmost span that fits

def parse_disk_map(filename)
  """
  Parse disk map into expanded block representation.
  Returns array where each element is file ID or -1 for free space.
  """
  disk_map = File.read(filename).strip

  blocks = []
  file_id = 0
  is_file = true

  disk_map.each_char do |digit|
    length = digit.to_i
    if is_file
      blocks.concat([file_id] * length)
      file_id += 1
    else
      blocks.concat([-1] * length)  # -1 represents free space
    end
    is_file = !is_file
  end

  blocks
end

def compact_blocks(blocks)
  """
  Compact disk by moving blocks one at a time from end to leftmost free space.
  """
  blocks = blocks.dup
  left = 0
  right = blocks.length - 1

  while left < right
    # Find leftmost free space
    while left < right && blocks[left] != -1
      left += 1
    end
    # Find rightmost file block
    while left < right && blocks[right] == -1
      right -= 1
    end

    if left < right
      # Swap
      blocks[left] = blocks[right]
      blocks[right] = -1
      left += 1
      right -= 1
    end
  end

  blocks
end

def calculate_checksum(blocks)
  """Calculate filesystem checksum: sum of position * file_id for each block."""
  checksum = 0
  blocks.each_with_index do |file_id, pos|
    if file_id != -1
      checksum += pos * file_id
    end
  end
  checksum
end

def part1
  """Compact by moving individual blocks, return checksum."""
  blocks = parse_disk_map('../input.txt')
  compacted = compact_blocks(blocks)
  calculate_checksum(compacted)
end

def part2
  """Compact by moving whole files (highest ID first), return checksum."""
  blocks = parse_disk_map('../input.txt')

  # Find all files: file_id -> [start_pos, length]
  files = {}
  i = 0
  while i < blocks.length
    if blocks[i] != -1
      file_id = blocks[i]
      start = i
      while i < blocks.length && blocks[i] == file_id
        i += 1
      end
      files[file_id] = [start, i - start]
    else
      i += 1
    end
  end

  # Process files in decreasing order of file ID
  max_file_id = files.keys.max

  max_file_id.downto(0) do |file_id|
    start, length = files[file_id]

    # Find leftmost span of free space that fits this file
    # Must be to the left of current position
    free_start = nil
    i = 0
    while i < start
      if blocks[i] == -1
        # Count consecutive free blocks
        span_start = i
        span_length = 0
        while i < start && blocks[i] == -1
          span_length += 1
          i += 1
        end
        if span_length >= length
          free_start = span_start
          break
        end
      else
        i += 1
      end
    end

    # Move file if we found a suitable span
    if free_start
      # Clear old position
      (start...(start + length)).each do |j|
        blocks[j] = -1
      end
      # Write to new position
      (free_start...(free_start + length)).each do |j|
        blocks[j] = file_id
      end
      # Update file position
      files[file_id] = [free_start, length]
    end
  end

  calculate_checksum(blocks)
end

if __FILE__ == $0
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end
