#!/usr/bin/env python3
"""
Advent of Code 2024 Day 9: Disk Fragmenter

Compact a fragmented disk by moving file blocks to fill gaps.
Part 1: Move blocks one at a time from end to leftmost free space
Part 2: Move whole files to leftmost span that fits
"""

def parse_disk_map(filename: str) -> list[int]:
    """
    Parse disk map into expanded block representation.
    Returns list where each element is file ID or -1 for free space.
    """
    with open(filename) as f:
        disk_map = f.read().strip()

    blocks = []
    file_id = 0
    is_file = True

    for digit in disk_map:
        length = int(digit)
        if is_file:
            blocks.extend([file_id] * length)
            file_id += 1
        else:
            blocks.extend([-1] * length)  # -1 represents free space
        is_file = not is_file

    return blocks


def compact_blocks(blocks: list[int]) -> list[int]:
    """
    Compact disk by moving blocks one at a time from end to leftmost free space.
    """
    blocks = blocks.copy()
    left = 0
    right = len(blocks) - 1

    while left < right:
        # Find leftmost free space
        while left < right and blocks[left] != -1:
            left += 1
        # Find rightmost file block
        while left < right and blocks[right] == -1:
            right -= 1

        if left < right:
            # Swap
            blocks[left] = blocks[right]
            blocks[right] = -1
            left += 1
            right -= 1

    return blocks


def calculate_checksum(blocks: list[int]) -> int:
    """Calculate filesystem checksum: sum of position * file_id for each block."""
    checksum = 0
    for pos, file_id in enumerate(blocks):
        if file_id != -1:
            checksum += pos * file_id
    return checksum


def part1() -> int:
    """Compact by moving individual blocks, return checksum."""
    blocks = parse_disk_map('../input.txt')
    compacted = compact_blocks(blocks)
    return calculate_checksum(compacted)


def part2() -> int:
    """Compact by moving whole files (highest ID first), return checksum."""
    blocks = parse_disk_map('../input.txt')

    # Find all files: file_id -> (start_pos, length)
    files = {}
    i = 0
    while i < len(blocks):
        if blocks[i] != -1:
            file_id = blocks[i]
            start = i
            while i < len(blocks) and blocks[i] == file_id:
                i += 1
            files[file_id] = (start, i - start)
        else:
            i += 1

    # Process files in decreasing order of file ID
    max_file_id = max(files.keys())

    for file_id in range(max_file_id, -1, -1):
        start, length = files[file_id]

        # Find leftmost span of free space that fits this file
        # Must be to the left of current position
        free_start = None
        i = 0
        while i < start:
            if blocks[i] == -1:
                # Count consecutive free blocks
                span_start = i
                span_length = 0
                while i < start and blocks[i] == -1:
                    span_length += 1
                    i += 1
                if span_length >= length:
                    free_start = span_start
                    break
            else:
                i += 1

        # Move file if we found a suitable span
        if free_start is not None:
            # Clear old position
            for j in range(start, start + length):
                blocks[j] = -1
            # Write to new position
            for j in range(free_start, free_start + length):
                blocks[j] = file_id
            # Update file position
            files[file_id] = (free_start, length)

    return calculate_checksum(blocks)


if __name__ == '__main__':
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
