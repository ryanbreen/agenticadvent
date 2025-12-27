from pathlib import Path


def parse_input(text: str):
    """Parse input into seeds and list of maps."""
    sections = text.strip().split('\n\n')

    # Parse seeds
    seeds = list(map(int, sections[0].split(': ')[1].split()))

    # Parse maps
    maps = []
    for section in sections[1:]:
        lines = section.strip().split('\n')
        ranges = []
        for line in lines[1:]:  # Skip header
            dst_start, src_start, length = map(int, line.split())
            ranges.append((dst_start, src_start, length))
        maps.append(ranges)

    return seeds, maps


def apply_map(value: int, ranges: list) -> int:
    """Apply a single map to transform a value."""
    for dst_start, src_start, length in ranges:
        if src_start <= value < src_start + length:
            return dst_start + (value - src_start)
    return value


def seed_to_location(seed: int, maps: list) -> int:
    """Convert a seed number to a location number through all maps."""
    value = seed
    for map_ranges in maps:
        value = apply_map(value, map_ranges)
    return value


def part1(seeds: list, maps: list) -> int:
    """Find the lowest location number for any initial seed."""
    return min(seed_to_location(seed, maps) for seed in seeds)


def apply_map_to_ranges(input_ranges: list, map_ranges: list) -> list:
    """Apply a map to a list of ranges, returning new ranges."""
    result = []

    for start, end in input_ranges:
        remaining = [(start, end)]

        for dst_start, src_start, length in map_ranges:
            src_end = src_start + length
            new_remaining = []

            for r_start, r_end in remaining:
                # Part before the map range (unmapped)
                if r_start < src_start:
                    new_remaining.append((r_start, min(r_end, src_start)))

                # Part within the map range (mapped)
                overlap_start = max(r_start, src_start)
                overlap_end = min(r_end, src_end)
                if overlap_start < overlap_end:
                    offset = dst_start - src_start
                    result.append((overlap_start + offset, overlap_end + offset))

                # Part after the map range (unmapped)
                if r_end > src_end:
                    new_remaining.append((max(r_start, src_end), r_end))

            remaining = new_remaining

        # Any remaining parts are unmapped (identity)
        result.extend(remaining)

    return result


def part2(seeds: list, maps: list) -> int:
    """Find the lowest location for seed ranges."""
    # Convert seeds to ranges: pairs of (start, start + length)
    ranges = []
    for i in range(0, len(seeds), 2):
        start, length = seeds[i], seeds[i + 1]
        ranges.append((start, start + length))

    # Apply each map to the ranges
    for map_ranges in maps:
        ranges = apply_map_to_ranges(ranges, map_ranges)

    # Find minimum start of any range
    return min(start for start, end in ranges)


def main():
    input_path = Path(__file__).parent.parent / 'input.txt'
    text = input_path.read_text()

    seeds, maps = parse_input(text)

    print('Part 1:', part1(seeds, maps))
    print('Part 2:', part2(seeds, maps))


if __name__ == '__main__':
    main()
