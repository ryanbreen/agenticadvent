#!/usr/bin/env python3
import os

def parse_input(text):
    """Parse numbers from input."""
    return [int(line) for line in text.strip().split('\n')]

def mix(numbers, times=1):
    """Mix the list of numbers, maintaining original order references."""
    n = len(numbers)
    # Store (original_index, value) pairs
    indexed = list(enumerate(numbers))

    for _ in range(times):
        for orig_idx in range(n):
            # Find current position of this element
            for curr_pos, (idx, val) in enumerate(indexed):
                if idx == orig_idx:
                    break

            # Remove from current position
            indexed.pop(curr_pos)

            # Calculate new position (modulo n-1 because we removed the element)
            new_pos = (curr_pos + val) % (n - 1)

            # Insert at new position
            indexed.insert(new_pos, (orig_idx, val))

    return [val for _, val in indexed]

def grove_coordinates(mixed):
    """Find sum of 1000th, 2000th, 3000th values after 0."""
    n = len(mixed)
    zero_idx = mixed.index(0)
    return sum(mixed[(zero_idx + offset) % n] for offset in [1000, 2000, 3000])

def part1(text):
    """Mix once and find grove coordinates."""
    numbers = parse_input(text)
    mixed = mix(numbers, times=1)
    return grove_coordinates(mixed)

def part2(text):
    """Multiply by decryption key, mix 10 times."""
    numbers = parse_input(text)
    decryption_key = 811589153
    numbers = [n * decryption_key for n in numbers]
    mixed = mix(numbers, times=10)
    return grove_coordinates(mixed)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
