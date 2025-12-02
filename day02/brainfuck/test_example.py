#!/usr/bin/env python3
"""Test with the example from the problem"""

from solution import is_invalid_id, solve

# Test individual invalid IDs from the example
test_cases = [
    (11, True),     # 11-22 range
    (22, True),     # 11-22 range
    (99, True),     # 95-115 range
    (1010, True),   # 998-1012 range
    (1188511885, True),  # 1188511880-1188511890 range
    (222222, True), # 222220-222224 range
    (446446, True), # 446443-446449 range
    (38593859, True),  # 38593856-38593862 range
    (101, False),   # Valid ID (not repeated)
    (123, False),   # Valid ID (odd length)
]

print("Testing individual cases:")
for num, expected in test_cases:
    result = is_invalid_id(num)
    status = "✓" if result == expected else "✗"
    print(f"{status} {num}: {result} (expected {expected})")

# Test the full example
example_input = """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124"""

total, count = solve(example_input)
print(f"\nExample result: {total}")
print(f"Expected: 1227775554")
print(f"Match: {'✓' if total == 1227775554 else '✗'}")
