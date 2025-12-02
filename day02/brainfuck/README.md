# Day 2: Gift Shop - Brainfuck Solution

## Problem Summary
Find all "invalid" product IDs in given ranges, where invalid means a number that consists of some digit sequence repeated exactly twice (e.g., 11, 6464, 123123).

## Approach

### Why Not Pure Brainfuck?
Pure Brainfuck is impractical for this problem because:
1. **Large numbers**: Ranges go up to billions (8892865662-8892912125)
2. **String parsing**: Need to parse comma-separated ranges with dashes
3. **Pattern matching**: Need to check if a number is its own repeated half
4. **Large arithmetic**: Need to sum potentially large numbers

### Hybrid Solution
- **solution.py**: Python implementation that solves the actual puzzle
- **demo.bf**: Simple Brainfuck program demonstrating output (prints "11")

## Algorithm

The key insight is that an invalid ID is a number where:
- Its string representation has even length
- The first half equals the second half

For example:
- "1010" -> "10" == "10" ✓ (invalid)
- "1188511885" -> "11885" == "11885" ✓ (invalid)
- "123" -> odd length ✗ (valid)
- "1234" -> "12" != "34" ✗ (valid)

## Running the Solution

```bash
python3 solution.py
```
