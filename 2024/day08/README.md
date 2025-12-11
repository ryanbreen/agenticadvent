# Day 8: Resonant Collinearity

## Problem Summary

You've discovered that antennas on a rooftop are emitting signals at specific frequencies. Each antenna is marked by a single character (lowercase letter, uppercase letter, or digit) representing its frequency. Your task is to find "antinodes" - special points created by the resonant interaction between antennas of the same frequency.

**Input:** A 50x50 grid containing antenna positions marked by alphanumeric characters, with empty spaces marked by `.`

## Part 1: Basic Antinodes

For any pair of antennas with the same frequency, two antinodes are created - one on each side of the pair. Specifically, an antinode occurs at a point that is perfectly in line with both antennas, where one antenna is exactly twice as far from the antinode as the other.

Given antennas at positions A and B:
- Antinode 1 is at position `2*A - B` (beyond A, away from B)
- Antinode 2 is at position `2*B - A` (beyond B, away from A)

**Answer: 400**

## Part 2: Harmonic Antinodes

With resonant harmonics considered, an antinode occurs at **any** grid position exactly in line with at least two antennas of the same frequency, regardless of distance. This means every point along the line through two antennas becomes an antinode, including the antenna positions themselves.

**Answer: 1280**

## Algorithmic Approach

### Key Insight

The problem is fundamentally about **collinearity** - finding points that lie on the same line as pairs of points. The key realization is:

1. **Part 1**: Uses the specific distance ratio (2:1) to calculate exactly two antinode positions per antenna pair
2. **Part 2**: Extends to all integer grid positions along the line connecting any antenna pair

### Data Structures Used

1. **Hash Map/Dictionary**: Group antenna positions by frequency character
   - Key: frequency character (case-sensitive!)
   - Value: list of (row, col) positions

2. **Set**: Store unique antinode positions to handle overlaps
   - Antinodes from different frequency pairs may overlap
   - Multiple antenna pairs may create the same antinode

### Algorithm

```
For each frequency:
    For each pair of antennas (a1, a2) with that frequency:
        Part 1:
            Calculate antinode at 2*a1 - a2
            Calculate antinode at 2*a2 - a1
            Add to set if within grid bounds

        Part 2:
            dr = a2.row - a1.row
            dc = a2.col - a1.col
            Extend from a1 in direction (dr, dc) until out of bounds
            Extend from a1 in direction (-dr, -dc) until out of bounds
            Add all positions to set
```

### Complexity

- **Time**: O(F * N^2 * G) where F = number of frequencies, N = max antennas per frequency, G = grid dimension
  - Part 1: O(F * N^2) - constant work per pair
  - Part 2: O(F * N^2 * G) - may traverse entire grid diagonal per pair
- **Space**: O(G^2) for the antinode set (worst case: every grid cell is an antinode)

## Programming Techniques Highlighted

### Pair Generation
The problem requires iterating through all unique pairs (i, j) where i < j:
```python
for i in range(len(positions)):
    for j in range(i+1, len(positions)):
```

Or using Python's `itertools.combinations(positions, 2)`

### Vector Arithmetic
Antinode calculation uses simple vector math:
- Position as (row, col) tuple
- Direction vector: `(r2-r1, c2-c1)`
- Point reflection: `2*p1 - p2`

### Case Sensitivity Pitfall
A critical detail: 'A' and 'a' are **different** frequencies! Languages with case-insensitive default string handling (like CFML) need special handling.

## Language-Specific Notes

### Fast Performers (6-8ms)
- **C++, Go, C, ARM64**: All achieve ~7ms with minimal memory (~2-4MB)
- These benefit from efficient hash set implementations and low-level memory control
- The algorithm is simple enough that language overhead dominates over algorithmic complexity

### Scripting Languages (25-65ms)
- **Python** (27ms): Excellent for this problem with `defaultdict` and `itertools`
- **Perl** (13ms): Surprisingly fast, benefits from efficient hash operations
- **Common Lisp** (28ms): SBCL's compiler produces efficient code
- **Bash** (52ms): Unusually fast for Bash! The algorithm doesn't require nested loops with heavy computation

### JVM Languages (140-466ms)
- **Java** (138ms): JVM startup dominates; actual algorithm is fast
- **Clojure** (466ms): Functional immutable data structures add overhead

### ColdFusion Challenges
- **ColdFusion** (2,930ms): Requires case-sensitive key handling
- Default struct keys are case-insensitive, so 'A' and 'a' merge incorrectly
- Solution: Use arrays with `compare()` function or encode keys as ASCII codes

### ARM64 Assembly Notes
- Required careful register management around function calls
- Caller-saved registers (x0-x18) get clobbered by subroutine calls
- Solution: Save coordinates in callee-saved registers (x19-x28) or stack

## Benchmark Results

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Go          | 6.6          | 4.1         |
| C++         | 6.6          | 1.9         |
| C           | 7.3          | 1.9         |
| ARM64 asm   | 7.3          | 1.9         |
| Rust        | 8.5          | 1.9         |
| Zig         | 11.2         | 1.9         |
| Perl        | 13.3         | 4.6         |
| Python      | 26.8         | 15.0        |
| Common Lisp | 27.7         | 41.0        |
| Node.js     | 46.9         | 39.8        |
| Bash        | 51.9         | 6.8         |
| PHP         | 62.2         | 24.6        |
| Ruby        | 64.5         | 28.3        |
| Java        | 138.4        | 46.5        |
| Clojure     | 465.9        | 142.0       |
| ColdFusion  | 2,930.5      | 1,143.8     |

## Answers

- **Part 1**: 400
- **Part 2**: 1280
