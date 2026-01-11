# Day 4: Giant Squid

## Problem Summary

A giant squid wants to play bingo! You have a list of drawn numbers and multiple 5x5 bingo boards.

**Part 1**: Find the first board to win (complete row or column). Calculate score as: sum of unmarked numbers × winning number.

**Part 2**: Find the last board to win. Calculate the same score for that board.

## Algorithmic Approach

### Part 1 Algorithm

1. Parse drawn numbers and boards from input
2. Create a "marked" boolean grid for each board
3. For each drawn number:
   - Mark that number on all boards where it appears
   - Check each board for a winning condition (complete row or column)
   - Return score of first winner

**Time Complexity**: O(n × b × 25) where n = drawn numbers, b = boards
**Space Complexity**: O(b × 25) for marked grids

### Part 2 Algorithm

1. Same setup as Part 1
2. Track which boards have already won
3. Continue through all numbers, skipping boards that already won
4. Track the score of the last board to win

**Time Complexity**: Same as Part 1
**Space Complexity**: O(b × 25) plus O(b) for won tracking

### Key Insight

The problem is essentially a simulation with early termination (Part 1) or late termination (Part 2). The main optimization is using a lookup structure to quickly find where each number appears on each board.

## Programming Techniques Highlighted

- **2D Array Manipulation**: Working with 5x5 grids
- **Simulation**: Stepping through the bingo game state
- **State Tracking**: Maintaining marked positions across multiple boards
- **Early/Late Termination**: Stopping at first winner vs tracking last winner

## Data Structures Used

- 2D arrays for boards (5x5 integer grids)
- 2D boolean arrays for marked positions
- Array of drawn numbers
- Boolean array for tracking won boards (Part 2)

## Language-Specific Notes

- **Compiled languages (ARM64, Zig, C++, C, Rust)**: 5-8ms, efficient array handling
- **Go**: 8.9ms, slightly more overhead for 2D slice management
- **Common Lisp**: 25ms, good performance with vectors
- **Perl**: 27ms, efficient array handling
- **Node.js/Python/Java**: 45-50ms range, clean implementation
- **PHP/Ruby**: 70-97ms, higher interpreted overhead
- **Clojure**: 520ms, functional approach with JVM startup
- **Bash**: 1.1s - nested loops are slow in shell
- **ColdFusion**: 2.4s with CommandBox overhead

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 5.6          | 1.9         |
| Zig         | 5.8          | 1.9         |
| C++         | 6.4          | 1.9         |
| C           | 7.3          | 1.9         |
| Rust        | 7.8          | 1.9         |
| Go          | 8.9          | 4.4         |
| Common Lisp | 25.3         | 40.3        |
| Perl        | 27.0         | 8.6         |
| Node.js     | 45.4         | 44.4        |
| Python      | 46.5         | 15.0        |
| Java        | 50.5         | 52.6        |
| Ruby        | 71.6         | 28.1        |
| PHP         | 96.5         | 26.1        |
| Clojure     | 519.6        | 143.9       |
| Bash        | 1,058.3      | 13.7        |
| ColdFusion  | 2,425.4      | 1,035.5     |

## Answers

- **Part 1**: 87456
- **Part 2**: 15561
