# Day 16: The Floor Will Be Lava

## Problem Summary

A beam of light enters a cavern containing a contraption made of mirrors (`/`, `\`) and splitters (`|`, `-`). The beam bounces around the grid according to the rules of each component, energizing tiles as it passes through them.

**Part 1**: Count how many tiles get energized when the beam enters from the top-left corner heading right.

**Part 2**: Find the starting position and direction (from any edge) that maximizes the number of energized tiles.

## Algorithmic Approach

### Part 1: BFS Beam Simulation

Simulate the beam using BFS (Breadth-First Search):
- State: `(row, col, direction)` where direction is 0=right, 1=down, 2=left, 3=up
- At each cell, determine the next direction(s) based on the cell contents:
  - `.` (empty): continue in the same direction
  - `/` mirror: reflect (right→up, down→left, left→down, up→right)
  - `\` mirror: reflect (right→down, down→right, left→up, up→left)
  - `|` splitter: if horizontal beam, split to up and down; otherwise pass through
  - `-` splitter: if vertical beam, split to left and right; otherwise pass through
- Track visited `(row, col, direction)` states to detect cycles
- Count unique `(row, col)` positions that were visited

### Part 2: Brute Force Edge Search

Try all possible starting configurations from each edge:
- All positions in top row, heading down
- All positions in bottom row, heading up
- All positions in left column, heading right
- All positions in right column, heading left

Return the maximum energized count across all configurations.

### Key Insight

The critical performance optimization is using **O(1) array-based lookups** for the visited state instead of hash sets:
- Use a 3D boolean array: `visited[row][col][direction]`
- This gives O(1) lookup/insert vs O(1) average/O(n) worst case for hash sets
- The improvement is dramatic: C++ went from 745ms to 31ms after this change

### Complexity

- **Time**: O(E × R × C × 4) where E is number of edge positions, R is rows, C is cols
  - Part 2 has E = 2(R + C) edge starting positions
  - Each simulation visits at most R × C × 4 states
- **Space**: O(R × C × 4) for the visited array

## Programming Techniques Highlighted

- **BFS (Breadth-First Search)**: Traversing all reachable states
- **State tracking with cycles**: Detecting when beams revisit the same state
- **Direction encoding**: Using integers (0-3) for efficient direction manipulation
- **Mirror/splitter reflection logic**: Using lookup tables for direction mappings

## Language-Specific Notes

- **Fast performers (C, ARM64, C++, Rust, Zig)**: Array-based visited tracking gives O(1) state lookups, running in ~20-40ms
- **Go**: Slightly slower than other systems languages due to GC overhead but still fast at ~50ms
- **Common Lisp**: Surprisingly fast at ~400ms with efficient hash tables
- **JVM languages (Java, Clojure)**: Higher overhead, but Clojure's startup dominates
- **Scripting languages (Python, Ruby, PHP, Perl)**: Hash-based sets work but are slower (~1-6 seconds)
- **Bash**: Extremely slow (~2 minutes) due to process spawning overhead for each cell lookup

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 20.0         | 1.9         |
| ARM64       | 28.4         | 1.9         |
| C++         | 31.0         | 1.9         |
| Rust        | 31.8         | 1.9         |
| Zig         | 42.5         | 1.9         |
| Go          | 50.8         | 11.5        |
| Common Lisp | 406.3        | 106.7       |
| Java        | 735.5        | 571.9       |
| PHP         | 1064.4       | 27.9        |
| Python      | 1222.3       | 20.2        |
| Node.js     | 1257.9       | 111.3       |
| Clojure     | 2841.2       | 1313.2      |
| Perl        | 3389.4       | 8.2         |
| Ruby        | 6062.9       | 34.0        |
| ColdFusion  | 8601.7       | 1000.1      |
| Bash        | ~121000      | 6.8         |

## Answers

- Part 1: **7307**
- Part 2: **7635**
