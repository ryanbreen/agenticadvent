# Day 20: Race Condition

## Problem Summary

Programs race through a maze from Start (S) to End (E), normally following the single valid path. However, they can "cheat" once per race by disabling collision detection for a limited time, allowing them to pass through walls.

### Part 1: 2-Picosecond Cheats
Count how many cheats (up to 2 picoseconds of phasing through walls) would save at least 100 picoseconds.

### Part 2: 20-Picosecond Cheats
Same problem but cheats can last up to 20 picoseconds, dramatically increasing the number of possible shortcuts.

## Algorithmic Approach

### Key Insight
Since there's only one valid path from S to E, we can pre-compute the distance from start for every track cell. A "cheat" is simply teleporting from one track position to another via Manhattan distance through walls.

### Algorithm
1. **BFS Path Tracing**: Trace the single path from start to end, recording `distance[pos]` for each track cell
2. **Cheat Enumeration**: For each pair of track positions (p1, p2):
   - Calculate Manhattan distance (the "cheat cost" in picoseconds)
   - If cheat_cost <= max_cheat_time:
     - savings = distance[p2] - distance[p1] - cheat_cost
     - If savings >= 100, count it as a valid cheat

### Complexity
- **Time**: O(N^2) where N is the number of track cells (~9,500 in the input)
- **Space**: O(N) for the distance map

This results in approximately 90 million pair comparisons for Part 2, which explains why scripting languages are significantly slower.

## Data Structures Used
- **Grid**: 2D array/vector for the maze
- **Distance Map**: Hash map or dense array mapping positions to distances from start
- **BFS Queue**: For path tracing

## Programming Techniques Highlighted
- BFS for single-source shortest paths
- Manhattan distance calculation
- O(N^2) enumeration with early pruning
- Dense array optimization for distance lookups

## Language Notes

### Fast performers
- **Zig** (58ms): Exceptional performance with simple loops and arrays
- **C++** (67ms): Fast hash maps and tight loops
- **C** (74ms): Raw array performance
- **Rust** (112ms): Safe but slightly slower due to bounds checking

### Notable implementations
- **ARM64 Assembly** (207ms): Direct syscalls and register-heavy computation
- **Clojure** (693ms): Uses Java int-arrays for the inner loop to achieve reasonable performance
- **Bash** (85s): Uses AWK for the heavy computation; pure Bash would be impractical

### Performance challenges
This problem exposes the O(N^2) inner loop performance:
- Scripting languages (Python, Ruby, Perl) take 9-28 seconds
- ColdFusion is particularly slow at ~2 minutes due to JVM overhead and interpreted execution

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 58.7         | 1.9         |
| C++         | 67.1         | 2.2         |
| C           | 73.7         | 1.9         |
| Rust        | 112.1        | 3.4         |
| ARM64       | 206.7        | 1.9         |
| Go          | 261.2        | 27.8        |
| Java        | 462.7        | 57.4        |
| Clojure     | 693.0        | 182.7       |
| Node.js     | 1089.2       | 60.3        |
| Lisp        | 2326.0       | 45.8        |
| PHP         | 4255.8       | 29.3        |
| Python      | 9027.6       | 16.7        |
| Ruby        | 13786.0      | 31.0        |
| Perl        | 28291.0      | 10.2        |
| Bash        | 84810.1      | 6.5         |
| ColdFusion  | 113057.8     | 1047.4      |

## Answers
- Part 1: 1375
- Part 2: 983054
