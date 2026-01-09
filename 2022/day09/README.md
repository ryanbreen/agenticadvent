# Day 9: Rope Bridge

## Problem Summary

Simulate a rope with knots moving on a 2D grid. The head moves according to directions, and each subsequent knot follows the one before it according to specific physics rules.

**Part 1**: Simulate a 2-knot rope (head + tail). Count unique positions visited by the tail.

**Part 2**: Simulate a 10-knot rope. Count unique positions visited by the last knot.

## Input Format

Movement instructions as direction and count:
```
R 4
U 4
L 3
D 1
R 4
```

## Algorithmic Approach

### Rope Physics

For each knot following another:
1. If within 1 step in both x and y (adjacent/overlapping), stay put
2. Otherwise, move one step toward the leader using `sign(delta)` for each axis
   - This handles both cardinal (straight) and diagonal movement

### Algorithm

```
for each move (direction, count):
    for step in range(count):
        move head in direction
        for each subsequent knot:
            if not adjacent to previous:
                move toward previous using sign(dx), sign(dy)
        record tail position
return count of unique tail positions
```

### Data Structures

- **Knots**: Array of (x, y) coordinates
- **Visited**: Set of (x, y) tuples (or hash table with string keys)

### Time Complexity

- O(n × m × k) where n = number of moves, m = average count per move, k = rope length
- Effectively O(total_steps × rope_length)

### Space Complexity

- O(k) for knot positions
- O(v) for visited set where v = unique visited positions

## Programming Techniques Highlighted

- **Sign Function**: `sign(x) = -1 if x < 0 else (1 if x > 0 else 0)`
- **Set/Hash Table**: For tracking unique positions
- **Simulation**: Step-by-step state tracking
- **Coordinate Geometry**: Manhattan and Chebyshev distance concepts

## Language-Specific Notes

### Fast Implementations (6-7ms)
- **C, Zig, Rust, ARM64, C++**: Direct simulation with efficient set/hash implementations
- C uses a 2D boolean grid, ARM64 uses a custom hash table

### Scripting Languages (35-76ms)
- **Common Lisp**: Fast hash table operations (34.9ms)
- **Python**: Tuple-based set operations (48.6ms)
- **Node.js/Java**: Map/Set with string keys (54-57ms)
- **Go/Perl/PHP**: Hash map operations (72-76ms)

### Slow Languages (102ms-57s)
- **Ruby**: Relatively slower set operations (102ms)
- **Clojure**: Immutable set updates add overhead (462ms)
- **ColdFusion**: Struct-based set tracking (2.8s)
- **Bash**: Associative arrays and shell overhead (57s)

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.9          | 1.9         |
| Zig         | 6.1          | 1.9         |
| Rust        | 6.2          | 1.9         |
| ARM64 asm   | 6.5          | 2.3         |
| C++         | 7.4          | 1.9         |
| Common Lisp | 34.9         | 44.4        |
| Python      | 48.6         | 15.8        |
| Node.js     | 54.0         | 47.1        |
| Java        | 57.2         | 56.3        |
| Go          | 72.0         | 27.9        |
| Perl        | 74.1         | 6.8         |
| PHP         | 76.2         | 26.2        |
| Ruby        | 101.8        | 28.6        |
| Clojure     | 461.6        | 182.9       |
| ColdFusion  | 2,768.4      | 1,087.6     |
| Bash        | 57,188       | 6.7         |

## Answers

- Part 1: 6209
- Part 2: 2460
