# Day 22: Monkey Map

## Problem Summary

Navigate a strangely-shaped map following path instructions. The map wraps around in unusual ways.

**Part 1**: Navigate the map with 2D flat wrapping - moving off one edge wraps to the opposite edge.

**Part 2**: Treat the map as the net of a cube - edges connect in 3D space, requiring complex wrapping logic.

## Input Format

Two sections separated by blank line:
1. Map grid with `.` (open), `#` (wall), and space (off-map)
2. Path instructions: alternating numbers (move forward) and L/R (turn)

## Algorithmic Approach

### Part 1: Flat Wrapping

Simple simulation with 2D wraparound:
1. Track position (row, col) and facing direction (0-3 for R/D/L/U)
2. For each move instruction, step forward one at a time
3. When going off the edge or hitting space, wrap to opposite edge on same row/column
4. Stop if hitting a wall after wrapping

### Part 2: Cube Wrapping

The map is a cube net. When leaving one face, you enter an adjacent face with potentially rotated orientation.

**Cube face layout for typical input:**
```
  12
  3
 45
 6
```

Each edge transition requires:
1. Identify which face and edge you're leaving
2. Map to destination face and edge
3. Transform local coordinates
4. Update facing direction

### Password Calculation

`password = 1000 * (row+1) + 4 * (col+1) + facing`

### Complexity

- **Time**: O(P) where P = total steps in path
- **Space**: O(H*W) for grid storage

## Programming Techniques Highlighted

- **State machine**: Track position and direction
- **Modular arithmetic**: Wraparound calculations
- **Coordinate transformation**: Cube face edge mappings
- **Hardcoded geometry**: Face adjacency relationships specific to input layout

## Language-Specific Notes

### Performance Characteristics

This problem is simulation-heavy with many position lookups.

**Very fast (< 10ms)**:
- **Zig, C, Rust, C++**: ~7ms - efficient array access

**Fast (10-50ms)**:
- **Perl, Python, Common Lisp**: 30-35ms - good string/array handling

**Moderate (50-100ms)**:
- **Bash, Node.js, PHP, Java, Ruby, Go**: 55-86ms - runtime overhead

**Slow (> 100ms)**:
- **Clojure**: 510.8ms - immutable data structure overhead
- **ColdFusion**: 2,492.3ms - interpreter overhead

### Implementation Notes

- Part 2 cube wrapping is input-specific - different layouts need different mappings
- The Bash solution uses AWK for performance
- All compiled languages achieve similar performance

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 6.8          | 1.9         |
| C           | 7.2          | 1.9         |
| Rust        | 7.2          | 2.0         |
| C++         | 7.2          | 1.9         |
| Perl        | 30.5         | 6.6         |
| Python      | 32.3         | 15.7        |
| Common Lisp | 34.3         | 47.0        |
| Bash        | 55.7         | 6.8         |
| Node.js     | 69.2         | 50.2        |
| PHP         | 71.5         | 25.9        |
| Java        | 71.6         | 48.5        |
| Ruby        | 77.0         | 28.7        |
| Go          | 85.9         | 27.3        |
| Clojure     | 510.8        | 202.0       |
| ColdFusion  | 2,492.3      | 1,061.6     |

## Answers

- Part 1: 60362
- Part 2: 74288
