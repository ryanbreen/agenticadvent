# Day 14: Restroom Redoubt

## Problem Summary

Robots patrol a bathroom lobby in predictable patterns on a toroidal grid (101x103 tiles). Each robot has an initial position and velocity, moving in straight lines with wraparound at edges.

## Part 1: Safety Factor

After simulating 100 seconds of robot movement, count robots in each quadrant (excluding the middle row and column), then multiply the four counts together.

**Key insight**: No step-by-step simulation needed. Since robots move linearly with wraparound, the position after N seconds can be computed directly:
```
new_x = (px + vx * N) mod WIDTH
new_y = (py + vy * N) mod HEIGHT
```

**Negative modulo handling**: Different languages handle negative modulo differently:
- Python/Ruby: `%` gives non-negative result for positive divisor
- C/Java/JavaScript: `%` can return negative, need `((n % m) + m) % m`
- Rust: Use `rem_euclid()` for Euclidean remainder
- Go/Java: Use `Math.floorMod()` or manual adjustment

## Part 2: Christmas Tree Detection

Find the first second where robots form a recognizable Christmas tree pattern.

**Key insight**: The Christmas tree has a border/outline made of robots arranged in horizontal lines. Detecting 20+ consecutive robots in a single row is sufficient to identify the tree frame.

**Algorithm**:
1. For each second from 1 to WIDTH*HEIGHT (maximum cycle length):
   - Compute all positions
   - Build a set of occupied positions
   - Scan each row for horizontal runs of consecutive robots
   - If any run has 20+ robots, return current second

**Why WIDTH*HEIGHT?**: The grid has period lcm(WIDTH, HEIGHT) = 101*103 = 10,403 (since they're coprime). The tree appears within this range.

## Algorithmic Approach

### Data Structures
- **Robot array**: Store (px, py, vx, vy) tuples
- **Position set**: Hash set for O(1) position lookup in Part 2
- **Grid array**: Alternative to set for Part 2 (mark occupied cells)

### Complexity
- **Part 1**: O(N) where N = number of robots (~500)
- **Part 2**: O(S * N * W) where S = seconds until tree (~7569), W = WIDTH
  - Each second: build set O(N), scan grid O(W * H)

## Programming Techniques

1. **Modular arithmetic**: Core to the solution - handling wraparound correctly
2. **Hash sets**: Fast position lookup for pattern detection
3. **Pattern recognition**: Heuristic-based detection (horizontal line threshold)
4. **Direct computation**: Skip simulation by using closed-form position formula

## Language-Specific Notes

### Fast Performers
- **C** (73ms): Heap-allocated grid, fast modulo operations
- **ARM64 asm** (167ms): Direct position computation, minimal overhead
- **Rust** (632ms): Idiomatic parsing with `filter_map`/`?`, `rem_euclid` for modulo
- **Go** (818ms): Good hash map performance

### Slow Performers
- **Bash** (154s): Each iteration creates new arrays/strings; external sort per iteration
- **Ruby** (36s): Set operations and range iteration overhead
- **ColdFusion** (18s): JVM startup + CFML interpretation overhead

### Notable Implementations
- **Zig**: Uses `@mod` for proper Euclidean modulo, `ArrayList.initCapacity` for preallocation
- **Clojure**: Functional approach with named map keys in `count-quadrants`
- **Common Lisp**: `mod` function naturally handles negative numbers correctly

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 73           | 1.9         |
| ARM64 asm   | 167          | 1.9         |
| Rust        | 632          | 1.5         |
| Go          | 818          | 26.8        |
| Java        | 1,239        | 1,250.8     |
| Zig         | 1,229        | 1.9         |
| Common Lisp | 1,725        | 91.1        |
| C++         | 2,535        | 166.6       |
| Python      | 3,907        | 15.9        |
| PHP         | 4,148        | 25.0        |
| Node.js     | 4,441        | 62.9        |
| Clojure     | 5,241        | 1,308.4     |
| Perl        | 8,251        | 5.0         |
| ColdFusion  | 17,956       | 1,069.3     |
| Ruby        | 36,334       | 28.6        |
| Bash        | 153,877      | 2.5         |

## Answers

- **Part 1**: 232589280
- **Part 2**: 7569
