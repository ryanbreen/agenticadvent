# Day 18: Lavaduct Lagoon

## Problem Summary

The Elves need to create a large lagoon to store lava. Given a dig plan that traces the perimeter of the lagoon, calculate how many cubic meters of lava it can hold (the total area including the trench boundary).

**Input format**: Each line contains a direction (R/D/L/U), distance, and a hex color code like `R 6 (#70c710)`.

## Part 1: Follow the Dig Plan

The digger follows simple instructions:
- **R** = right, **D** = down, **L** = left, **U** = up
- Each instruction specifies how many meters to dig in that direction

**Goal**: Calculate the total area (interior + trench boundary) of the lagoon.

## Part 2: Decode from Hex

The color codes actually contain the real instructions:
- First 5 hex digits = distance
- Last hex digit = direction (0=R, 1=D, 2=L, 3=U)

Example: `#70c710` → distance=461937, direction=R

**Goal**: Calculate the area using the hex-decoded instructions (much larger polygon).

## Algorithmic Approach

### Key Insight

This is a **polygon area problem** that can be solved analytically without simulating the entire grid. The key mathematical tools are:

1. **Shoelace Formula**: Calculates the signed area of a polygon given its vertices
2. **Pick's Theorem**: Relates interior points, boundary points, and area

### The Shoelace Formula

For a polygon with vertices (x₁,y₁), (x₂,y₂), ..., (xₙ,yₙ):

```
2A = |Σᵢ (xᵢ × yᵢ₊₁ - xᵢ₊₁ × yᵢ)|
```

This gives twice the signed area of the polygon.

### Pick's Theorem

For a polygon with integer vertex coordinates:

```
A = i + b/2 - 1
```

Where:
- A = area
- i = number of interior lattice points
- b = number of boundary lattice points (the perimeter)

### Combining the Formulas

We want: **Total = interior + boundary = i + b**

From Pick's theorem: `i = A - b/2 + 1`

Therefore: `Total = A + b/2 + 1`

Where A is the Shoelace area and b is the perimeter.

### Algorithm Steps

1. Parse instructions and trace vertices by following directions
2. Calculate the perimeter (sum of all distances)
3. Apply Shoelace formula to get polygon area
4. Apply Pick's theorem adjustment: `result = area + perimeter/2 + 1`

### Complexity

- **Time**: O(n) where n = number of instructions
- **Space**: O(n) for storing vertices

## Language Notes

- **Fast performers**: Zig, ARM64, Rust, C, C++, Go - simple arithmetic operations, minimal overhead
- **This problem is fast**: Unlike Day 17 (Dijkstra's algorithm), this is O(n) with simple arithmetic, so even slower languages finish quickly
- **Large numbers in Part 2**: The hex distances create coordinates in the millions, requiring 64-bit integers
  - PHP: Uses GMP for arbitrary precision
  - Bash: Uses `bc` for the Shoelace calculation
  - ColdFusion: Uses `precisionEvaluate()` for BigDecimal arithmetic

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 5.8          | 1.9         |
| ARM64       | 7.5          | 1.9         |
| Rust        | 7.7          | 1.9         |
| Go          | 8.8          | 1.9         |
| C++         | 8.9          | 1.9         |
| C           | 11.5         | 1.9         |
| Perl        | 14.7         | 5.1         |
| Python      | 34.7         | 16.0        |
| Common Lisp | 42.4         | 40.6        |
| Node.js     | 53.7         | 41.1        |
| Java        | 67.0         | 48.8        |
| Bash        | 67.6         | 6.9         |
| Ruby        | 77.2         | 28.2        |
| PHP         | 79.3         | 26.8        |
| Clojure     | 557.0        | 132.1       |
| ColdFusion  | 3,965.8      | 991.3       |

## Answers

- Part 1: **28911**
- Part 2: **77366737561114**
