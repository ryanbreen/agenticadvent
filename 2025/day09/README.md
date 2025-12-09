# Day 9: Movie Theater

## Problem Summary

The Elves are redecorating a movie theater by switching out tiles. Some tiles are **red**, and they want to find the largest rectangle that uses red tiles for two of its opposite corners.

**Input:** A list of 496 red tile coordinates (x, y) with values up to ~100,000

### Part 1: Largest Rectangle with Red Corners
Find the largest rectangle using any two red tiles as opposite corners. The area includes all tiles within the rectangle (inclusive of corners).

### Part 2: Rectangle Must Be Inside Polygon
The red tiles form a closed polygon in the order they appear in the input. Consecutive red tiles are connected by horizontal or vertical lines of **green** tiles, and the polygon's interior is also green. Find the largest rectangle with red corners where the **entire rectangle** is contained within the red/green region.

## Algorithmic Approach

### Part 1: Brute Force Pairs
With ~500 points, checking all pairs is O(n²) ≈ 123,000 comparisons - very manageable.

**Algorithm:**
1. Parse all red tile coordinates
2. For each pair of points (i, j):
   - Calculate rectangle area = (|x2-x1| + 1) * (|y2-y1| + 1)
   - Track maximum area found
3. Return maximum

**Complexity:** O(n²) time, O(n) space

### Part 2: Rectilinear Polygon Containment
The challenge is efficiently checking if a rectangle is entirely inside the polygon without iterating over every tile (which could be ~10 billion tiles given coordinate ranges up to 100,000).

**Key Insight:** For a rectilinear (axis-aligned) polygon, a rectangle is entirely inside if and only if:
1. No polygon edge crosses through the rectangle's interior
2. The rectangle's center is inside the polygon

**Algorithm:**
1. Build lists of horizontal and vertical edges from consecutive points
2. For each pair of red points:
   - Check if any vertical edge has x-coordinate strictly between rectangle's x bounds AND y-range overlapping
   - Check if any horizontal edge has y-coordinate strictly between rectangle's y bounds AND x-range overlapping
   - If no edges cross through, verify center point is inside using ray casting
3. Return maximum area among valid rectangles

**Ray Casting:** Cast a horizontal ray to the right from the center point. Count crossings with vertical edges. Odd count = inside.

**Complexity:** O(n² * E) where E is number of edges (~500). Total ≈ O(n³) but with small constants.

## Programming Techniques Highlighted

- **Computational Geometry**: Point-in-polygon testing, edge intersection
- **Ray Casting Algorithm**: Classic method for determining if a point is inside a polygon
- **Rectilinear Polygon Properties**: Axis-aligned edges simplify intersection tests
- **Optimization**: Avoiding O(area) checks by using edge-based containment tests

## Data Structures Used

- **Arrays/Lists**: Store points and edges
- **Hash Maps**: Index edges by x-coordinate (vertical) or y-coordinate (horizontal) for efficient lookup
- **Coordinate Compression**: Not needed since we work with edges, not individual tiles

## Computer Science Topics

This problem is an excellent introduction to:
1. **Computational geometry** - working with polygons and containment
2. **Ray casting** - fundamental algorithm for point-in-polygon tests
3. **Rectilinear polygons** - special case with simpler intersection math
4. **Optimization thinking** - avoiding brute force over large coordinate spaces

## Language Notes

### Fast Performers
- **C, C++, Rust**: Direct memory access, efficient loops
- **Go, Java**: Good JIT optimization for nested loops

### Moderate Performance
- **Node.js, Python**: Adequate for n² with n≈500
- **Ruby, PHP, Perl**: Slower but still reasonable

### Functional Languages
- **Clojure, Common Lisp**: Map/filter patterns work well here

## Benchmark Results

| Language | Runtime (ms) | Memory (MB) | Notes |
|----------|-------------|-------------|-------|
| Python | TBD | TBD | Reference implementation |
| Node.js | TBD | TBD | ES6+ implementation |
| C | TBD | TBD | |
| C++ | TBD | TBD | |
| Rust | TBD | TBD | |
| Go | TBD | TBD | |
| Java | TBD | TBD | |
| Ruby | TBD | TBD | |
| PHP | TBD | TBD | |
| Perl | TBD | TBD | |
| Bash | TBD | TBD | AWK-based |
| Clojure | TBD | TBD | |
| Common Lisp | TBD | TBD | SBCL |
| ColdFusion | TBD | TBD | |
| Zig | TBD | TBD | |
| ARM64/C | TBD | TBD | Optimized C |

## Answers

- **Part 1**: 4750297200
- **Part 2**: 1578115935
