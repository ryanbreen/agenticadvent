# Day 11: Cosmic Expansion

## Problem Summary

The puzzle presents a galaxy observation image where you need to calculate the sum of Manhattan distances between all pairs of galaxies. The twist is that empty rows and columns in space have "expanded" - in Part 1 they count as 2 units instead of 1, and in Part 2 they count as 1,000,000 units.

### Input Format
- A 2D grid of characters
- `.` represents empty space
- `#` represents a galaxy

### Part 1: Expansion Factor 2
Calculate the sum of shortest path lengths (Manhattan distances) between all pairs of galaxies, where empty rows and columns count double.

### Part 2: Expansion Factor 1,000,000
Same calculation, but each empty row/column now represents 1 million units of distance instead of just 2.

## Algorithmic Approach

### Key Insight
The crucial insight is that we **don't need to actually expand the grid**. Instead of creating a much larger grid (which would be impossible for Part 2's million-fold expansion), we can:

1. Keep track of which rows and columns are empty
2. Calculate the base Manhattan distance between galaxy pairs
3. Add extra distance for each empty row/column crossed

For an expansion factor `f`, each empty row/column adds `(f - 1)` extra units to the distance (since the base distance already counts 1 unit).

### Algorithm Steps

1. **Parse the grid**: Find all galaxy positions (`#` characters) and store them as (row, col) pairs.

2. **Identify empty rows/columns**:
   - An empty row contains no `#` characters
   - An empty column has no `#` at that position in any row

3. **Calculate pairwise distances**:
   For each unique pair of galaxies (r1, c1) and (r2, c2):
   - Base distance = `|r2 - r1| + |c2 - c1|`
   - Count empty rows between r1 and r2: `empty_rows_crossed`
   - Count empty columns between c1 and c2: `empty_cols_crossed`
   - Expanded distance = base_distance + (empty_rows_crossed + empty_cols_crossed) × (expansion_factor - 1)

4. **Sum all pairwise distances**

### Data Structures Used

- **Array/List**: Store galaxy positions as (row, col) tuples
- **Set/Hash Map**: Store empty row and column indices for O(1) lookup
- **Prefix sums** (optimization): Pre-compute cumulative count of empty rows/columns to enable O(1) queries for how many empty rows exist in any range

### Complexity Analysis

Let:
- `N` = number of rows
- `M` = number of columns
- `G` = number of galaxies

**Time Complexity**: O(N×M + G²×max(N,M))
- O(N×M) to scan the grid and find galaxies/empty rows/columns
- O(G²) pairs of galaxies
- For each pair, O(max(N,M)) to count empty rows/columns in range (or O(1) with prefix sums)

**Space Complexity**: O(N + M + G)
- O(G) for galaxy positions
- O(N) for empty rows tracking
- O(M) for empty columns tracking

With prefix sum optimization, each pair calculation is O(1), giving O(G²) total.

## Language-Specific Notes

### Fast Performers
- **C (13.1 ms)**: Direct array indexing and minimal overhead. Simple loops are very efficient.
- **ARM64 Assembly (22.1 ms)**: Hand-optimized loops but the O(G²) algorithm still dominates.
- **Zig (42.9 ms)**: Good balance of safety and performance with zero-cost abstractions.

### Mid-Tier Performers
- **C++/Rust/Java/Lisp/Node.js** (70-135 ms): All reasonable performance for an O(G²) algorithm. Different trade-offs between set lookup costs and iteration patterns.

### Slower Languages
- **Python (429.5 ms)**: The nested loops and set membership checks add overhead, but still very manageable.
- **Ruby (1,169.9 ms)**: Object overhead and dynamic dispatch slow things down.
- **Bash (1,615.8 ms)**: The optimized version using prefix sums and single bc call makes this surprisingly competitive.

### Special Handling Required

**Large Numbers (Part 2)**:
- Part 2 results reach ~857 billion, requiring 64-bit integers
- **Bash**: Uses `bc` for arbitrary precision arithmetic
- **PHP**: Native integers handle this fine (64-bit)
- **All compiled languages**: Use `int64_t`, `long long`, or equivalent

**ColdFusion Note**:
- Uses `##` to escape the `#` character in string comparisons since `#` is used for variable interpolation

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 13.1         | 1.8         |
| ARM64 asm   | 22.1         | 1.9         |
| Zig         | 42.9         | 1.9         |
| C++         | 73.5         | 1.9         |
| Rust        | 109.2        | 1.9         |
| Java        | 125.6        | 54.9        |
| Lisp        | 131.0        | 40.5        |
| Node.js     | 133.9        | 49.3        |
| PHP         | 163.6        | 25.9        |
| Go          | 182.6        | 56.7        |
| Python      | 429.5        | 15.0        |
| Perl        | 789.4        | 4.6         |
| Clojure     | 808.4        | 454.0       |
| Ruby        | 1,169.9      | 28.4        |
| Bash        | 1,615.8      | 6.9         |
| ColdFusion  | 4,278.8      | 1,066.3     |

## Answers

- **Part 1**: 9565386
- **Part 2**: 857986849428
