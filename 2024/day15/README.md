# Day 15: Warehouse Woes

## Problem Summary

A malfunctioning robot is pushing boxes around a lanternfish warehouse. Given the warehouse map and a sequence of robot movement commands, simulate the robot's movements and calculate the final positions of all boxes.

### Part 1: Simple Box Pushing
The warehouse contains:
- `#` walls
- `O` boxes (single-cell)
- `@` robot
- `.` empty space

The robot follows movement commands (`<`, `>`, `^`, `v`). When the robot moves into a box, it pushes the entire chain of boxes in that direction if there's empty space at the end. If the chain would hit a wall, nothing moves.

Calculate the sum of GPS coordinates (100 * row + col) for all boxes after processing all moves.

### Part 2: Wide Boxes
The warehouse is now twice as wide:
- `#` becomes `##`
- `O` becomes `[]` (a wide box spanning two cells)
- `.` becomes `..`
- `@` becomes `@.`

Wide boxes can push multiple other wide boxes vertically when they overlap. For example, pushing up on `[]` might push two boxes above it if it overlaps with both. All boxes in a connected chain must be able to move, or none move.

## Algorithmic Approach

### Part 1: Linear Chain Pushing

Simple simulation with linear box chain detection:
1. Parse the grid and extract moves (ignoring newlines)
2. For each move, check the next cell:
   - Wall: don't move
   - Empty: move robot
   - Box: scan in the move direction to find end of box chain
     - If wall at end: don't move
     - If empty at end: place box there, move robot to first box position

**Complexity**: O(M * max(W, H)) where M = number of moves, W = width, H = height

### Part 2: Recursive Wide Box Pushing

Vertical movement becomes significantly more complex because:
- A wide box `[]` can overlap with two boxes above/below it
- Those boxes might each overlap with more boxes, creating a tree of dependencies

**Algorithm for vertical movement:**
1. `canMoveBoxVertical(box_left_col, row, dr)`: Recursively check if a box and all boxes it would push can move
   - Check both cells at (row+dr, box_left_col) and (row+dr, box_left_col+1)
   - If either is a wall: return false
   - If either contains part of a box, recursively check that box
   - All boxes in the tree must be movable

2. `collectBoxesVertical(box_left_col, row, dr, collected)`: Gather all boxes that need to move
   - Add current box to collected set
   - Recursively collect boxes this one would push

3. Move boxes in sorted order:
   - Sort by row descending (if moving down) or ascending (if moving up)
   - This prevents overwriting boxes before they're moved

**Horizontal movement** remains simple: scan for end of box chain, shift all cells.

**Complexity**: O(M * B) where B = number of boxes (worst case: all boxes in one vertical chain)

### Key Insight

The crucial realization for Part 2 is that vertical box pushing requires **tree traversal**, not linear scanning. A single `[]` box can cause a cascade affecting dozens of boxes if they're arranged in a pyramid shape.

## Data Structures Used

- **2D Grid**: Character array for the warehouse
- **Position tracking**: Robot coordinates (row, col)
- **Set/Hash for collection**: Track unique boxes during vertical push collection
- **Stack or recursion**: For tree traversal in Part 2 vertical movement

## Complexity

- **Time**: O(M * W) for Part 1, O(M * B) for Part 2 where M=moves, W=width, B=boxes
- **Space**: O(W * H) for grid, O(B) for box collection set in Part 2

## Programming Techniques Highlighted

- **Grid simulation**: Classic 2D array manipulation
- **Chain detection**: Linear scanning for connected elements
- **Recursive tree traversal**: Part 2's box dependency resolution
- **Topological ordering**: Moving boxes in correct sequence to avoid overwrites

## Language-Specific Notes

### Fast Performers
- **ARM64 Assembly** (6.2ms): Direct memory manipulation, minimal overhead
- **C++** (7.1ms): Efficient with vectors and direct indexing
- **Rust** (8.2ms): Zero-cost abstractions, good hash set performance

### Interesting Implementations
- **Python** (41.5ms): Clean recursive implementation, sets for box collection
- **Bash** (>300s): Associative arrays are extremely slow for this many operations; recursive functions add significant overhead

### Challenges by Language
- **ARM64 Assembly**: Implementing recursion with proper stack management for Part 2's tree traversal
- **Bash**: No native 2D arrays; associative array overhead is prohibitive for ~20,000+ moves
- **ColdFusion**: JVM startup overhead dominates for short-running solutions

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 6.2          | 1.4         |
| C++         | 7.1          | 1.9         |
| Rust        | 8.2          | 1.9         |
| Go          | 9.6          | 4.0         |
| C           | 9.9          | 2.0         |
| Zig         | 10.2         | 1.9         |
| Python      | 41.5         | 16.3        |
| Java        | 60.6         | 51.3        |
| Node.js     | 61.6         | 50.2        |
| Perl        | 63.2         | 8.4         |
| PHP         | 72.0         | 25.0        |
| Common Lisp | 73.4         | 58.9        |
| Ruby        | 107.9        | 29.1        |
| Clojure     | 523.4        | 190.9       |
| ColdFusion  | 4,555.9      | 1,092.1     |
| Bash        | >300,000     | TBD         |

## Answers

- Part 1: 1471826
- Part 2: 1457703
