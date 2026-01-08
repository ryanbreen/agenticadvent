# Day 1: Calorie Counting

## Problem Summary

The Elves are on an expedition to find magical star fruit. Each Elf is carrying food with various calorie counts. The input represents each Elf's inventory as a group of numbers (one calorie value per line), with blank lines separating different Elves.

**Part 1**: Find the Elf carrying the most total calories.

**Part 2**: Find the total calories carried by the top three Elves.

## Input Format

Numbers grouped by blank lines. Each group represents one Elf's food inventory:

```
1000
2000
3000

4000

5000
6000
```

The above represents 3 Elves carrying 6000, 4000, and 11000 calories respectively.

## Algorithmic Approach

### Part 1: Maximum Group Sum

The algorithm is straightforward:
1. Parse the input into groups separated by blank lines
2. Sum the values within each group
3. Return the maximum sum

This is a simple linear scan with no complex data structures needed.

### Part 2: Top 3 Sums

Two common approaches:
1. **Sort-based**: Sort all group sums in descending order, take the top 3
2. **Selection-based**: Maintain only the top 3 values while scanning (more efficient for large inputs)

Most implementations use the sort-based approach since the number of Elves is small (~250).

### Time Complexity

- **Parsing**: O(n) where n is the total number of input lines
- **Part 1**: O(e) where e is the number of Elves (groups)
- **Part 2**: O(e log e) for sorting, or O(e) for selection-based

### Space Complexity

- O(e) to store all Elf totals

## Key Insight

The problem is essentially a "split by delimiter, aggregate, then find max/top-k" pattern - a very common parsing exercise. The blank line as a group separator is the main parsing challenge.

## Programming Techniques Highlighted

- **String splitting**: Splitting input on double newlines (`\n\n`)
- **Grouping/aggregation**: Summing values within groups
- **Sorting**: Finding top-k elements from a collection
- **File I/O**: Reading and parsing text input

## Language-Specific Notes

### Fast Performers (< 10ms)
- **ARM64 Assembly**: Direct syscalls, manual string parsing, minimal overhead
- **C**: Simple arrays, `atoi()` for parsing, `qsort()` for sorting
- **C++**: Similar to C but with `std::vector` and `std::sort`
- **Zig**: Modern systems programming with explicit memory management
- **Rust**: Zero-cost abstractions, iterator-based functional style

### Scripting Languages (10-60ms)
- **Perl**: Excellent text processing with paragraph mode (`$/=""`)
- **Bash**: Uses `awk` for efficient text processing; pure Bash would be much slower
- **Python**: Clean, readable with `split()` and `sum()`
- **Common Lisp**: Functional style with `reduce` and `mapcar`

### Interpreted/VM Languages (40-60ms)
- **Node.js**: ES module syntax, `split()` and `reduce()`
- **Java**: JVM startup overhead dominates for this simple problem
- **Go**: Includes compilation time when using `go run`
- **PHP**: Standard scripting with `array_sum()` and `rsort()`
- **Ruby**: Expressive with method chaining

### Slow Starters (300-2500ms)
- **Clojure**: JVM startup plus Clojure runtime initialization
- **ColdFusion**: Heavy JVM-based runtime (Lucee/CommandBox)

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 4.9          | 1.9         |
| C           | 4.9          | 1.8         |
| C++         | 5.2          | 1.9         |
| Zig         | 5.5          | 1.9         |
| Rust        | 5.9          | 1.9         |
| Perl        | 13.9         | 6.1         |
| Bash        | 19.6         | 6.7         |
| Common Lisp | 21.1         | 38.4        |
| Python      | 22.3         | 14.8        |
| Node.js     | 43.1         | 37.5        |
| Java        | 44.5         | 47.6        |
| Go          | 47.1         | 27.9        |
| PHP         | 50.3         | 25.5        |
| Ruby        | 54.4         | 28.1        |
| Clojure     | 377.7        | 128.7       |
| ColdFusion  | 2,376.8      | 1,136.5     |

## Answers

- Part 1: 69501
- Part 2: 202346
