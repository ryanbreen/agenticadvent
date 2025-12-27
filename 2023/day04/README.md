# Day 4: Scratchcards

## Problem Summary

The Elf has a pile of scratchcards and needs help determining their value. Each card has two lists of numbers separated by a vertical bar (`|`): winning numbers on the left and "your numbers" on the right.

**Input Format:**
```
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
```

## Part 1: Point Calculation

For each card, find how many of "your numbers" appear in the winning numbers. The first match is worth 1 point, and each additional match doubles the card's value.

**Scoring formula:** If a card has `n` matches where `n > 0`, the score is `2^(n-1)`. Cards with no matches score 0.

**Answer: 25174**

## Part 2: Cascading Card Copies

Instead of points, matching numbers win copies of subsequent cards. A card with `n` matches wins one copy each of the next `n` cards. These copies are processed the same way, potentially winning more copies.

**Example cascade:**
- Card 1 has 4 matches → win copies of cards 2, 3, 4, 5
- Card 2 has 2 matches → each copy wins copies of cards 3 and 4
- Process continues until no more cards are won

**Answer: 6420979**

## Algorithmic Approach

### Part 1: Set Intersection

1. Parse each card into two collections: winning numbers and your numbers
2. Use a set/hash structure for winning numbers for O(1) lookup
3. Count how many of "your numbers" appear in the winning set
4. Calculate score: `2^(matches-1)` if matches > 0, else 0
5. Sum all scores

**Time Complexity:** O(n × m) where n = number of cards, m = numbers per card
**Space Complexity:** O(m) for the winning number set

### Part 2: Dynamic Programming / Forward Propagation

The key insight is that we don't need to simulate the cascade - we can process cards in order and propagate copy counts forward:

1. Pre-compute match counts for all cards
2. Initialize a `copies` array with 1 for each card (the originals)
3. For each card `i` with `m` matches:
   - Add `copies[i]` to cards `i+1` through `i+m`
4. Sum all values in the `copies` array

**Time Complexity:** O(n × max_matches) ≈ O(n²) worst case, but typically much better
**Space Complexity:** O(n) for the copies and matches arrays

### Key Insight

The forward propagation approach works because:
- Cards only affect cards after them (no cycles)
- Order is deterministic - card i is fully processed before cards i+1..n
- Multiplication of copies happens through addition: if card i has 5 copies and wins copies of card j, we add 5 to card j's count

## Programming Techniques Highlighted

### Data Structures
- **Sets/Hash Maps:** Essential for O(1) winning number lookup
- **Arrays:** For tracking match counts and copy counts
- **Bit manipulation:** `1 << (n-1)` is faster than `pow(2, n-1)` for integer powers of 2

### Parsing Challenges
- Splitting on multiple delimiters (`:` and `|`)
- Handling variable whitespace in number lists
- Some numbers are single-digit with extra padding spaces

### Patterns Used
- **Set intersection** for finding common elements
- **Forward propagation** instead of recursive simulation
- **Pre-computation** of match counts for efficiency

## Language-Specific Notes

### Fast Performers (< 15 ms)
- **Go, Rust, C++, C, ARM64, Zig:** Low-level languages with minimal overhead shine here. The problem is I/O and parsing bound more than computation bound.
- **Go:** Optimized with compiled binary and efficient map operations
- **Rust:** Memory-safe with zero-cost abstractions
- **C:** Simple data structures and direct memory access
- **ARM64:** Hand-optimized assembly using bitmap for winning numbers

### Mid-Tier (15-80 ms)
- **Perl:** Surprisingly fast with hash-based set operations
- **Python:** Good balance of readability and performance with set operations
- **Bash:** Much faster than Day 1-3 due to simpler parsing requirements
- **Node.js, Java, Ruby, PHP:** JIT compilation overhead but good runtime performance

### Slower Performers (> 400 ms)
- **Clojure:** JVM startup time dominates; functional style adds overhead
- **ColdFusion:** Interpreted nature and JVM startup; still produces correct results

### Notable Implementation Details
- **Bash:** Uses string pattern matching (`*" $num "*`) for set membership instead of arrays
- **ARM64:** Uses a 128-byte bitmap for O(1) set operations with bit manipulation
- **Common Lisp:** Uses `member` for list-based set membership (could be optimized with hash tables)
- **Clojure:** Leverages `clojure.set/intersection` for idiomatic set operations

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Go          | 5.4          | 4.4         |
| Rust        | 5.4          | 1.9         |
| C++         | 6.0          | 1.9         |
| C           | 6.6          | 1.9         |
| ARM64 asm   | 6.8          | 1.9         |
| Zig         | 10.2         | 1.9         |
| Perl        | 16.3         | 6.6         |
| Lisp        | 24.7         | 40.3        |
| Python      | 30.4         | 16.2        |
| Bash        | 30.7         | 7.0         |
| Node.js     | 45.1         | 42.4        |
| PHP         | 49.6         | 26.2        |
| Ruby        | 56.0         | 28.9        |
| Java        | 56.5         | 55.4        |
| Clojure     | 408.6        | 137.6       |
| ColdFusion  | 2,497.6      | 1,162.2     |

## Answers

- **Part 1:** 25174
- **Part 2:** 6420979
