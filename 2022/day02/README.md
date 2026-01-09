# Day 2: Rock Paper Scissors

## Problem Summary

The Elves are playing Rock Paper Scissors to determine tent placement. You receive an encrypted strategy guide with two columns: the opponent's move (A/B/C = Rock/Paper/Scissors) and a second column that requires different interpretation for each part.

**Part 1**: The second column (X/Y/Z) represents what you should play (Rock/Paper/Scissors). Calculate your total score.

**Part 2**: The second column actually represents the desired outcome (X=lose, Y=draw, Z=win). Figure out what to play and calculate your total score.

## Input Format

Each line contains two characters separated by a space:
```
A Y
B X
C Z
```

## Scoring

- **Shape score**: Rock=1, Paper=2, Scissors=3
- **Outcome score**: Loss=0, Draw=3, Win=6
- **Total score** = shape score + outcome score for each round

## Algorithmic Approach

### Part 1: Direct Lookup

The algorithm uses a lookup table for all 9 possible combinations (3 opponent moves x 3 player moves):

1. Parse each line to extract opponent move and player move
2. Look up outcome score from a 3x3 table
3. Add shape score (X=1, Y=2, Z=3)
4. Sum all rounds

### Part 2: Reverse Lookup

Instead of looking up the outcome, we look up what shape to play:

1. Parse opponent move and desired outcome
2. Look up required shape from a 3x3 table (opponent x outcome -> shape)
3. Add outcome score (X=0, Y=3, Z=6)
4. Sum all rounds

### Mathematical Approach

Some implementations use modular arithmetic instead of lookup tables:

- Rock=0, Paper=1, Scissors=2
- Win condition: `(player - opponent + 3) % 3 == 1`
- Draw condition: `player == opponent`
- To win: play `(opponent + 1) % 3`
- To lose: play `(opponent + 2) % 3`

### Time Complexity

- O(n) where n is the number of rounds
- Each round is O(1) with lookup tables

### Space Complexity

- O(1) - only fixed-size lookup tables needed

## Key Insight

The problem is essentially a lookup table exercise. All possible input combinations are known upfront, so precomputing the scores for each combination makes the solution trivial.

## Programming Techniques Highlighted

- **Lookup tables**: Hash maps or 2D arrays for O(1) score retrieval
- **Modular arithmetic**: Alternative approach for computing outcomes
- **Character arithmetic**: Converting 'A'/'X' to indices (0-2)
- **Pattern matching**: Useful in functional languages

## Language-Specific Notes

### Fast Performers (< 7ms)
- **C, ARM64, Zig, Rust, C++**: All essentially tied at ~5-6ms
- Dominated by I/O overhead; actual computation is negligible
- Lookup tables provide constant-time scoring

### Shell/Scripting (15-30ms)
- **Bash**: Uses `awk` for efficient text processing - surprisingly fast!
- **Perl**: Excellent text processing with hash lookups
- **Python**: Clean dictionary-based lookup tables

### VM Languages (45-80ms)
- **Common Lisp, Node.js, Java**: JIT overhead for short programs
- **Ruby, PHP, Go**: Interpretation/compilation overhead
- Go includes `go run` compilation time

### Heavy Runtimes (400-3000ms)
- **Clojure**: JVM + Clojure runtime startup dominates
- **ColdFusion**: Heavy enterprise runtime (Lucee/CommandBox)

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.5          | 1.9         |
| ARM64 asm   | 5.5          | 1.9         |
| Zig         | 5.7          | 1.9         |
| Rust        | 6.3          | 1.9         |
| C++         | 6.3          | 1.9         |
| Bash        | 17.2         | 6.7         |
| Perl        | 18.4         | 6.5         |
| Python      | 26.2         | 15.1        |
| Common Lisp | 46.0         | 39.3        |
| Node.js     | 49.2         | 40.9        |
| Java        | 55.0         | 50.1        |
| Ruby        | 66.0         | 28.5        |
| PHP         | 74.5         | 26.5        |
| Go          | 78.0         | 27.4        |
| Clojure     | 437.1        | 129.8       |
| ColdFusion  | 2,941.2      | 1,182.6     |

## Answers

- Part 1: 13446
- Part 2: 13509
