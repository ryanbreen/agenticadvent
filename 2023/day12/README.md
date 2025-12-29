# Day 12: Hot Springs

## Problem Summary

You're at the hot springs helping repair damaged condition records of spring arrangements. Each row contains a pattern of springs: operational (`.`), damaged (`#`), or unknown (`?`), followed by a list of contiguous damaged spring group sizes.

Your task is to count how many valid arrangements of springs fit the given criteria.

**Input Format:** Lines like `???.### 1,1,3` where the pattern shows spring conditions and the numbers indicate sizes of contiguous damaged spring groups.

## Part 1: Count Valid Arrangements

For each row, determine how many different arrangements of operational and broken springs satisfy the pattern constraints and group sizes. Sum all counts.

**Example:** `???.### 1,1,3` has exactly 1 valid arrangement: `#.#.###`

## Part 2: Unfolded Records

The records were folded! Unfold by:
- Repeating the pattern 5 times, joined by `?`
- Repeating the group list 5 times

Example: `.# 1` becomes `.#?.#?.#?.#?.# 1,1,1,1,1`

This dramatically increases the search space, making naive solutions infeasible.

## Algorithmic Approach

### Core Algorithm: Memoized Dynamic Programming

The problem is solved using a recursive DP with three state variables:

1. **pos**: Current position in the pattern string
2. **group_idx**: Index of the current group we're trying to match
3. **current_run**: Length of the current run of damaged springs

At each position, we make decisions based on the current character:
- If `.` or `?`: Try placing an operational spring (ends current run if it matches group size)
- If `#` or `?`: Try placing a damaged spring (extends current run)

### Key Insight

The recursive structure creates overlapping subproblems (same state reached via different paths), making memoization essential. Without memoization, Part 2 would require exploring an astronomical number of possibilities.

### State Transitions

```
dp(pos, group_idx, current_run) =
  case pattern[pos]:
    '.' or '?':
      if current_run == 0: dp(pos+1, group_idx, 0)
      elif current_run == groups[group_idx]: dp(pos+1, group_idx+1, 0)
    '#' or '?':
      if current_run < groups[group_idx]: dp(pos+1, group_idx, current_run+1)
```

### Base Case

At the end of the pattern:
- Valid if all groups matched and no partial run
- Valid if on last group and current run equals that group's size

### Complexity

- **Time:** O(N × G × M) where N = pattern length, G = number of groups, M = max group size
- **Space:** O(N × G × M) for memoization table

For Part 2, with 5x unfolding:
- Pattern length increases ~5x
- Groups increase 5x
- Still polynomial thanks to memoization

## Programming Techniques Highlighted

1. **Memoization/Dynamic Programming**: Essential for avoiding exponential blowup
2. **String Processing**: Handling pattern characters and group parsing
3. **Recursive State Machine**: The DP formulation acts like a state machine consuming input

## Data Structures Used

- **Hash Map / Memo Table**: For caching DP states (most implementations)
- **3D Array**: Some implementations (C, ARM64) use fixed-size arrays for speed
- **Tuple/String Keys**: State representation varies by language

## Language-Specific Notes

### Fast Performers
- **C++ (49.0ms)**: Fast hash maps with `unordered_map` and efficient string handling
- **C (59.5ms)**: 3D array memoization avoids hash overhead
- **Go (62.7ms)**: Efficient map implementation and fast compilation

### Interesting Observations
- **ARM64 (128.7ms)**: Competitive despite being hand-written assembly with 3D memo table
- **Common Lisp (125.7ms)**: Surprisingly fast, leveraging efficient hash tables
- **Zig (103.3ms)**: AutoHashMap works well for this problem

### Language Challenges
- **Bash (>10 minutes)**: Memoized DP is pathologically slow in shell
  - Associative arrays exist but are slow
  - Subshell spawning for function returns adds massive overhead
  - Part 1 works but Part 2 times out
- **Ruby (1,405.7ms)**: Hash lookups slower than compiled languages
- **Perl (604.2ms)**: Decent but hash performance is limiting

### Implementation Variations

| Approach | Languages |
|----------|-----------|
| HashMap with tuple/string key | Python, Node.js, Ruby, PHP, Perl, Go, Rust, Java, Clojure |
| HashMap with custom struct | C++, Zig |
| 3D array (fixed size) | C, ARM64 |
| Atom-based cache | Clojure |
| Hash-table with list keys | Common Lisp |

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C++         | 49.0         | 2.3         |
| C           | 59.5         | 3.3         |
| Go          | 62.7         | 10.1        |
| Rust        | 77.9         | 3.2         |
| Zig         | 103.3        | 1.9         |
| Lisp        | 125.7        | 91.5        |
| ARM64 asm   | 128.7        | 2.8         |
| Node.js     | 169.3        | 52.1        |
| Java        | 171.9        | 137.1       |
| PHP         | 256.4        | 95.8        |
| Python      | 328.8        | 95.5        |
| Perl        | 604.2        | 5.5         |
| Clojure     | 712.2        | 490.9       |
| Ruby        | 1,405.7      | 29.0        |
| Bash*       | >600,000     | 2.0         |
| ColdFusion* | ~3,500       | ~1,100      |

*Bash requires >10 minutes for Part 2 due to memoized DP overhead in shell.
*ColdFusion estimated from similar day patterns.

## Answers

- **Part 1:** 7407
- **Part 2:** 30568243604962
