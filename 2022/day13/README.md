# Day 13: Distress Signal

## Problem Summary

You receive packets from a distress signal that are out of order. Each packet is a nested list structure containing integers and other lists. Your task is to compare and sort these packets to decode the message.

**Part 1**: Given pairs of packets, determine which pairs are already in the correct order according to specific comparison rules. Return the sum of 1-indexed indices of correctly ordered pairs.

**Part 2**: Sort all packets (including two divider packets `[[2]]` and `[[6]]`) and find the "decoder key" - the product of the 1-indexed positions of the divider packets.

## Input Format

Pairs of packets separated by blank lines. Each packet is valid JSON (nested arrays of integers):
```
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]
```

## Algorithmic Approach

### Comparison Rules

The comparison is a recursive algorithm with three cases:

1. **Both integers**: Compare numerically
   - Left < Right: correct order (-1)
   - Left > Right: wrong order (1)
   - Equal: continue (0)

2. **Both lists**: Compare element by element
   - Recursively compare corresponding elements
   - If all elements equal, shorter list comes first
   - Left exhausted first: correct order (-1)
   - Right exhausted first: wrong order (1)

3. **Mixed types**: Convert integer to single-element list, retry comparison

### Key Insight

The comparison function forms a valid **total ordering** suitable for sorting. This means we can:
- Use it directly as a comparator for sorting algorithms
- The recursive structure handles arbitrarily nested data

### Part 1 Algorithm

```
for each pair (index i):
    if compare(left, right) == -1:
        sum += i + 1  # 1-indexed
```

### Part 2 Algorithm

```
all_packets = parse_all_packets()
all_packets.append([[2]])  # divider 1
all_packets.append([[6]])  # divider 2
all_packets.sort(key=comparison)
return (position of [[2]]) * (position of [[6]])
```

### Data Structures

- **Recursive ADT**: Packets are either integers or lists of packets
- **JSON parsing**: Most languages have built-in JSON parsers
- **Comparison function**: Returns -1, 0, or 1 (standard comparator pattern)

### Complexity

- **Part 1**: O(n * d) where n = number of pairs, d = average comparison depth
- **Part 2**: O(m * log(m) * d) where m = total packets (sorting dominates)
- **Space**: O(m * s) where s = average packet size for storage

## Programming Techniques Highlighted

- **Recursive comparison**: The heart of the solution
- **JSON parsing**: Leveraging language built-ins for nested structure parsing
- **Custom sorting**: Using comparison functions with language sort APIs
- **Pattern matching**: Many languages benefit from matching on types (list vs integer)

## Language-Specific Notes

### JSON Parsing
All languages needed JSON parsing for nested structures:
- **Python, Node.js, PHP, Ruby**: Built-in JSON libraries
- **C, C++, Rust, Zig, Go**: Manual parsing or third-party libraries
- **ARM64**: Hand-rolled recursive descent parser
- **Bash**: `jq` for JSON manipulation
- **Clojure**: EDN/JSON reader
- **Common Lisp**: cl-json or manual parsing

### Comparator Adapters
Languages vary in how they accept custom comparators:
- **Python**: `functools.cmp_to_key()` wraps old-style comparators
- **Java**: `Comparator` interface
- **Node.js**: Direct comparator function to `.sort()`
- **Go**: `sort.Slice()` with less function

### Performance Notes
- **C, ARM64**: Fastest due to minimal overhead, manual parsing
- **Perl, Common Lisp, Python**: Middle tier, efficient recursion
- **Bash**: Slowest scripted due to `jq` subprocess overhead
- **ColdFusion**: CFML runtime overhead dominates

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 6.7          | 1.9         |
| ARM64 asm   | 6.7          | 1.9         |
| Rust        | 7.5          | 2.3         |
| C++         | 8.1          | 2.0         |
| Zig         | 9.1          | 1.9         |
| Go          | 9.4          | 5.6         |
| Perl        | 26.3         | 8.1         |
| Common Lisp | 26.5         | 40.4        |
| Python      | 28.1         | 15.5        |
| Node.js     | 47.3         | 44.1        |
| Java        | 61.9         | 53.9        |
| PHP         | 66.4         | 26.6        |
| Ruby        | 69.4         | 29.0        |
| Bash        | 223.2        | 6.6         |
| Clojure     | 423.1        | 137.5       |
| ColdFusion  | 2,758.5      | 1,077.6     |

## Answers

- Part 1: 6101
- Part 2: 21909
