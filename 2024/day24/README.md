# Day 24: Crossed Wires

## Problem Summary

Day 24 presents a Boolean logic circuit simulation problem with two distinct challenges:

**Part 1**: Simulate a circuit of logic gates (AND, OR, XOR) with initial wire values and compute the decimal number represented by wires starting with 'z'. The circuit has no loops - gates wait for both inputs before computing their output, and outputs propagate through the system until all values are resolved.

**Part 2**: Discover that the circuit is actually a faulty binary adder. Exactly four pairs of gate outputs have been swapped (8 wires total). Identify which wires were swapped by analyzing the expected structure of a ripple-carry adder circuit and finding structural violations.

### Input Format

The input has two sections separated by a blank line:

1. **Initial wire values**: Lines like `x00: 1` or `y01: 0` setting wire states
2. **Gate definitions**: Lines like `x00 AND y00 -> z00` describing gate connections

Wires starting with 'x' and 'y' are inputs to the adder, and wires starting with 'z' are outputs.

## Algorithmic Approach

### Part 1: Circuit Simulation

A straightforward topological evaluation:

1. **Parse** initial wire values into a dictionary
2. **Parse** gate definitions as tuples of (input1, operation, input2, output)
3. **Simulate** using an iterative approach:
   - Iterate through all gates
   - If both inputs are available, compute the output (AND, OR, or XOR)
   - Remove processed gates from the queue
   - Repeat until all gates are processed
4. **Extract result** by collecting all z-wires, sorting them, and converting from binary to decimal

**Complexity**: O(G) where G is the number of gates. Each gate is processed exactly once.

### Part 2: Ripple-Carry Adder Analysis

This is a **circuit debugging problem** requiring knowledge of how a binary adder should be structured:

#### Correct Adder Structure

A ripple-carry adder has a specific gate pattern for each bit:

- **Bit 0** (special case):
  - `z00 = x00 XOR y00`
  - `carry0 = x00 AND y00`

- **Bit i** (general case):
  - `sum_partial = x[i] XOR y[i]` (first-level XOR)
  - `z[i] = sum_partial XOR carry[i-1]` (second-level XOR)
  - `carry_partial = x[i] AND y[i]` (direct carry)
  - `carry_propagate = sum_partial AND carry[i-1]` (propagated carry)
  - `carry[i] = carry_partial OR carry_propagate` (combined carry)

#### Detection Algorithm

Build two lookup structures:
1. **By output**: Map each output wire to its gate specification
2. **By inputs + operation**: Map (frozenset of inputs, operation) to output wire

Then check these structural rules:

1. **XOR gates not taking x/y inputs must output to z-wires**
   - Second-level XOR computes the final sum bit
   - If an XOR gate has non-x/y inputs but doesn't output to z, it's swapped

2. **Z-wires (except the highest bit) must come from XOR gates**
   - The final output of each bit is always a XOR operation
   - If a z-wire comes from AND or OR, it's swapped

3. **AND gates (except x00 AND y00) must feed into OR gates**
   - AND gates produce carry bits that combine via OR
   - If an AND output isn't used by an OR, it's swapped

4. **First-level XOR (x[i] XOR y[i]) must feed both XOR and AND**
   - The partial sum is used by both second-level XOR and carry-propagate AND
   - If it doesn't feed both gate types, it's swapped

Any wire that violates these rules is part of a swap.

**Complexity**: O(G²) in worst case for checking usage patterns, but practically O(G) since each gate is checked against a small constant number of rules.

## Key Insights

### Part 1 Insight

The "no loops" constraint is critical - it guarantees the circuit will always settle to a final state. This allows a simple queue-based simulation without worrying about oscillation or complex dependency resolution.

### Part 2 Insight

The problem transforms from circuit simulation to **circuit structural analysis**. Instead of testing all possible swaps (which would be combinatorially explosive), we leverage domain knowledge about ripple-carry adders. The key realization is that certain gate types must appear in specific positions with specific connectivity patterns. Violations of these patterns directly reveal swapped wires.

This is a beautiful example of how domain-specific knowledge (adder architecture) can reduce an intractable search problem to simple pattern matching.

## Programming Techniques Highlighted

### Data Structures
- **Hash maps**: Wire value lookups and gate indexing
- **Sets**: Collecting unique swapped wires
- **Tuples/Records**: Representing gates as immutable structures
- **Frozensets**: Creating hashable wire pair keys

### Algorithms
- **Topological sorting**: Implicit in the gate simulation (dependency-driven execution)
- **Pattern matching**: Identifying gate types and wire name patterns
- **Graph analysis**: The circuit is a directed acyclic graph (DAG)
- **Constraint verification**: Checking structural rules against expected patterns

### Computer Science Concepts
- **Digital logic**: Boolean gates, binary arithmetic
- **Circuit design**: Ripple-carry adders, carry propagation
- **Invariant checking**: Verifying expected properties hold
- **Debugging by specification**: Comparing actual structure to ideal specification

## Language-Specific Notes

### Compiled Languages Excel

**C, C++, Rust, Zig, ARM64** (4.9-6.1ms):
- Efficient hash table implementations
- Fast string operations for wire name parsing
- Minimal overhead for gate simulation loop
- ARM64 assembly shows hand-optimized string handling with BSD syscalls

**Go** (9.5ms):
- Built-in map performance is excellent
- Garbage collection adds minimal overhead for this problem
- String manipulation slightly slower than C/Rust

### Scripting Languages Perform Well

**Perl** (13.5ms): Excellent for text parsing and pattern matching
- Native hash table support
- Regex for wire name classification
- Set operations via hash keys

**Python** (27.8ms): Clean and readable
- Dictionary comprehensions for wire parsing
- Straightforward set operations for Part 2
- List comprehensions for z-wire extraction

**Node.js** (49.1ms): V8 JIT helps
- Object/Map for wire storage
- String splitting and parsing efficient
- Set for collecting swapped wires

**Common Lisp** (49.9ms): Functional style
- Hash tables for wire/gate lookups
- List processing for gate simulation
- Fast string operations

**Java** (52.1ms): JVM startup dominates
- HashMap and HashSet perform well
- String parsing is verbose but fast
- The actual computation is quick, startup overhead is the cost

**Ruby** (70.0ms): Expressive but slower
- Hash and Set built-ins work well
- Regex and string methods are convenient
- Interpreter overhead noticeable

**PHP** (90.1ms): Respectable for scripting
- Associative arrays for everything
- String functions adequate
- Higher variance in timing

### JVM Startup Tax

**Clojure** (464.9ms): JVM + Clojure runtime
- Immutable data structures add overhead
- Elegant functional solution
- Startup time dominates the benchmark

### Shell Scripting Struggles

**Bash** (157.0ms): Much better than nested-loop problems
- Associative arrays work adequately
- String processing via parameter expansion
- Avoids heavy arithmetic unlike some days
- Still slow compared to compiled languages

### ColdFusion

Requires web server context (Lucee/Adobe ColdFusion server), not benchmarkable via CLI.

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 4.9          | 1.9         |
| C++         | 5.2          | 1.9         |
| Rust        | 5.4          | 1.9         |
| C           | 5.5          | 1.9         |
| ARM64       | 6.1          | 1.9         |
| Go          | 9.5          | 5.0         |
| Perl        | 13.5         | 4.7         |
| Python      | 27.8         | 15.4        |
| Node.js     | 49.1         | 43.7        |
| Common Lisp | 49.9         | 52.0        |
| Java        | 52.1         | 47.6        |
| Ruby        | 70.0         | 28.4        |
| PHP         | 90.1         | 25.9        |
| Bash        | 157.0        | 7.0         |
| Clojure     | 464.9        | 140.5       |
| ColdFusion  | N/A          | N/A         |

## Answers

- **Part 1**: 51107420031718
- **Part 2**: cpm,ghp,gpr,krs,nks,z10,z21,z33
