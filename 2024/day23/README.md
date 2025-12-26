# Day 23: LAN Party

## Problem Summary

The puzzle involves analyzing a network map of computer connections to locate a LAN party at Easter Bunny HQ. Given a list of bidirectional connections between computers (represented as pairs like `kh-tc`), the goal is to identify groups of fully connected computers.

**Part 1**: Find all sets of three computers where each computer is connected to the other two (triangles in graph theory). Count how many of these triangles contain at least one computer whose name starts with `t`.

**Part 2**: Find the largest set of computers where every computer is connected to every other computer in the set (maximum clique). The password to the LAN party is all computer names in this set, sorted alphabetically and joined with commas.

### Input Format

Each line represents a bidirectional connection between two computers:
```
kh-tc
qp-kh
de-cg
...
```

Computer names are exactly 2 characters (lowercase letters).

## Algorithmic Approach

### Part 1 Algorithm: Triangle Enumeration

1. **Build adjacency graph**: Parse connections into an adjacency set/matrix representation
2. **Find all triangles**: For each edge (a,b), find common neighbors c where both a-c and b-c exist
3. **Filter by 't' prefix**: Count triangles where at least one node starts with 't'

**Implementation strategy**:
```
for each node a:
    for each neighbor b of a (where a < b):
        for each node c in (neighbors of a) ∩ (neighbors of b):
            found triangle (a, b, c)
```

### Part 2 Algorithm: Maximum Clique Finding

This is the classic **maximum clique problem**, which is NP-complete. However, for the input size (~500 nodes, ~3000 edges), the **Bron-Kerbosch algorithm** with pivoting is efficient enough.

**Bron-Kerbosch Algorithm** (recursive backtracking):
- Maintains three sets:
  - **R**: Current clique being built
  - **P**: Candidate nodes that could extend R
  - **X**: Already-processed nodes (for pruning)
- **Base case**: If P and X are empty, R is a maximal clique
- **Recursive step**: For each vertex v in P, recursively explore R ∪ {v} with neighbors of v
- **Pivoting optimization**: Choose a pivot vertex u from P ∪ X that has the most neighbors in P. Only branch on vertices NOT adjacent to the pivot, significantly reducing the search space.

### Key Insights

1. **Part 1 is tractable**: Finding triangles is O(n × d²) where d is average degree, much simpler than general clique finding
2. **Bron-Kerbosch efficiency**: The pivoting optimization is crucial - it reduces branching by avoiding vertices that cannot extend the current maximum clique
3. **Graph sparsity matters**: Real network graphs tend to have low clustering coefficients, making clique-finding more efficient than worst-case bounds suggest
4. **Set intersection is the bottleneck**: Finding common neighbors quickly (via set intersection) is key to performance

### Complexity

**Part 1 - Triangle Finding**:
- **Time**: O(n × d²) where n = number of nodes, d = average degree
  - For each of ~500 nodes, check ~6 neighbors, then find common neighbors (~6²)
  - Actual: ~500 × 36 = ~18,000 triangle checks
- **Space**: O(n + e) for adjacency structure (~500 nodes + ~3000 edges)

**Part 2 - Bron-Kerbosch**:
- **Worst-case time**: O(3^(n/3)) for general graphs (exponential)
- **Practical time**: Much better on sparse graphs with small maximum clique
  - Input has maximum clique of size 13
  - Pivoting reduces branching factor dramatically
- **Space**: O(n) for recursion stack (depth ~13)

## Programming Techniques Highlighted

### Core Concepts
- **Graph theory**: Undirected graphs, adjacency representations, triangles, cliques
- **Set operations**: Intersection, union, difference are fundamental primitives
- **Backtracking**: Bron-Kerbosch is a systematic search with pruning
- **Combinatorial optimization**: Maximum clique is a classic NP-complete problem

### Data Structures
- **Adjacency lists/sets**: Efficient neighbor lookups (O(1) membership test with sets)
- **Hash sets**: Fast set operations in dynamic languages
- **Bitmaps**: C/C++ use boolean arrays for efficient set representation
- **Recursion stack**: Managing R, P, X sets across recursive calls

### Algorithmic Techniques
- **Pruning**: The pivot optimization eliminates unnecessary branches
- **Memoization potential**: Could cache maximal cliques found, though not necessary here
- **Graph enumeration**: Systematically visiting all triangles without duplicates

## Language-Specific Notes

### Fast Performers

**C (18.4ms), ARM64 (9.0ms), Rust (20.3ms), Zig (24.3ms)**:
- Efficient adjacency matrix (boolean arrays) or hash-based adjacency lists
- Stack-allocated sets for Bron-Kerbosch (no allocation overhead)
- ARM64 particularly fast with hand-optimized set operations and minimal syscalls

**Go (27.2ms)**:
- Good map performance for adjacency sets
- Set operations via map[string]bool patterns
- Compiled performance with garbage collection doesn't hurt here

**C++ (40.2ms)**:
- STL unordered_set/unordered_map provide clean abstractions
- Set intersection via std::set_intersection or manual loops
- Some overhead from C++ abstractions vs. raw C

### Mid-Tier: Scripting Languages

**Python (45.1ms)**:
- Native set type with fast intersection (`&` operator)
- Dictionary-based adjacency: `defaultdict(set)`
- Clean Bron-Kerbosch implementation with set comprehensions

**Perl (84.3ms)**:
- Hash-based sets (keys as elements, values ignored)
- Recursive subroutines for Bron-Kerbosch
- String manipulation efficient for node names

**Node.js (87.4ms)**:
- Set object for adjacency lists
- Spread operator for set operations: `[...setA].filter(x => setB.has(x))`
- V8 JIT compilation helps

### Slower Implementations

**Ruby (168.2ms)**:
- Set class from standard library
- Recursive block-based Bron-Kerbosch
- Interpreter overhead noticeable on recursive algorithms

**Common Lisp (169.1ms)**:
- Hash tables for adjacency sets
- List-based set operations (less efficient than specialized structures)
- SBCL compiles to native code but still slower than C

**PHP (204.4ms)**:
- Associative arrays for sets (array keys)
- Recursive function calls
- PHP interpreter overhead significant

**Clojure (524.9ms)**:
- JVM startup dominates (~400ms)
- Persistent data structures (immutable sets) have overhead
- Functional style with recursion

### Extremely Slow

**Bash (91,025ms = 91 seconds)**:
- Graph algorithms in shell scripting are brutally slow
- Associative arrays for adjacency, but every operation spawns subprocesses
- Recursive functions have massive overhead
- Set operations implemented as loops over associative array keys
- Each triangle/clique check involves many fork/exec calls
- **This problem is pathological for Bash** - 1000x slower than C

### ColdFusion
- Requires web server/servlet context, not benchmarkable via CLI
- Would use struct-based adjacency representation

### Implementation Challenges

1. **Set operations**: Languages without native set types (C, Bash) require manual implementation
2. **Recursion depth**: Bron-Kerbosch can reach depth ~13, but most languages handle this fine
3. **Memory management**: C/Zig require careful memory handling for graph structures
4. **String sorting**: Part 2 requires alphabetically sorting clique members for the password

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64       | 9.0          | 2.3         |
| C           | 18.4         | 2.5         |
| Rust        | 20.3         | 4.9         |
| Zig         | 24.3         | 3.5         |
| Go          | 27.2         | 9.5         |
| C++         | 40.2         | 4.7         |
| Python      | 45.1         | 17.1        |
| Perl        | 84.3         | 10.2        |
| Node.js     | 87.4         | 51.6        |
| Java        | 148.6        | 91.1        |
| Ruby        | 168.2        | 33.2        |
| Common Lisp | 169.1        | 59.2        |
| PHP         | 204.4        | 30.0        |
| Clojure     | 524.9        | 186.0       |
| Bash        | 91025.1      | 7.1         |
| ColdFusion  | N/A          | N/A         |

### Performance Notes

- **ARM64 dominance**: Hand-optimized assembly with efficient bit manipulation and minimal overhead
- **C's efficiency**: Direct memory access, stack-allocated sets, adjacency matrix lookup
- **Systems languages cluster**: Rust/Zig/Go all within 2x of C performance
- **Python's competitiveness**: Native set operations keep Python fast despite being interpreted
- **JVM startup tax**: Java/Clojure pay ~100-400ms startup cost
- **Bash catastrophe**: Recursive graph algorithms expose shell scripting's fundamental inefficiency

## Answers

- **Part 1**: 1599
- **Part 2**: av,ax,dg,di,dw,fa,ge,kh,ki,ot,qw,vz,yw
