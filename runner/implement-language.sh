#!/bin/bash
# implement-language.sh - Implements a single language for a single AoC day
# Usage: ./implement-language.sh <year> <day> <language>
#
# Two-pass approach:
#   Pass 1: Initial implementation based on problem and reference solution
#   Pass 2: Quality review and improvement on three dimensions:
#           - Idiomatic quality (language patterns/conventions)
#           - Stylistic quality (comments, structure)
#           - Algorithmic integrity (performance within language constraints)

set -e

YEAR="${1:?Usage: $0 <year> <day> <language>}"
DAY="${2:?Usage: $0 <year> <day> <language>}"
LANG="${3:?Usage: $0 <year> <day> <language>}"

# Pad day with leading zero
DAY_PADDED=$(printf "%02d" "$DAY")

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DAY_DIR="$PROJECT_ROOT/$YEAR/day$DAY_PADDED"
LANG_DIR="$DAY_DIR/$LANG"
REPORT_DIR="$PROJECT_ROOT/runner/reports"
REPORT_FILE="$REPORT_DIR/${YEAR}_day${DAY_PADDED}_${LANG}.json"

# Create directories
mkdir -p "$LANG_DIR"
mkdir -p "$REPORT_DIR"

# Determine solution file extension and run command
get_solution_info() {
    case "$LANG" in
        python) echo "solution.py|python3 solution.py" ;;
        node) echo "solution.js|node solution.js" ;;
        ruby) echo "solution.rb|ruby solution.rb" ;;
        perl) echo "solution.pl|perl solution.pl" ;;
        php) echo "solution.php|php solution.php" ;;
        bash) echo "solution.sh|bash solution.sh" ;;
        c) echo "solution.c|./solution" ;;
        cpp) echo "solution.cpp|./solution" ;;
        rust) echo "solution.rs|./solution" ;;
        go) echo "solution.go|./solution" ;;
        zig) echo "solution.zig|./solution" ;;
        java) echo "solution.java|java solution" ;;
        clojure) echo "solution.clj|clojure -M solution.clj" ;;
        lisp) echo "solution.lisp|sbcl --script solution.lisp" ;;
        cfml) echo "solution.cfm|box solution.cfm" ;;
        arm64) echo "solution.s|./solution" ;;
        *) echo "solution.txt|echo 'Unknown language'" ;;
    esac
}

SOLUTION_INFO=$(get_solution_info)
SOLUTION_FILE="${SOLUTION_INFO%|*}"
RUN_CMD="${SOLUTION_INFO#*|}"

echo "=============================================="
echo "Implementing $YEAR Day $DAY in $LANG"
echo "=============================================="
echo "Solution file: $LANG_DIR/$SOLUTION_FILE"
echo "Run command: $RUN_CMD"
echo ""

# Find a reference implementation to base the solution on
find_reference() {
    for ref_lang in python node c rust go; do
        local ref_file="$DAY_DIR/$ref_lang/solution.*"
        if compgen -G "$ref_file" > /dev/null 2>&1; then
            local actual_file=$(ls $ref_file 2>/dev/null | head -1)
            if [ -f "$actual_file" ] && [ -s "$actual_file" ]; then
                # Check if it's not a stub (has actual implementation)
                if ! grep -q "TODO: Implement" "$actual_file" 2>/dev/null; then
                    echo "$actual_file"
                    return
                fi
            fi
        fi
    done
    echo ""
}

REFERENCE=$(find_reference)

# =============================================================================
# PASS 1: Initial Implementation
# =============================================================================
echo "--- PASS 1: Initial Implementation ---"

PASS1_PROMPT="You are implementing Advent of Code $YEAR Day $DAY in $LANG.

TASK: Create a complete, working implementation.

Problem description: $DAY_DIR/problem.md
Input file: $DAY_DIR/input.txt
Output to: $LANG_DIR/$SOLUTION_FILE"

if [ -n "$REFERENCE" ]; then
    PASS1_PROMPT="$PASS1_PROMPT
Reference implementation for algorithm: $REFERENCE"
fi

PASS1_PROMPT="$PASS1_PROMPT

REQUIREMENTS:
1. Read input from ../input.txt (relative to solution file)
2. Implement both Part 1 and Part 2
3. Print output as 'Part 1: <answer>' and 'Part 2: <answer>'
4. The solution must be self-contained and runnable

DO NOT ask questions. Just implement the solution and verify it runs correctly.
After writing the code, run it to verify the output."

# Run Claude for Pass 1
cd "$PROJECT_ROOT"
echo "$PASS1_PROMPT" | claude --print 2>&1 | tee "$REPORT_DIR/${YEAR}_day${DAY_PADDED}_${LANG}_pass1.log"

# Check if solution was created
if [ ! -f "$LANG_DIR/$SOLUTION_FILE" ]; then
    echo "ERROR: Solution file not created after Pass 1"
    exit 1
fi

# =============================================================================
# PASS 2: Quality Review and Improvement
# =============================================================================
echo ""
echo "--- PASS 2: Quality Review and Improvement ---"

PASS2_PROMPT="Review and improve the $LANG implementation at $LANG_DIR/$SOLUTION_FILE

Score the current implementation on three dimensions (1-10):

1. IDIOMATIC QUALITY
   - Uses $LANG-specific patterns and conventions
   - Leverages standard library effectively
   - Follows community best practices

2. STYLISTIC QUALITY
   - Code a skilled $LANG developer would be proud to write
   - Clear naming following $LANG conventions
   - Appropriate comments where logic isn't self-evident
   - Consistent formatting

3. ALGORITHMIC INTEGRITY
   - As tight and performant as reasonable for $LANG
   - Efficient data structures
   - No unnecessary complexity

For any dimension below 10/10, make specific improvements.
After improvements, verify the solution still produces correct output.

Output a summary with:
- Initial scores (3 numbers)
- Changes made
- Final scores (should be 10/10/10)
- Benchmark: run time and output"

cd "$PROJECT_ROOT"
echo "$PASS2_PROMPT" | claude --print 2>&1 | tee "$REPORT_DIR/${YEAR}_day${DAY_PADDED}_${LANG}_pass2.log"

# =============================================================================
# BENCHMARKING
# =============================================================================
echo ""
echo "--- BENCHMARKING ---"

# Run benchmark
cd "$PROJECT_ROOT"
BENCHMARK_OUTPUT=$(python3 runner/benchmark.py "cd $LANG_DIR && $RUN_CMD" 3 2>&1) || true
echo "$BENCHMARK_OUTPUT"

# Extract metrics
RUNTIME=$(echo "$BENCHMARK_OUTPUT" | grep "Avg time:" | sed 's/.*: //' | sed 's/ ms//')
MEMORY=$(echo "$BENCHMARK_OUTPUT" | grep "Memory:" | sed 's/.*: //' | sed 's/ MB//')
OUTPUT=$(echo "$BENCHMARK_OUTPUT" | grep -A2 "Output:" | tail -2)

# =============================================================================
# GENERATE REPORT
# =============================================================================
echo ""
echo "--- GENERATING REPORT ---"

cat > "$REPORT_FILE" << EOF
{
    "year": $YEAR,
    "day": $DAY,
    "language": "$LANG",
    "solution_file": "$LANG_DIR/$SOLUTION_FILE",
    "runtime_ms": ${RUNTIME:-null},
    "memory_mb": ${MEMORY:-null},
    "output": $(echo "$OUTPUT" | python3 -c 'import sys,json; print(json.dumps(sys.stdin.read().strip()))'),
    "status": "completed",
    "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
}
EOF

echo "Report saved to: $REPORT_FILE"
cat "$REPORT_FILE"

echo ""
echo "=============================================="
echo "DONE: $YEAR Day $DAY in $LANG"
echo "=============================================="
