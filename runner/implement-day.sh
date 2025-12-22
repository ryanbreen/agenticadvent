#!/bin/bash
# implement-day.sh - Implements all languages for an AoC day in parallel
# Usage: ./implement-day.sh <year> <day> [--max-parallel N]
#
# Coordinates parallel execution of implement-language.sh workers
# Default: 8 parallel workers

set -e

YEAR="${1:?Usage: $0 <year> <day> [--max-parallel N]}"
DAY="${2:?Usage: $0 <year> <day> [--max-parallel N]}"
MAX_PARALLEL=8

# Parse optional args
shift 2
while [[ $# -gt 0 ]]; do
    case $1 in
        --max-parallel)
            MAX_PARALLEL="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Pad day with leading zero
DAY_PADDED=$(printf "%02d" "$DAY")

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DAY_DIR="$PROJECT_ROOT/$YEAR/day$DAY_PADDED"
REPORT_DIR="$SCRIPT_DIR/reports"
SUMMARY_FILE="$REPORT_DIR/${YEAR}_day${DAY_PADDED}_summary.md"

# All 16 required languages
LANGUAGES=(
    python node ruby perl php bash
    c cpp rust go zig
    java clojure lisp cfml arm64
)

mkdir -p "$REPORT_DIR"

echo "=============================================="
echo "Implementing $YEAR Day $DAY across ${#LANGUAGES[@]} languages"
echo "Max parallel workers: $MAX_PARALLEL"
echo "=============================================="
echo ""

# Check prerequisites
if [ ! -f "$DAY_DIR/problem.md" ]; then
    echo "ERROR: Problem not extracted. Run: node runner/extract.js $YEAR $DAY"
    exit 1
fi

# Track running jobs
declare -A PIDS
declare -A STATUSES

# Function to wait for a slot
wait_for_slot() {
    while [ ${#PIDS[@]} -ge $MAX_PARALLEL ]; do
        for lang in "${!PIDS[@]}"; do
            pid=${PIDS[$lang]}
            if ! kill -0 "$pid" 2>/dev/null; then
                wait "$pid" 2>/dev/null
                STATUSES[$lang]=$?
                unset PIDS[$lang]
                if [ ${STATUSES[$lang]} -eq 0 ]; then
                    echo "[DONE] $lang completed successfully"
                else
                    echo "[FAIL] $lang failed with exit code ${STATUSES[$lang]}"
                fi
            fi
        done
        sleep 1
    done
}

# Function to wait for all jobs
wait_all() {
    for lang in "${!PIDS[@]}"; do
        pid=${PIDS[$lang]}
        wait "$pid" 2>/dev/null
        STATUSES[$lang]=$?
        if [ ${STATUSES[$lang]} -eq 0 ]; then
            echo "[DONE] $lang completed successfully"
        else
            echo "[FAIL] $lang failed with exit code ${STATUSES[$lang]}"
        fi
    done
}

# Start time
START_TIME=$(date +%s)

# Launch workers
echo "Launching workers..."
for lang in "${LANGUAGES[@]}"; do
    wait_for_slot

    echo "[START] Launching $lang..."
    LOG_FILE="$REPORT_DIR/${YEAR}_day${DAY_PADDED}_${lang}_worker.log"

    "$SCRIPT_DIR/implement-language.sh" "$YEAR" "$DAY" "$lang" > "$LOG_FILE" 2>&1 &
    PIDS[$lang]=$!
done

# Wait for all to complete
echo ""
echo "Waiting for all workers to complete..."
wait_all

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# =============================================================================
# SYNTHESIZE RESULTS
# =============================================================================
echo ""
echo "=============================================="
echo "Synthesizing results..."
echo "=============================================="

# Collect all reports
SUCCESSFUL=0
FAILED=0

cat > "$SUMMARY_FILE" << EOF
# Day $DAY: Implementation Summary

Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")
Total time: ${DURATION}s

## Benchmark Results

| Language    | Runtime (ms) | Memory (MB) | Status |
|-------------|--------------|-------------|--------|
EOF

for lang in "${LANGUAGES[@]}"; do
    REPORT_FILE="$REPORT_DIR/${YEAR}_day${DAY_PADDED}_${lang}.json"

    if [ -f "$REPORT_FILE" ]; then
        RUNTIME=$(jq -r '.runtime_ms // "N/A"' "$REPORT_FILE")
        MEMORY=$(jq -r '.memory_mb // "N/A"' "$REPORT_FILE")
        STATUS="✓"
        ((SUCCESSFUL++))
    else
        RUNTIME="N/A"
        MEMORY="N/A"
        STATUS="✗"
        ((FAILED++))
    fi

    printf "| %-11s | %12s | %11s | %6s |\n" "$lang" "$RUNTIME" "$MEMORY" "$STATUS" >> "$SUMMARY_FILE"
done

cat >> "$SUMMARY_FILE" << EOF

## Summary

- Successful: $SUCCESSFUL / ${#LANGUAGES[@]}
- Failed: $FAILED / ${#LANGUAGES[@]}
- Total time: ${DURATION}s

## Individual Reports

EOF

for lang in "${LANGUAGES[@]}"; do
    REPORT_FILE="$REPORT_DIR/${YEAR}_day${DAY_PADDED}_${lang}.json"
    if [ -f "$REPORT_FILE" ]; then
        echo "- [$lang](./${YEAR}_day${DAY_PADDED}_${lang}.json)" >> "$SUMMARY_FILE"
    fi
done

echo ""
echo "Summary saved to: $SUMMARY_FILE"
echo ""
cat "$SUMMARY_FILE"

echo ""
echo "=============================================="
echo "COMPLETE: $YEAR Day $DAY"
echo "Successful: $SUCCESSFUL / ${#LANGUAGES[@]}"
echo "Failed: $FAILED"
echo "Total time: ${DURATION}s"
echo "=============================================="
