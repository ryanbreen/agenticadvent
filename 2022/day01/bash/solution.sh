#!/bin/bash

# Advent of Code 2022 Day 1: Calorie Counting
# Parse input where numbers are grouped by blank lines
# Part 1: Find max sum of any single group
# Part 2: Find sum of top 3 group totals

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Use awk to calculate totals per elf (groups separated by blank lines)
# Output each elf's total on a separate line
elf_totals=$(awk '
    BEGIN { total = 0 }
    /^$/ {
        if (total > 0) print total
        total = 0
        next
    }
    { total += $1 }
    END { if (total > 0) print total }
' "$INPUT_FILE")

# Part 1: Find maximum
part1=$(echo "$elf_totals" | sort -rn | head -1)
echo "Part 1: $part1"

# Part 2: Find sum of top 3
part2=$(echo "$elf_totals" | sort -rn | head -3 | awk '{sum += $1} END {print sum}')
echo "Part 2: $part2"
