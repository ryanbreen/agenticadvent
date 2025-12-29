#!/usr/bin/env bash
# Advent of Code 2023 Day 11: Cosmic Expansion

set -e

INPUT_FILE="${1:-../input.txt}"

# Read grid into array
declare -a grid
row=0
while IFS= read -r line || [[ -n "$line" ]]; do
    [[ -z "$line" ]] && continue
    grid[$row]="$line"
    ((row++)) || true
done < "$INPUT_FILE"

num_rows=${#grid[@]}
num_cols=${#grid[0]}

# Find galaxy positions
declare -a galaxy_rows
declare -a galaxy_cols
galaxy_count=0

for ((r=0; r<num_rows; r++)); do
    line="${grid[$r]}"
    for ((c=0; c<num_cols; c++)); do
        ch="${line:$c:1}"
        if [[ "$ch" == "#" ]]; then
            galaxy_rows[$galaxy_count]=$r
            galaxy_cols[$galaxy_count]=$c
            ((galaxy_count++)) || true
        fi
    done
done

# Find empty rows (rows without any galaxy)
declare -A empty_rows
for ((r=0; r<num_rows; r++)); do
    line="${grid[$r]}"
    if [[ "$line" != *"#"* ]]; then
        empty_rows[$r]=1
    fi
done

# Find empty columns (columns without any galaxy)
declare -A empty_cols
for ((c=0; c<num_cols; c++)); do
    has_galaxy=0
    for ((r=0; r<num_rows; r++)); do
        line="${grid[$r]}"
        ch="${line:$c:1}"
        if [[ "$ch" == "#" ]]; then
            has_galaxy=1
            break
        fi
    done
    if [[ $has_galaxy -eq 0 ]]; then
        empty_cols[$c]=1
    fi
done

# Pre-compute prefix sums for empty rows and columns to speed up distance calculation
declare -a empty_row_prefix
declare -a empty_col_prefix

empty_row_prefix[0]=0
for ((r=0; r<num_rows; r++)); do
    prev=${empty_row_prefix[$r]}
    if [[ -n "${empty_rows[$r]}" ]]; then
        empty_row_prefix[$((r+1))]=$((prev + 1))
    else
        empty_row_prefix[$((r+1))]=$prev
    fi
done

empty_col_prefix[0]=0
for ((c=0; c<num_cols; c++)); do
    prev=${empty_col_prefix[$c]}
    if [[ -n "${empty_cols[$c]}" ]]; then
        empty_col_prefix[$((c+1))]=$((prev + 1))
    else
        empty_col_prefix[$((c+1))]=$prev
    fi
done

# Calculate distances
# We compute:
# - base_distance: sum of Manhattan distances without expansion
# - expansion_count: total number of empty rows/cols crossed across all pairs
# Part 1 = base_distance + expansion_count * 1 (factor 2 = original + 1 extra)
# Part 2 = base_distance + expansion_count * 999999 (factor 1000000 = original + 999999 extra)

base_distance=0
expansion_count=0

for ((i=0; i<galaxy_count; i++)); do
    r1=${galaxy_rows[$i]}
    c1=${galaxy_cols[$i]}
    for ((j=i+1; j<galaxy_count; j++)); do
        r2=${galaxy_rows[$j]}
        c2=${galaxy_cols[$j]}

        # Row distance
        if ((r1 < r2)); then
            min_r=$r1
            max_r=$r2
        else
            min_r=$r2
            max_r=$r1
        fi
        row_dist=$((max_r - min_r))
        empty_r=$((empty_row_prefix[max_r] - empty_row_prefix[min_r]))

        # Column distance
        if ((c1 < c2)); then
            min_c=$c1
            max_c=$c2
        else
            min_c=$c2
            max_c=$c1
        fi
        col_dist=$((max_c - min_c))
        empty_c=$((empty_col_prefix[max_c] - empty_col_prefix[min_c]))

        base_distance=$((base_distance + row_dist + col_dist))
        expansion_count=$((expansion_count + empty_r + empty_c))
    done
done

# Part 1: expansion factor = 2 means add 1 extra unit per empty row/col
part1=$((base_distance + expansion_count))
echo "Part 1: $part1"

# Part 2: expansion factor = 1000000 means add 999999 extra units per empty row/col
# Use bc for large number multiplication
part2=$(echo "$base_distance + $expansion_count * 999999" | bc)
echo "Part 2: $part2"
