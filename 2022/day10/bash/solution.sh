#!/bin/bash

# Advent of Code 2022 Day 10: Cathode-Ray Tube

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Read input into array
mapfile -t instructions < "$INPUT_FILE"

# CPU simulation
x=1
cycle=0
signal_sum=0

# CRT display (6 rows of 40 characters)
declare -a screen
for i in {0..5}; do
    screen[$i]=""
done

# Function to process a cycle
process_cycle() {
    cycle=$((cycle + 1))

    # Part 1: Check signal strength at specific cycles
    if (( cycle == 20 || cycle == 60 || cycle == 100 || cycle == 140 || cycle == 180 || cycle == 220 )); then
        signal_sum=$((signal_sum + cycle * x))
    fi

    # Part 2: Draw pixel
    pos=$(( (cycle - 1) % 40 ))
    row=$(( (cycle - 1) / 40 ))

    # Check if sprite (3 pixels wide at x-1, x, x+1) overlaps with current position
    diff=$((pos - x))
    if (( diff >= -1 && diff <= 1 )); then
        screen[$row]="${screen[$row]}#"
    else
        screen[$row]="${screen[$row]}."
    fi
}

# Process instructions
for line in "${instructions[@]}"; do
    if [[ "$line" == "noop" ]]; then
        process_cycle
    elif [[ "$line" == addx* ]]; then
        v="${line#addx }"
        process_cycle
        process_cycle
        x=$((x + v))
    fi
done

# Output results
echo "Part 1: $signal_sum"
echo "Part 2:"
for row in "${screen[@]}"; do
    echo "$row"
done
