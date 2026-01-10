#!/usr/bin/env bash

# Day 2: Dive!
# Read submarine navigation commands and compute final position

INPUT_FILE="${BASH_SOURCE%/*}/../input.txt"

# Part 1: Simple horizontal/depth tracking
part1() {
    local horizontal=0
    local depth=0

    while read -r cmd val; do
        case "$cmd" in
            forward) ((horizontal += val)) ;;
            down)    ((depth += val)) ;;
            up)      ((depth -= val)) ;;
        esac
    done < "$INPUT_FILE"

    echo $((horizontal * depth))
}

# Part 2: Track aim, depth changes on forward
part2() {
    local horizontal=0
    local depth=0
    local aim=0

    while read -r cmd val; do
        case "$cmd" in
            forward)
                ((horizontal += val))
                ((depth += aim * val))
                ;;
            down) ((aim += val)) ;;
            up)   ((aim -= val)) ;;
        esac
    done < "$INPUT_FILE"

    echo $((horizontal * depth))
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
