#!/usr/bin/env bash
# Day 25: Code Chronicle - Lock and key matching

set -euo pipefail

INPUT_FILE="$(dirname "$0")/../input.txt"

# Arrays to hold locks and keys (stored as comma-separated height strings)
declare -a locks
declare -a keys

# Parse schematics into locks and keys
parse_input() {
    local schematic=""
    local lines=()

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ -z "$line" ]]; then
            # Process accumulated schematic
            if [[ ${#lines[@]} -gt 0 ]]; then
                process_schematic
                lines=()
            fi
        else
            lines+=("$line")
        fi
    done < "$INPUT_FILE"

    # Process final schematic if any
    if [[ ${#lines[@]} -gt 0 ]]; then
        process_schematic
    fi
}

# Process a single schematic
process_schematic() {
    local heights=()

    # Check if it's a lock (top row is #####) or key (top row is .....)
    if [[ "${lines[0]}" == "#####" ]]; then
        # Lock: count # from top (excluding top row)
        for col in {0..4}; do
            local height=0
            for row in {1..6}; do
                local char="${lines[$row]:$col:1}"
                if [[ "$char" == "#" ]]; then
                    height=$((height + 1))
                else
                    break
                fi
            done
            heights+=("$height")
        done
        # Join heights with commas
        local lock_str
        lock_str=$(IFS=,; echo "${heights[*]}")
        locks+=("$lock_str")
    else
        # Key: count # from bottom (excluding bottom row)
        for col in {0..4}; do
            local height=0
            for row in {5..0}; do
                local char="${lines[$row]:$col:1}"
                if [[ "$char" == "#" ]]; then
                    height=$((height + 1))
                else
                    break
                fi
            done
            heights+=("$height")
        done
        # Join heights with commas
        local key_str
        key_str=$(IFS=,; echo "${heights[*]}")
        keys+=("$key_str")
    fi
}

# Check if a key fits a lock - echo 1 if fits, 0 if not
fits() {
    local lock_str="$1"
    local key_str="$2"

    IFS=',' read -ra lock_heights <<< "$lock_str"
    IFS=',' read -ra key_heights <<< "$key_str"

    for i in {0..4}; do
        local sum=$((lock_heights[i] + key_heights[i]))
        if [[ $sum -gt 5 ]]; then
            echo 0
            return
        fi
    done

    echo 1
}

# Count unique lock/key pairs that fit
part1() {
    local count=0

    for lock in "${locks[@]}"; do
        for key in "${keys[@]}"; do
            local result
            result=$(fits "$lock" "$key")
            if [[ "$result" -eq 1 ]]; then
                count=$((count + 1))
            fi
        done
    done

    echo "$count"
}

# Main execution
main() {
    parse_input

    local answer1
    answer1=$(part1)
    echo "Part 1: $answer1"

    # Day 25 typically only has Part 1
    echo "Part 2: Merry Christmas!"
}

main
