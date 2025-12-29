#!/bin/bash
# Requires bash 4.0+ for associative arrays

# Day 8: Haunted Wasteland
# Navigate network using L/R instructions

input_file="../input.txt"

# Read input
instructions=""
line_num=0
declare -A left_map
declare -A right_map
declare -a all_nodes

while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ $line_num -eq 0 ]]; then
        instructions="$line"
    elif [[ $line_num -gt 1 && -n "$line" ]]; then
        # Parse: AAA = (BBB, CCC)
        node="${line%% =*}"
        rest="${line#*= (}"
        rest="${rest%)}"
        left="${rest%%, *}"
        right="${rest##*, }"

        left_map["$node"]="$left"
        right_map["$node"]="$right"
        all_nodes+=("$node")
    fi
    ((line_num++))
done < "$input_file"

instructions_len=${#instructions}

# GCD using iterative Euclidean algorithm
gcd() {
    local a=$1 b=$2
    while [[ $b -ne 0 ]]; do
        local t=$b
        b=$((a % b))
        a=$t
    done
    echo "$a"
}

# LCM computed from GCD
lcm() {
    local a=$1 b=$2
    local g
    g=$(gcd "$a" "$b")
    echo $((a / g * b))
}

# Part 1: Navigate from AAA to ZZZ
part1() {
    local current="AAA"
    local steps=0

    while [[ "$current" != "ZZZ" ]]; do
        local idx=$((steps % instructions_len))
        local direction="${instructions:idx:1}"

        if [[ "$direction" == "L" ]]; then
            current="${left_map[$current]}"
        else
            current="${right_map[$current]}"
        fi
        ((steps++))
    done

    echo "$steps"
}

# Part 2: Navigate all nodes ending in A to nodes ending in Z simultaneously
part2() {
    # Find all starting nodes (ending in A)
    local -a start_nodes=()
    for node in "${all_nodes[@]}"; do
        if [[ "$node" == *A ]]; then
            start_nodes+=("$node")
        fi
    done

    # Find cycle length for each starting node
    local -a cycle_lengths=()
    for start in "${start_nodes[@]}"; do
        local current="$start"
        local steps=0

        # Navigate until we reach a node ending in Z
        while [[ "$current" != *Z ]]; do
            local idx=$((steps % instructions_len))
            local direction="${instructions:idx:1}"

            if [[ "$direction" == "L" ]]; then
                current="${left_map[$current]}"
            else
                current="${right_map[$current]}"
            fi
            ((steps++))
        done

        cycle_lengths+=("$steps")
    done

    # Calculate LCM of all cycle lengths
    local result="${cycle_lengths[0]}"
    for ((i = 1; i < ${#cycle_lengths[@]}; i++)); do
        result=$(lcm "$result" "${cycle_lengths[i]}")
    done

    echo "$result"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
