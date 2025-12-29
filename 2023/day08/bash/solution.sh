#!/bin/bash

# Day 8: Haunted Wasteland
# Navigate network using L/R instructions
# Compatible with bash 3.x (macOS default)

input_file="../input.txt"

# Read input line by line
instructions=""
line_num=0
declare -a nodes
declare -a lefts
declare -a rights
node_count=0

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

        nodes[$node_count]="$node"
        lefts[$node_count]="$left"
        rights[$node_count]="$right"
        ((node_count++))
    fi
    ((line_num++))
done < "$input_file"

instructions_len=${#instructions}

# Function to find index of a node
find_node_index() {
    local target=$1
    for ((i = 0; i < node_count; i++)); do
        if [[ "${nodes[i]}" == "$target" ]]; then
            echo "$i"
            return
        fi
    done
    echo "-1"
}

# Part 1: Navigate from AAA to ZZZ
part1() {
    local current="AAA"
    local steps=0
    local current_idx
    current_idx=$(find_node_index "$current")

    while [[ "$current" != "ZZZ" ]]; do
        local idx=$((steps % instructions_len))
        local direction="${instructions:idx:1}"

        if [[ "$direction" == "L" ]]; then
            current="${lefts[current_idx]}"
        else
            current="${rights[current_idx]}"
        fi
        current_idx=$(find_node_index "$current")
        ((steps++))
    done

    echo "$steps"
}

# GCD using bc for arbitrary precision
gcd() {
    local a=$1
    local b=$2
    echo "define gcd(a,b) { if (b==0) return a; return gcd(b, a%b); }; gcd($a,$b)" | bc
}

# LCM using bc for arbitrary precision
lcm() {
    local a=$1
    local b=$2
    local g
    g=$(gcd "$a" "$b")
    echo "$a * $b / $g" | bc
}

# Part 2: Navigate all nodes ending in A to nodes ending in Z simultaneously
part2() {
    # Find all starting nodes (ending in A)
    local -a start_nodes=()
    local -a start_indices=()
    for ((i = 0; i < node_count; i++)); do
        if [[ "${nodes[i]}" == *A ]]; then
            start_nodes+=("${nodes[i]}")
            start_indices+=("$i")
        fi
    done

    # Find cycle length for each starting node
    local -a cycle_lengths=()
    for ((j = 0; j < ${#start_nodes[@]}; j++)); do
        local current="${start_nodes[j]}"
        local current_idx="${start_indices[j]}"
        local steps=0

        # Navigate until we reach a node ending in Z
        while [[ "$current" != *Z ]]; do
            local idx=$((steps % instructions_len))
            local direction="${instructions:idx:1}"

            if [[ "$direction" == "L" ]]; then
                current="${lefts[current_idx]}"
            else
                current="${rights[current_idx]}"
            fi
            current_idx=$(find_node_index "$current")
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
