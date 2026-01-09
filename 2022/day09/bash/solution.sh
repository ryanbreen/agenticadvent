#!/bin/bash

# Day 9: Rope Bridge

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Sign function: returns -1, 0, or 1
sign() {
    local x=$1
    if (( x > 0 )); then
        echo 1
    elif (( x < 0 )); then
        echo -1
    else
        echo 0
    fi
}

# Simulate rope with given length
# Returns count of unique positions visited by tail
simulate_rope() {
    local rope_length=$1

    # Initialize knots (x,y pairs stored as separate arrays)
    declare -a knot_x
    declare -a knot_y
    for (( i=0; i<rope_length; i++ )); do
        knot_x[i]=0
        knot_y[i]=0
    done

    # Track visited positions using associative array as set
    declare -A visited
    visited["0,0"]=1

    # Direction mappings
    declare -A dir_dx dir_dy
    dir_dx[U]=0; dir_dy[U]=1
    dir_dx[D]=0; dir_dy[D]=-1
    dir_dx[L]=-1; dir_dy[L]=0
    dir_dx[R]=1; dir_dy[R]=0

    # Read and process moves
    while read -r direction count; do
        local dx=${dir_dx[$direction]}
        local dy=${dir_dy[$direction]}

        for (( step=0; step<count; step++ )); do
            # Move head
            (( knot_x[0] += dx ))
            (( knot_y[0] += dy ))

            # Move each subsequent knot
            for (( i=1; i<rope_length; i++ )); do
                local prev=$((i-1))
                local delta_x=$((knot_x[prev] - knot_x[i]))
                local delta_y=$((knot_y[prev] - knot_y[i]))

                # Check if adjacent or overlapping
                local abs_dx=${delta_x#-}
                local abs_dy=${delta_y#-}

                if (( abs_dx <= 1 && abs_dy <= 1 )); then
                    # Adjacent, don't move this or subsequent knots
                    break
                fi

                # Move toward previous knot
                local sx=$(sign $delta_x)
                local sy=$(sign $delta_y)
                (( knot_x[i] += sx ))
                (( knot_y[i] += sy ))
            done

            # Record tail position
            local tail_idx=$((rope_length - 1))
            visited["${knot_x[tail_idx]},${knot_y[tail_idx]}"]=1
        done
    done < "$INPUT_FILE"

    echo ${#visited[@]}
}

part1() {
    simulate_rope 2
}

part2() {
    simulate_rope 10
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
