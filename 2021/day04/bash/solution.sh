#!/usr/bin/env bash

# Day 4: Giant Squid (Bingo)

# Declare associative arrays (requires bash 4+)
declare -A boards
declare -A marked
declare -a draw_numbers
declare -a won
num_boards=0

# Read and parse input
parse_input() {
    local first_line=1
    local board_idx=-1
    local row_idx=0

    while IFS= read -r line || [[ -n "$line" ]]; do
        if ((first_line)); then
            # First line: comma-separated draw numbers
            IFS=',' read -ra draw_numbers <<< "$line"
            first_line=0
        elif [[ -z "$line" ]]; then
            # Empty line: start new board
            ((board_idx++))
            row_idx=0
        else
            # Board row: 5 numbers
            read -ra nums <<< "$line"
            for col in 0 1 2 3 4; do
                boards[$board_idx,$row_idx,$col]=${nums[$col]}
            done
            ((row_idx++))
        fi
    done < "../input.txt"

    num_boards=$((board_idx + 1))
}

# Mark a number on all boards (uses global marked array)
mark_number() {
    local number=$1
    for ((b=0; b<num_boards; b++)); do
        for ((r=0; r<5; r++)); do
            for ((c=0; c<5; c++)); do
                if ((boards[$b,$r,$c] == number)); then
                    marked[$b,$r,$c]=1
                fi
            done
        done
    done
}

# Check if a board has won
check_winner() {
    local b=$1

    # Check rows
    for ((r=0; r<5; r++)); do
        local row_complete=1
        for ((c=0; c<5; c++)); do
            if ((marked[$b,$r,$c] == 0)); then
                row_complete=0
                break
            fi
        done
        if ((row_complete)); then
            return 0
        fi
    done

    # Check columns
    for ((c=0; c<5; c++)); do
        local col_complete=1
        for ((r=0; r<5; r++)); do
            if ((marked[$b,$r,$c] == 0)); then
                col_complete=0
                break
            fi
        done
        if ((col_complete)); then
            return 0
        fi
    done

    return 1
}

# Calculate score of a board
calculate_score() {
    local b=$1
    local last_number=$2
    local sum=0

    for ((r=0; r<5; r++)); do
        for ((c=0; c<5; c++)); do
            if ((marked[$b,$r,$c] == 0)); then
                ((sum += boards[$b,$r,$c]))
            fi
        done
    done

    echo $((sum * last_number))
}

# Reset marked array
reset_marked() {
    for ((b=0; b<num_boards; b++)); do
        for ((r=0; r<5; r++)); do
            for ((c=0; c<5; c++)); do
                marked[$b,$r,$c]=0
            done
        done
    done
}

# Part 1: Find first winning board
part1() {
    reset_marked

    for number in "${draw_numbers[@]}"; do
        mark_number "$number"
        for ((b=0; b<num_boards; b++)); do
            if check_winner "$b"; then
                calculate_score "$b" "$number"
                return
            fi
        done
    done
}

# Part 2: Find last winning board
part2() {
    reset_marked

    # Reset won tracking
    for ((b=0; b<num_boards; b++)); do
        won[$b]=0
    done

    local last_score=0

    for number in "${draw_numbers[@]}"; do
        mark_number "$number"
        for ((b=0; b<num_boards; b++)); do
            if ((won[$b] == 0)) && check_winner "$b"; then
                won[$b]=1
                last_score=$(calculate_score "$b" "$number")
            fi
        done
    done

    echo "$last_score"
}

# Parse input and run
parse_input

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
