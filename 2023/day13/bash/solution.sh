#!/usr/bin/env bash

# Read all patterns from input file
read_patterns() {
    local input_file="$1"
    local -n patterns_ref=$2
    local current_pattern=()

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ -z "$line" ]]; then
            if [[ ${#current_pattern[@]} -gt 0 ]]; then
                patterns_ref+=("$(printf '%s\n' "${current_pattern[@]}")")
                current_pattern=()
            fi
        else
            current_pattern+=("$line")
        fi
    done < "$input_file"

    # Add last pattern if exists
    if [[ ${#current_pattern[@]} -gt 0 ]]; then
        patterns_ref+=("$(printf '%s\n' "${current_pattern[@]}")")
    fi
}

# Count character differences between two strings
count_differences() {
    local s1="$1"
    local s2="$2"
    local diff=0
    local len1=${#s1}
    local len2=${#s2}
    local min_len=$((len1 < len2 ? len1 : len2))

    for ((i=0; i<min_len; i++)); do
        if [[ "${s1:$i:1}" != "${s2:$i:1}" ]]; then
            ((diff++))
        fi
    done
    echo "$diff"
}

# Reverse a string
reverse_string() {
    local str="$1"
    local rev=""
    for ((i=${#str}-1; i>=0; i--)); do
        rev="${rev}${str:$i:1}"
    done
    echo "$rev"
}

# Find vertical reflection (returns columns to the left, or 0)
find_vertical_reflection() {
    local pattern="$1"
    local smudge_mode="$2"  # 0 for perfect, 1 for one smudge

    readarray -t rows <<< "$pattern"
    local width=${#rows[0]}

    for ((col=1; col<width; col++)); do
        local total_diff=0

        for row in "${rows[@]}"; do
            local left="${row:0:$col}"
            local right="${row:$col}"

            # Reverse left side
            left=$(reverse_string "$left")

            # Compare overlapping parts
            local left_len=${#left}
            local right_len=${#right}
            local min_len=$((left_len < right_len ? left_len : right_len))

            left="${left:0:$min_len}"
            right="${right:0:$min_len}"

            local diff=$(count_differences "$left" "$right")
            total_diff=$((total_diff + diff))

            # Early exit if too many differences
            if [[ $smudge_mode -eq 0 && $total_diff -gt 0 ]]; then
                break
            fi
            if [[ $smudge_mode -eq 1 && $total_diff -gt 1 ]]; then
                break
            fi
        done

        if [[ $smudge_mode -eq 0 && $total_diff -eq 0 ]]; then
            echo "$col"
            return
        fi
        if [[ $smudge_mode -eq 1 && $total_diff -eq 1 ]]; then
            echo "$col"
            return
        fi
    done

    echo "0"
}

# Find horizontal reflection (returns rows above, or 0)
find_horizontal_reflection() {
    local pattern="$1"
    local smudge_mode="$2"  # 0 for perfect, 1 for one smudge

    readarray -t rows <<< "$pattern"
    local height=${#rows[@]}

    for ((row=1; row<height; row++)); do
        local total_diff=0

        # Compare top with bottom (mirrored)
        local min_len=$(( (height - row) < row ? (height - row) : row ))

        for ((i=0; i<min_len; i++)); do
            local top_idx=$((row - 1 - i))
            local bottom_idx=$((row + i))

            local diff=$(count_differences "${rows[$top_idx]}" "${rows[$bottom_idx]}")
            total_diff=$((total_diff + diff))

            # Early exit if too many differences
            if [[ $smudge_mode -eq 0 && $total_diff -gt 0 ]]; then
                break
            fi
            if [[ $smudge_mode -eq 1 && $total_diff -gt 1 ]]; then
                break
            fi
        done

        if [[ $smudge_mode -eq 0 && $total_diff -eq 0 ]]; then
            echo "$row"
            return
        fi
        if [[ $smudge_mode -eq 1 && $total_diff -eq 1 ]]; then
            echo "$row"
            return
        fi
    done

    echo "0"
}

# Summarize a pattern
summarize_pattern() {
    local pattern="$1"
    local smudge_mode="$2"

    local v=$(find_vertical_reflection "$pattern" "$smudge_mode")
    if [[ $v -gt 0 ]]; then
        echo "$v"
        return
    fi

    local h=$(find_horizontal_reflection "$pattern" "$smudge_mode")
    echo $((h * 100))
}

# Part 1: Find perfect reflections
part1() {
    local total=0
    local pattern

    for pattern in "${patterns[@]}"; do
        local value=$(summarize_pattern "$pattern" 0)
        total=$((total + value))
    done

    echo "$total"
}

# Part 2: Find reflections with one smudge
part2() {
    local total=0
    local pattern

    for pattern in "${patterns[@]}"; do
        local value=$(summarize_pattern "$pattern" 1)
        total=$((total + value))
    done

    echo "$total"
}

# Main
declare -a patterns

main() {
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local input_file="$script_dir/../input.txt"

    # Read patterns
    read_patterns "$input_file" patterns

    # Solve
    echo "Part 1: $(part1)"
    echo "Part 2: $(part2)"
}

main
