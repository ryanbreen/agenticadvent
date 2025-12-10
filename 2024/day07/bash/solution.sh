#!/usr/bin/env bash

# Day 7: Bridge Repair - Fast Bash solution
# Use native bash arithmetic when possible, bc only for large numbers

# Evaluate expression left-to-right
evaluate() {
    local result="${g_nums[0]}"

    for ((i=0; i<${#g_ops[@]}; i++)); do
        local op="${g_ops[$i]}"
        local next="${g_nums[$((i+1))]}"

        if [[ "$op" == "+" ]]; then
            # Try bash arithmetic first
            if [[ "$result" =~ ^[0-9]+$ ]] && [[ "$next" =~ ^[0-9]+$ ]] && ((result < 1000000000000)) && ((next < 1000000000000)); then
                result=$((result + next))
            else
                result=$(echo "$result + $next" | bc)
            fi
        elif [[ "$op" == "*" ]]; then
            # Try bash arithmetic first
            if [[ "$result" =~ ^[0-9]+$ ]] && [[ "$next" =~ ^[0-9]+$ ]] && ((result < 10000000)) && ((next < 10000000)); then
                result=$((result * next))
            else
                result=$(echo "$result * $next" | bc)
            fi
        elif [[ "$op" == "||" ]]; then
            result="${result}${next}"
        fi
    done

    echo "$result"
}

# Generate operator combinations and test them
try_all_combinations() {
    local n_combinations=1
    local n_operators=${#g_operators[@]}

    # Calculate total combinations
    for ((i=0; i<g_n_ops; i++)); do
        n_combinations=$((n_combinations * n_operators))
    done

    # Try each combination
    for ((combo=0; combo<n_combinations; combo++)); do
        # Convert combo to base-n
        local temp=$combo
        g_ops=()

        for ((pos=0; pos<g_n_ops; pos++)); do
            local op_idx=$((temp % n_operators))
            g_ops+=("${g_operators[$op_idx]}")
            temp=$((temp / n_operators))
        done

        # Evaluate
        local result=$(evaluate)

        if [[ "$result" == "$g_target" ]]; then
            return 0
        fi
    done

    return 1
}

# Parse input and solve
solve() {
    g_operators=("$@")
    local total=0

    while IFS=': ' read -r target nums_str; do
        [[ -z "$target" ]] && continue

        g_target="$target"
        read -ra g_nums <<< "$nums_str"
        g_n_ops=$((${#g_nums[@]} - 1))

        if try_all_combinations; then
            # Add to total using bc for safety
            total=$(echo "$total + $target" | bc)
        fi
    done < ../input.txt

    echo "$total"
}

# Part 1: Only + and *
part1() {
    solve "+" "*"
}

# Part 2: +, *, and ||
part2() {
    solve "+" "*" "||"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
