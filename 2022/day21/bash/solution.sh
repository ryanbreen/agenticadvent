#!/usr/bin/env bash

# Day 21: Monkey Math
# Uses associative arrays and bc for arbitrary precision arithmetic

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Declare associative arrays
declare -A monkey_value      # Stores numeric values for monkeys
declare -A monkey_left       # Left operand for operation monkeys
declare -A monkey_op         # Operator for operation monkeys
declare -A monkey_right      # Right operand for operation monkeys
declare -A eval_cache        # Memoization cache for evaluate
declare -A humn_cache        # Memoization cache for contains_humn

# Parse input and populate associative arrays
parse_input() {
    while IFS=': ' read -r name job; do
        if [[ $job =~ ^-?[0-9]+$ ]]; then
            monkey_value["$name"]="$job"
        else
            read -r left op right <<< "$job"
            monkey_left["$name"]="$left"
            monkey_op["$name"]="$op"
            monkey_right["$name"]="$right"
        fi
    done < "$INPUT_FILE"
}

# Evaluate a monkey's value recursively with memoization
evaluate() {
    local name="$1"

    # Check cache
    if [[ -n "${eval_cache[$name]}" ]]; then
        echo "${eval_cache[$name]}"
        return
    fi

    # If it's a number monkey
    if [[ -n "${monkey_value[$name]}" ]]; then
        echo "${monkey_value[$name]}"
        return
    fi

    # It's an operation monkey
    local left="${monkey_left[$name]}"
    local op="${monkey_op[$name]}"
    local right="${monkey_right[$name]}"

    local left_val=$(evaluate "$left")
    local right_val=$(evaluate "$right")

    local result
    case "$op" in
        '+') result=$(echo "$left_val + $right_val" | bc) ;;
        '-') result=$(echo "$left_val - $right_val" | bc) ;;
        '*') result=$(echo "$left_val * $right_val" | bc) ;;
        '/') result=$(echo "$left_val / $right_val" | bc) ;;
    esac

    eval_cache["$name"]="$result"
    echo "$result"
}

# Check if a monkey's evaluation tree contains 'humn'
contains_humn() {
    local name="$1"

    # Check cache
    if [[ -n "${humn_cache[$name]+isset}" ]]; then
        echo "${humn_cache[$name]}"
        return
    fi

    if [[ "$name" == "humn" ]]; then
        humn_cache["$name"]=1
        echo 1
        return
    fi

    # If it's a number monkey (and not humn)
    if [[ -n "${monkey_value[$name]}" ]]; then
        humn_cache["$name"]=0
        echo 0
        return
    fi

    # It's an operation monkey
    local left="${monkey_left[$name]}"
    local right="${monkey_right[$name]}"

    local left_has=$(contains_humn "$left")
    if [[ "$left_has" == "1" ]]; then
        humn_cache["$name"]=1
        echo 1
        return
    fi

    local right_has=$(contains_humn "$right")
    if [[ "$right_has" == "1" ]]; then
        humn_cache["$name"]=1
        echo 1
        return
    fi

    humn_cache["$name"]=0
    echo 0
}

# Solve for humn value given that 'name' should equal 'target'
solve_for_humn() {
    local name="$1"
    local target="$2"

    if [[ "$name" == "humn" ]]; then
        echo "$target"
        return
    fi

    # If it's a number monkey (shouldn't happen in valid path)
    if [[ -n "${monkey_value[$name]}" ]]; then
        return
    fi

    local left="${monkey_left[$name]}"
    local op="${monkey_op[$name]}"
    local right="${monkey_right[$name]}"

    local left_has_humn=$(contains_humn "$left")

    if [[ "$left_has_humn" == "1" ]]; then
        # Left side contains humn, evaluate right side
        local right_val=$(evaluate "$right")
        local new_target
        case "$op" in
            '+') new_target=$(echo "$target - $right_val" | bc) ;;
            '-') new_target=$(echo "$target + $right_val" | bc) ;;
            '*') new_target=$(echo "$target / $right_val" | bc) ;;
            '/') new_target=$(echo "$target * $right_val" | bc) ;;
        esac
        solve_for_humn "$left" "$new_target"
    else
        # Right side contains humn, evaluate left side
        local left_val=$(evaluate "$left")
        local new_target
        case "$op" in
            '+') new_target=$(echo "$target - $left_val" | bc) ;;
            '-') new_target=$(echo "$left_val - $target" | bc) ;;
            '*') new_target=$(echo "$target / $left_val" | bc) ;;
            '/') new_target=$(echo "$left_val / $target" | bc) ;;
        esac
        solve_for_humn "$right" "$new_target"
    fi
}

part1() {
    evaluate "root"
}

part2() {
    local left="${monkey_left[root]}"
    local right="${monkey_right[root]}"

    local left_has_humn=$(contains_humn "$left")

    if [[ "$left_has_humn" == "1" ]]; then
        local target=$(evaluate "$right")
        solve_for_humn "$left" "$target"
    else
        local target=$(evaluate "$left")
        solve_for_humn "$right" "$target"
    fi
}

# Main
parse_input
echo "Part 1: $(part1)"

# Clear eval cache for part 2 (not strictly needed but cleaner)
eval_cache=()

echo "Part 2: $(part2)"
