#!/usr/bin/env bash
# Advent of Code 2023 Day 12: Hot Springs
# Iterative DP approach (avoids subshell spawning for performance)

# Global variables for current problem
declare -a groups
declare -A memo
pattern=""
pattern_len=0
num_groups=0

# Count arrangements using memoized iterative DP
# Uses a stack-based approach to avoid subshell spawning
count_arrangements_iterative() {
    local pat="$1"
    local groups_str="$2"

    # Set globals
    pattern="$pat"
    pattern_len=${#pattern}
    IFS=',' read -ra groups <<< "$groups_str"
    num_groups=${#groups[@]}
    memo=()

    # Use stack-based simulation of recursion
    # Stack entries: "pos,group_idx,current_run,return_slot"
    # Results stored in result_stack array
    declare -a stack
    declare -A results
    local stack_idx=0
    local result_id=0

    # Push initial call
    stack[stack_idx]="0,0,0,main"
    ((stack_idx++))

    while (( stack_idx > 0 )); do
        ((stack_idx--))
        local entry="${stack[$stack_idx]}"
        IFS=',' read -r pos group_idx current_run return_slot <<< "$entry"

        local key="${pos},${group_idx},${current_run}"

        # Check if already computed
        if [[ -v memo["$key"] ]]; then
            results["$return_slot"]="${memo[$key]}"
            continue
        fi

        # Base case: reached end of pattern
        if (( pos == pattern_len )); then
            local val=0
            if (( group_idx == num_groups && current_run == 0 )); then
                val=1
            elif (( group_idx == num_groups - 1 && num_groups > 0 && groups[group_idx] == current_run )); then
                val=1
            fi
            memo["$key"]="$val"
            results["$return_slot"]="$val"
            continue
        fi

        local char="${pattern:$pos:1}"

        # Check if we need to compute sub-results or aggregate them
        local dot_key="${return_slot}_dot"
        local hash_key="${return_slot}_hash"

        # Check if sub-results are ready
        local have_dot=0
        local have_hash=0
        local need_dot=0
        local need_hash=0

        # Determine what we need
        if [[ "$char" == "." || "$char" == "?" ]]; then
            if (( current_run == 0 )); then
                need_dot=1
            elif (( group_idx < num_groups && groups[group_idx] == current_run )); then
                need_dot=1
            fi
        fi

        if [[ "$char" == "#" || "$char" == "?" ]]; then
            if (( group_idx < num_groups && current_run < groups[group_idx] )); then
                need_hash=1
            fi
        fi

        # Check if we have needed results
        if (( need_dot )); then
            if [[ -v results["$dot_key"] ]]; then
                have_dot=1
            fi
        else
            have_dot=1
            results["$dot_key"]=0
        fi

        if (( need_hash )); then
            if [[ -v results["$hash_key"] ]]; then
                have_hash=1
            fi
        else
            have_hash=1
            results["$hash_key"]=0
        fi

        # If we have all results, aggregate
        if (( have_dot && have_hash )); then
            local result=$((results["$dot_key"] + results["$hash_key"]))
            memo["$key"]="$result"
            results["$return_slot"]="$result"
            # Clean up intermediate results
            unset results["$dot_key"]
            unset results["$hash_key"]
        else
            # Re-push current entry to aggregate later
            stack[stack_idx]="$entry"
            ((stack_idx++))

            # Push needed sub-calls
            if (( need_dot && !have_dot )); then
                if (( current_run == 0 )); then
                    local sub_key="$((pos + 1)),${group_idx},0"
                    if [[ -v memo["$sub_key"] ]]; then
                        results["$dot_key"]="${memo[$sub_key]}"
                    else
                        stack[stack_idx]="$((pos + 1)),${group_idx},0,${dot_key}"
                        ((stack_idx++))
                    fi
                else
                    local sub_key="$((pos + 1)),$((group_idx + 1)),0"
                    if [[ -v memo["$sub_key"] ]]; then
                        results["$dot_key"]="${memo[$sub_key]}"
                    else
                        stack[stack_idx]="$((pos + 1)),$((group_idx + 1)),0,${dot_key}"
                        ((stack_idx++))
                    fi
                fi
            fi

            if (( need_hash && !have_hash )); then
                local sub_key="$((pos + 1)),${group_idx},$((current_run + 1))"
                if [[ -v memo["$sub_key"] ]]; then
                    results["$hash_key"]="${memo[$sub_key]}"
                else
                    stack[stack_idx]="$((pos + 1)),${group_idx},$((current_run + 1)),${hash_key}"
                    ((stack_idx++))
                fi
            fi
        fi
    done

    echo "${results[main]}"
}

# Process a line and return arrangement count
process_line() {
    local line="$1"
    local unfold="$2"

    # Parse line
    local pat="${line%% *}"
    local groups_str="${line##* }"

    # Unfold for Part 2
    if (( unfold == 1 )); then
        local unfolded_pattern="$pat"
        local unfolded_groups="$groups_str"
        for i in {1..4}; do
            unfolded_pattern="${unfolded_pattern}?${pat}"
            unfolded_groups="${unfolded_groups},${groups_str}"
        done
        pat="$unfolded_pattern"
        groups_str="$unfolded_groups"
    fi

    count_arrangements_iterative "$pat" "$groups_str"
}

# Main
main() {
    local input_file="../input.txt"

    if [[ ! -f "$input_file" ]]; then
        echo "Error: input file not found: $input_file" >&2
        exit 1
    fi

    local part1_total=0
    local part2_total=0

    # Part 1
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue
        local count
        count=$(process_line "$line" 0)
        part1_total=$((part1_total + count))
    done < "$input_file"

    echo "Part 1: $part1_total"

    # Part 2
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue
        local count
        count=$(process_line "$line" 1)
        part2_total=$((part2_total + count))
    done < "$input_file"

    echo "Part 2: $part2_total"
}

# Run if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    cd "$(dirname "$0")" || exit 1
    main
fi
