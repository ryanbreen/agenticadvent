#!/bin/bash
# Advent of Code 2023 Day 5: If You Give A Seed A Fertilizer

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Parse input into seeds and map data
# Maps are stored in arrays: map_N_dst, map_N_src, map_N_len
declare -a seeds
declare -a map_counts

parse_input() {
    local current_map=-1
    local line_num=0

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ $line_num -eq 0 ]]; then
            # Parse seeds line
            local seeds_str="${line#seeds: }"
            read -ra seeds <<< "$seeds_str"
        elif [[ $line =~ ^[a-z]+-to-[a-z]+\ map: ]]; then
            # New map section
            ((current_map++))
            map_counts[$current_map]=0
        elif [[ $line =~ ^[0-9] ]]; then
            # Map entry: dst_start src_start length
            read -r dst src len <<< "$line"
            local idx=${map_counts[$current_map]}
            eval "map_${current_map}_dst_${idx}=$dst"
            eval "map_${current_map}_src_${idx}=$src"
            eval "map_${current_map}_len_${idx}=$len"
            ((map_counts[$current_map]++))
        fi
        ((line_num++))
    done < "$INPUT_FILE"
}

# Apply a single map to a value
apply_map() {
    local value=$1
    local map_idx=$2
    local count=${map_counts[$map_idx]}

    for ((i=0; i<count; i++)); do
        local dst=$(eval echo \$map_${map_idx}_dst_${i})
        local src=$(eval echo \$map_${map_idx}_src_${i})
        local len=$(eval echo \$map_${map_idx}_len_${i})

        # Check if value is in source range
        if (( value >= src && value < src + len )); then
            echo $((dst + value - src))
            return
        fi
    done
    echo "$value"
}

# Convert seed to location through all maps
seed_to_location() {
    local value=$1
    for ((m=0; m<7; m++)); do
        value=$(apply_map "$value" "$m")
    done
    echo "$value"
}

# Part 1: Find minimum location for individual seeds
part1() {
    local min_loc=""

    for seed in "${seeds[@]}"; do
        local loc=$(seed_to_location "$seed")
        if [[ -z "$min_loc" ]] || (( loc < min_loc )); then
            min_loc=$loc
        fi
    done

    echo "$min_loc"
}

# Part 2: Process seed ranges
# Use ranges represented as "start:end" pairs
# This approach processes ranges through each map

part2() {
    # Build initial ranges from seeds (pairs of start, length)
    local ranges=()
    for ((i=0; i<${#seeds[@]}; i+=2)); do
        local start=${seeds[$i]}
        local len=${seeds[$((i+1))]}
        local end=$((start + len))
        ranges+=("$start:$end")
    done

    # Apply each of the 7 maps
    for ((m=0; m<7; m++)); do
        local new_ranges=()
        local count=${map_counts[$m]}

        # Process each range
        for range in "${ranges[@]}"; do
            local r_start="${range%%:*}"
            local r_end="${range##*:}"

            # Remaining parts after mapping
            local remaining=("$r_start:$r_end")

            for ((i=0; i<count; i++)); do
                local dst=$(eval echo \$map_${m}_dst_${i})
                local src=$(eval echo \$map_${m}_src_${i})
                local len=$(eval echo \$map_${m}_len_${i})
                local src_end=$((src + len))

                local next_remaining=()

                for rem in "${remaining[@]}"; do
                    local rem_start="${rem%%:*}"
                    local rem_end="${rem##*:}"

                    # Part before the map range (unmapped, keep for later)
                    if (( rem_start < src )); then
                        local before_end=$rem_end
                        (( before_end > src )) && before_end=$src
                        if (( rem_start < before_end )); then
                            next_remaining+=("$rem_start:$before_end")
                        fi
                    fi

                    # Part within the map range (mapped)
                    local overlap_start=$rem_start
                    (( overlap_start < src )) && overlap_start=$src
                    local overlap_end=$rem_end
                    (( overlap_end > src_end )) && overlap_end=$src_end

                    if (( overlap_start < overlap_end )); then
                        local offset=$((dst - src))
                        local mapped_start=$((overlap_start + offset))
                        local mapped_end=$((overlap_end + offset))
                        new_ranges+=("$mapped_start:$mapped_end")
                    fi

                    # Part after the map range (unmapped, keep for later)
                    if (( rem_end > src_end )); then
                        local after_start=$rem_start
                        (( after_start < src_end )) && after_start=$src_end
                        if (( after_start < rem_end )); then
                            next_remaining+=("$after_start:$rem_end")
                        fi
                    fi
                done

                remaining=("${next_remaining[@]}")
            done

            # Add remaining unmapped parts
            for rem in "${remaining[@]}"; do
                new_ranges+=("$rem")
            done
        done

        ranges=("${new_ranges[@]}")
    done

    # Find minimum start of any range
    local min_loc=""
    for range in "${ranges[@]}"; do
        local start="${range%%:*}"
        if [[ -z "$min_loc" ]] || (( start < min_loc )); then
            min_loc=$start
        fi
    done

    echo "$min_loc"
}

# Main
parse_input
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
