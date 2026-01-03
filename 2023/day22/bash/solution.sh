#!/bin/bash
# Day 22: Sand Slabs - 3D falling bricks simulation

INPUT_FILE="${1:-../input.txt}"

# Associative arrays for bricks and occupied cells
declare -a brick_x1 brick_y1 brick_z1 brick_x2 brick_y2 brick_z2
declare -A height_map  # "x,y" -> max z at that position
declare -A top_brick   # "x,y" -> brick index at top of that position
declare -A supports    # brick_index -> space-separated list of bricks it supports
declare -A supporters  # brick_index -> space-separated list of bricks that support it

# Parse input
parse_input() {
    local idx=0
    while IFS='~' read -r left right || [[ -n "$left" ]]; do
        [[ -z "$left" ]] && continue

        IFS=',' read -r x1 y1 z1 <<< "$left"
        IFS=',' read -r x2 y2 z2 <<< "$right"

        # Ensure z1 <= z2
        if (( z1 > z2 )); then
            local tmp
            tmp=$x1; x1=$x2; x2=$tmp
            tmp=$y1; y1=$y2; y2=$tmp
            tmp=$z1; z1=$z2; z2=$tmp
        fi

        brick_x1[idx]=$x1
        brick_y1[idx]=$y1
        brick_z1[idx]=$z1
        brick_x2[idx]=$x2
        brick_y2[idx]=$y2
        brick_z2[idx]=$z2

        ((idx++)) || true
    done < "$INPUT_FILE"

    num_bricks=$idx
}

# Sort bricks by z1 (minimum z coordinate)
# Returns sorted indices in sorted_indices array
sort_bricks_by_z() {
    # Create array of "z1 index" pairs
    local -a pairs
    local i
    for ((i = 0; i < num_bricks; i++)); do
        pairs+=("${brick_z1[i]} $i")
    done

    # Sort and extract indices
    sorted_indices=()
    while read -r z idx; do
        sorted_indices+=("$idx")
    done < <(printf '%s\n' "${pairs[@]}" | sort -n -k1)
}

# Settle all bricks and build support relationships
settle_bricks() {
    for orig_idx in "${sorted_indices[@]}"; do
        local x1=${brick_x1[orig_idx]}
        local y1=${brick_y1[orig_idx]}
        local z1=${brick_z1[orig_idx]}
        local x2=${brick_x2[orig_idx]}
        local y2=${brick_y2[orig_idx]}
        local z2=${brick_z2[orig_idx]}

        # Find minimum x, y and maximum x, y
        local min_x=$x1 max_x=$x2
        local min_y=$y1 max_y=$y2
        (( x1 > x2 )) && { min_x=$x2; max_x=$x1; }
        (( y1 > y2 )) && { min_y=$y2; max_y=$y1; }

        # Find the maximum height in the footprint
        local max_height=0
        local x y
        for ((x = min_x; x <= max_x; x++)); do
            for ((y = min_y; y <= max_y; y++)); do
                local key="$x,$y"
                local h=${height_map[$key]:-0}
                (( h > max_height )) && max_height=$h
            done
        done

        # Calculate new z positions (brick rests on max_height + 1)
        local brick_height=$((z2 - z1))
        local new_z1=$((max_height + 1))
        local new_z2=$((new_z1 + brick_height))

        # Update brick positions
        brick_z1[orig_idx]=$new_z1
        brick_z2[orig_idx]=$new_z2

        # Find supporters and update height map
        local supporter_list=""
        for ((x = min_x; x <= max_x; x++)); do
            for ((y = min_y; y <= max_y; y++)); do
                local key="$x,$y"
                local h=${height_map[$key]:-0}

                # If this cell is at max_height and has a brick, it's a supporter
                if (( h == max_height && h > 0 )); then
                    local supporter_idx="${top_brick[$key]}"
                    if [[ -n "$supporter_idx" ]]; then
                        # Add to supporter list if not already there
                        if [[ ! " $supporter_list " =~ " $supporter_idx " ]]; then
                            supporter_list="$supporter_list $supporter_idx"
                        fi
                    fi
                fi

                # Update height map and top brick
                height_map[$key]=$new_z2
                top_brick[$key]=$orig_idx
            done
        done

        # Store supporters for this brick
        supporters[$orig_idx]="${supporter_list# }"

        # Update supports relationship (reverse direction)
        for supporter_idx in $supporter_list; do
            local current_supports="${supports[$supporter_idx]}"
            if [[ ! " $current_supports " =~ " $orig_idx " ]]; then
                supports[$supporter_idx]="${current_supports:+$current_supports }$orig_idx"
            fi
        done
    done
}

# Part 1: Count bricks that can be safely disintegrated
part1() {
    local safe_count=0
    local i

    for ((i = 0; i < num_bricks; i++)); do
        local can_remove=1
        local supported_list="${supports[$i]}"

        for supported in $supported_list; do
            # Count supporters of the supported brick
            local supporter_list="${supporters[$supported]}"
            local count=0
            for s in $supporter_list; do
                ((count++)) || true
            done

            if (( count == 1 )); then
                can_remove=0
                break
            fi
        done

        if (( can_remove )); then
            ((safe_count++)) || true
        fi
    done

    echo "$safe_count"
}

# Part 2: Count total bricks that would fall for each disintegration
part2() {
    local total_falls=0
    local i

    for ((i = 0; i < num_bricks; i++)); do
        # Track falling bricks using an associative array
        declare -A falling
        falling[$i]=1

        # Use array as queue for BFS
        local -a queue
        queue=("$i")
        local head=0

        while (( head < ${#queue[@]} )); do
            local brick="${queue[head]}"
            ((head++)) || true

            # Check all bricks that this brick supports
            local supported_list="${supports[$brick]}"
            for supported in $supported_list; do
                [[ -n "${falling[$supported]}" ]] && continue

                # Check if all supporters have fallen
                local all_fallen=1
                local supporter_list="${supporters[$supported]}"
                for s in $supporter_list; do
                    if [[ -z "${falling[$s]}" ]]; then
                        all_fallen=0
                        break
                    fi
                done

                if (( all_fallen )); then
                    falling[$supported]=1
                    queue+=("$supported")
                fi
            done
        done

        # Count fallen bricks (excluding the initial one)
        local fall_count=0
        for k in "${!falling[@]}"; do
            ((fall_count++)) || true
        done
        ((total_falls += fall_count - 1)) || true

        unset falling
    done

    echo "$total_falls"
}

# Main execution
cd "$(dirname "$0")"

parse_input
sort_bricks_by_z
settle_bricks

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
