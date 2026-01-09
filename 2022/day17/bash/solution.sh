#!/usr/bin/env bash

# Day 17: Pyroclastic Flow - Falling rock simulation
# Uses bitmask representation for rows for better performance

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Read jet pattern
JETS=$(tr -d '\n\r' < "$INPUT_FILE")
JET_LEN=${#JETS}

# Chamber width = 7, stored as bits 0-6
WIDTH=7

# Rock shapes as bitmasks for each row (from bottom)
# Each rock shape array: rows from bottom to top
# Rock 0: ####  (at x=0: bits 0-3 set = 15)
# Rock 1: .#. / ### / .#.  (at x=0: 2, 7, 2)
# Rock 2: ..# / ..# / ###  (at x=0: 7, 4, 4)
# Rock 3: # / # / # / #    (at x=0: 1, 1, 1, 1)
# Rock 4: ## / ##          (at x=0: 3, 3)

# Heights of each rock type
declare -a ROCK_HEIGHTS=(1 3 3 4 2)

# Rock bitmasks per row at x=0 (from bottom)
get_rock_rows() {
    local rock_type=$1
    local x=$2
    case $rock_type in
        0) echo "$((15 << x))" ;;  # 1 row
        1) echo "$((2 << x)) $((7 << x)) $((2 << x))" ;;  # 3 rows
        2) echo "$((7 << x)) $((4 << x)) $((4 << x))" ;;  # 3 rows
        3) echo "$((1 << x)) $((1 << x)) $((1 << x)) $((1 << x))" ;;  # 4 rows
        4) echo "$((3 << x)) $((3 << x))" ;;  # 2 rows
    esac
}

# Check rock width for collision with right wall
get_rock_width() {
    case $1 in
        0) echo 4 ;;
        1) echo 3 ;;
        2) echo 3 ;;
        3) echo 1 ;;
        4) echo 2 ;;
    esac
}

# Grid stored as array of row bitmasks (each row is 7 bits)
declare -a grid
height=0
jet_idx=0

# Check if rock can be placed at (x, y)
can_place() {
    local rock_type=$1
    local x=$2
    local y=$3
    local rock_width
    rock_width=$(get_rock_width "$rock_type")

    # Check left wall
    if (( x < 0 )); then
        return 1
    fi

    # Check right wall
    if (( x + rock_width > WIDTH )); then
        return 1
    fi

    # Check floor
    if (( y < 0 )); then
        return 1
    fi

    # Get rock rows and check collisions
    local rock_rows
    rock_rows=$(get_rock_rows "$rock_type" "$x")
    local row_y=$y
    for row_mask in $rock_rows; do
        if (( row_y < ${#grid[@]} )); then
            local grid_row=${grid[$row_y]:-0}
            if (( (row_mask & grid_row) != 0 )); then
                return 1
            fi
        fi
        row_y=$((row_y + 1))
    done

    return 0
}

# Place rock at position
place_rock() {
    local rock_type=$1
    local x=$2
    local y=$3

    local rock_rows
    rock_rows=$(get_rock_rows "$rock_type" "$x")
    local row_y=$y
    for row_mask in $rock_rows; do
        # Extend grid if needed
        while (( row_y >= ${#grid[@]} )); do
            grid+=( 0 )
        done
        grid[$row_y]=$(( grid[$row_y] | row_mask ))
        row_y=$((row_y + 1))
    done

    # Update height
    local rock_height=${ROCK_HEIGHTS[$rock_type]}
    local new_top=$((y + rock_height))
    if (( new_top > height )); then
        height=$new_top
    fi
}

# Drop one rock
drop_rock() {
    local rock_type=$1

    # Start position: left=2, bottom=height+3
    local x=2
    local y=$((height + 3))

    while true; do
        # Jet push
        local jet="${JETS:$jet_idx:1}"
        jet_idx=$(( (jet_idx + 1) % JET_LEN ))

        if [[ "$jet" == ">" ]]; then
            local dx=1
        else
            local dx=-1
        fi

        # Try horizontal move
        if can_place "$rock_type" $((x + dx)) "$y"; then
            x=$((x + dx))
        fi

        # Try fall
        if can_place "$rock_type" "$x" $((y - 1)); then
            y=$((y - 1))
        else
            # Rock stops
            place_rock "$rock_type" "$x" "$y"
            break
        fi
    done
}

# Get profile as string for cycle detection
get_profile() {
    local profile=""
    local depth=20
    local start=$((height - 1))

    for (( i=0; i<depth && start-i>=0; i++ )); do
        local row_idx=$((start - i))
        profile+="${grid[$row_idx]:-0},"
    done
    echo "$profile"
}

# Part 1
part1() {
    grid=()
    height=0
    jet_idx=0

    for (( rock_num=0; rock_num<2022; rock_num++ )); do
        drop_rock $((rock_num % 5))
    done

    echo $height
}

# Part 2 with cycle detection
part2() {
    grid=()
    height=0
    jet_idx=0

    declare -A states
    declare -a heights_arr

    local target=1000000000000
    local rock_num=0

    while (( rock_num < target )); do
        local rock_type=$((rock_num % 5))

        drop_rock "$rock_type"
        heights_arr+=( "$height" )

        # Try cycle detection after some rocks have fallen
        if (( rock_num > 50 )); then
            local profile
            profile=$(get_profile)
            local state="$rock_type,$jet_idx,$profile"

            if [[ -n "${states[$state]}" ]]; then
                # Found a cycle!
                local cycle_start=${states[$state]}
                local cycle_len=$((rock_num - cycle_start))
                local cycle_height=$((height - heights_arr[cycle_start]))

                # Calculate final height
                local remaining=$((target - rock_num - 1))
                local full_cycles=$((remaining / cycle_len))
                local leftover=$((remaining % cycle_len))

                local final_height=$((height + full_cycles * cycle_height))
                if (( leftover > 0 )); then
                    local extra=$((heights_arr[cycle_start + leftover] - heights_arr[cycle_start]))
                    final_height=$((final_height + extra))
                fi

                echo "$final_height"
                return
            fi

            states["$state"]=$rock_num
        fi

        rock_num=$((rock_num + 1))
    done

    echo $height
}

result1=$(part1)
echo "Part 1: $result1"

result2=$(part2)
echo "Part 2: $result2"
