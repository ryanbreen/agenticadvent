#!/usr/bin/env bash
set -uo pipefail

# Read the input file
input_file="../input.txt"

# Read grid into arrays
declare -a rows_data
row_count=0
col_count=0

while IFS= read -r line; do
    rows_data[$row_count]="$line"
    if (( col_count == 0 )); then
        col_count=${#line}
    fi
    ((row_count++))
done < "$input_file"

# Function to get grid value at position
get_grid() {
    local r=$1
    local c=$2
    local line="${rows_data[$r]}"
    echo "${line:$c:1}"
}

# Directions: up, down, left, right
declare -a dr=(-1 1 0 0)
declare -a dc=(0 0 -1 1)

# Find all trailheads (positions with height 0)
declare -a trailheads
for ((r=0; r<row_count; r++)); do
    for ((c=0; c<col_count; c++)); do
        val=$(get_grid $r $c)
        if [[ "$val" = "0" ]]; then
            trailheads+=("$r,$c")
        fi
    done
done

# Part 1: BFS to count reachable 9s from each trailhead
count_reachable_nines() {
    local start_r=$1
    local start_c=$2

    # Use temp files for efficiency
    local visited_file=$(mktemp)
    local nines_file=$(mktemp)
    local queue_file=$(mktemp)

    echo "$start_r,$start_c" > "$visited_file"
    echo "$start_r,$start_c" > "$queue_file"

    while [[ -s "$queue_file" ]]; do
        # Get first line from queue
        local pos=$(head -n 1 "$queue_file")
        tail -n +2 "$queue_file" > "$queue_file.tmp"
        mv "$queue_file.tmp" "$queue_file"

        IFS=',' read -r r c <<< "$pos"
        local current_height=$(get_grid $r $c)

        if [[ "$current_height" = "9" ]]; then
            # Check if this nine is already in our set
            if ! grep -qFx "$r,$c" "$nines_file" 2>/dev/null; then
                echo "$r,$c" >> "$nines_file"
            fi
            continue
        fi

        # Try all four directions
        for ((i=0; i<4; i++)); do
            local nr=$((r + dr[i]))
            local nc=$((c + dc[i]))

            if (( nr >= 0 && nr < row_count && nc >= 0 && nc < col_count )); then
                local next_height=$(get_grid $nr $nc)
                local expected_height=$((current_height + 1))

                # Check if not visited and height matches
                if ! grep -qFx "$nr,$nc" "$visited_file" && [[ "$next_height" = "$expected_height" ]]; then
                    echo "$nr,$nc" >> "$visited_file"
                    echo "$nr,$nc" >> "$queue_file"
                fi
            fi
        done
    done

    # Count the 9s found
    local count=0
    if [[ -s "$nines_file" ]]; then
        count=$(wc -l < "$nines_file")
    fi

    rm -f "$visited_file" "$nines_file" "$queue_file"
    echo $count
}

# Part 2: DFS to count distinct trails
# Uses recursive DFS with memoization
count_distinct_trails() {
    local start_r=$1
    local start_c=$2

    # Use memoization with a temp file
    local memo_file=$(mktemp)

    dfs_iter() {
        local r=$1
        local c=$2

        # Check memo
        local key="$r,$c"
        if grep -q "^$key " "$memo_file" 2>/dev/null; then
            grep "^$key " "$memo_file" | cut -d' ' -f2
            return
        fi

        local current_height=$(get_grid $r $c)

        if [[ "$current_height" = "9" ]]; then
            echo "$key 1" >> "$memo_file"
            echo 1
            return
        fi

        local total=0
        for ((i=0; i<4; i++)); do
            local nr=$((r + dr[i]))
            local nc=$((c + dc[i]))

            if (( nr >= 0 && nr < row_count && nc >= 0 && nc < col_count )); then
                local next_height=$(get_grid $nr $nc)
                local expected_height=$((current_height + 1))

                if [[ "$next_height" = "$expected_height" ]]; then
                    local count=$(dfs_iter $nr $nc)
                    total=$((total + count))
                fi
            fi
        done

        echo "$key $total" >> "$memo_file"
        echo $total
    }

    local result=$(dfs_iter $start_r $start_c)
    rm -f "$memo_file"
    echo $result
}

# Part 1
part1_total=0
for pos in "${trailheads[@]}"; do
    IFS=',' read -r r c <<< "$pos"
    count=$(count_reachable_nines $r $c)
    part1_total=$((part1_total + count))
done

echo "Part 1: $part1_total"

# Part 2
part2_total=0
for pos in "${trailheads[@]}"; do
    IFS=',' read -r r c <<< "$pos"
    count=$(count_distinct_trails $r $c)
    part2_total=$((part2_total + count))
done

echo "Part 2: $part2_total"
