#!/opt/homebrew/bin/bash
# Day 23: A Long Walk - Longest path through hiking trails
# Uses graph compression (junctions) and iterative DFS with explicit backtracking
# Requires Bash 4+ for associative arrays

INPUT_FILE="${1:-../input.txt}"

# Grid storage
declare -a grid
declare -i rows=0 cols=0
declare -i start_r start_c end_r end_c

# Junctions: "r,c" -> 1
declare -A junctions

# Graph: junction edges with weights
# graph_p1["from_r,from_c to_r,to_c"] = weight (Part 1, respect slopes)
# graph_p2["from_r,from_c to_r,to_c"] = weight (Part 2, ignore slopes)
declare -A graph_p1 graph_p2

# Neighbors for a junction (space-separated "r,c" values)
declare -A neighbors_p1 neighbors_p2

# Parse input
parse_input() {
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue
        grid[rows]="$line"
        ((rows++)) || true
    done < "$INPUT_FILE"
    cols=${#grid[0]}
}

# Find start and end points
find_endpoints() {
    local c
    for ((c = 0; c < cols; c++)); do
        if [[ "${grid[0]:c:1}" == "." ]]; then
            start_r=0
            start_c=$c
            break
        fi
    done
    for ((c = 0; c < cols; c++)); do
        if [[ "${grid[rows-1]:c:1}" == "." ]]; then
            end_r=$((rows - 1))
            end_c=$c
            break
        fi
    done
}

# Find all junction points
find_junctions() {
    local r c neighbors ch nr nc dr dc

    # Add start and end
    junctions["$start_r,$start_c"]=1
    junctions["$end_r,$end_c"]=1

    # Find intersections (3+ walkable neighbors)
    for ((r = 0; r < rows; r++)); do
        for ((c = 0; c < cols; c++)); do
            ch="${grid[r]:c:1}"
            [[ "$ch" == "#" ]] && continue

            neighbors=0
            for dr in -1 0 1; do
                for dc in -1 0 1; do
                    # Only cardinal directions
                    (( (dr == 0 && dc == 0) || (dr != 0 && dc != 0) )) && continue
                    nr=$((r + dr))
                    nc=$((c + dc))
                    (( nr < 0 || nr >= rows || nc < 0 || nc >= cols )) && continue
                    [[ "${grid[nr]:nc:1}" != "#" ]] && ((neighbors++))
                done
            done

            if (( neighbors >= 3 )); then
                junctions["$r,$c"]=1
            fi
        done
    done
}

# Build compressed graph for a junction
# Uses global temp_visited to avoid local -A issues
declare -A temp_visited

build_edges_from() {
    local start_j=$1
    local respect_slopes=$2
    local graph_name=$3
    local neighbors_name=$4

    temp_visited=()
    local -a stack_pos stack_dist
    local sp=0

    IFS=',' read -r sr sc <<< "$start_j"

    stack_pos[sp]="$start_j"
    stack_dist[sp]=0
    ((sp++)) || true
    temp_visited["$start_j"]=1

    while (( sp > 0 )); do
        ((sp--)) || true
        local pos="${stack_pos[sp]}"
        local dist="${stack_dist[sp]}"

        IFS=',' read -r r c <<< "$pos"

        # If at a different junction, record edge
        if (( dist > 0 )) && [[ -n "${junctions[$pos]}" ]]; then
            local key="$start_j $pos"
            eval "$graph_name[\"\$key\"]=$dist"

            # Add to neighbors list
            local current_neighbors
            eval "current_neighbors=\"\${$neighbors_name[\"\$start_j\"]}\""
            if [[ ! " $current_neighbors " =~ " $pos " ]]; then
                eval "$neighbors_name[\"\$start_j\"]=\"\${current_neighbors:+\$current_neighbors }\$pos\""
            fi
            continue
        fi

        # Explore neighbors
        local dr dc nr nc cell
        for dr in -1 1; do
            nr=$((r + dr))
            nc=$c
            (( nr < 0 || nr >= rows )) && continue
            cell="${grid[nr]:nc:1}"
            [[ "$cell" == "#" ]] && continue
            [[ -n "${temp_visited[$nr,$nc]}" ]] && continue

            # Check slope constraints for Part 1
            if (( respect_slopes )); then
                local current_cell="${grid[r]:c:1}"
                if [[ "$current_cell" == "^" && "$dr" != "-1" ]]; then continue; fi
                if [[ "$current_cell" == "v" && "$dr" != "1" ]]; then continue; fi
                if [[ "$current_cell" == "<" || "$current_cell" == ">" ]]; then continue; fi
            fi

            temp_visited["$nr,$nc"]=1
            stack_pos[sp]="$nr,$nc"
            stack_dist[sp]=$((dist + 1))
            ((sp++)) || true
        done

        for dc in -1 1; do
            nr=$r
            nc=$((c + dc))
            (( nc < 0 || nc >= cols )) && continue
            cell="${grid[nr]:nc:1}"
            [[ "$cell" == "#" ]] && continue
            [[ -n "${temp_visited[$nr,$nc]}" ]] && continue

            # Check slope constraints for Part 1
            if (( respect_slopes )); then
                local current_cell="${grid[r]:c:1}"
                if [[ "$current_cell" == "<" && "$dc" != "-1" ]]; then continue; fi
                if [[ "$current_cell" == ">" && "$dc" != "1" ]]; then continue; fi
                if [[ "$current_cell" == "^" || "$current_cell" == "v" ]]; then continue; fi
            fi

            temp_visited["$nr,$nc"]=1
            stack_pos[sp]="$nr,$nc"
            stack_dist[sp]=$((dist + 1))
            ((sp++)) || true
        done
    done
}

# Build the complete compressed graph
build_graph() {
    local j
    for j in "${!junctions[@]}"; do
        build_edges_from "$j" 1 "graph_p1" "neighbors_p1"
        build_edges_from "$j" 0 "graph_p2" "neighbors_p2"
    done
}

# Global visited for DFS
declare -A dfs_visited

# Iterative DFS with explicit backtracking stack for longest path
longest_path() {
    local graph_name=$1
    local neighbors_name=$2

    local start="$start_r,$start_c"
    local end="$end_r,$end_c"

    # Stack entries: "action pos dist"
    # action: VISIT (explore node) or BACKTRACK (restore state)
    local -a stack
    local sp=0
    local max_dist=-999999999

    dfs_visited=()

    # Push initial state
    stack[sp]="VISIT $start 0"
    ((sp++)) || true

    while (( sp > 0 )); do
        ((sp--)) || true
        local entry="${stack[sp]}"
        local action pos dist
        read -r action pos dist <<< "$entry"

        if [[ "$action" == "BACKTRACK" ]]; then
            # Remove from visited
            unset "dfs_visited[$pos]"
            continue
        fi

        # VISIT action
        if [[ "$pos" == "$end" ]]; then
            if (( dist > max_dist )); then
                max_dist=$dist
            fi
            continue
        fi

        # Skip if already visited in current path
        [[ -n "${dfs_visited[$pos]}" ]] && continue

        # Mark visited
        dfs_visited["$pos"]=1

        # Push backtrack action
        stack[sp]="BACKTRACK $pos 0"
        ((sp++)) || true

        # Get neighbors and push them
        local neighbor_list
        eval "neighbor_list=\"\${$neighbors_name[\"\$pos\"]}\""

        local neighbor
        for neighbor in $neighbor_list; do
            [[ -n "${dfs_visited[$neighbor]}" ]] && continue

            local key="$pos $neighbor"
            local weight
            eval "weight=\"\${$graph_name[\"\$key\"]}\""

            if [[ -n "$weight" ]]; then
                stack[sp]="VISIT $neighbor $((dist + weight))"
                ((sp++)) || true
            fi
        done
    done

    echo "$max_dist"
}

# Main execution
cd "$(dirname "$0")"

parse_input
find_endpoints
find_junctions
build_graph

echo "Part 1: $(longest_path "graph_p1" "neighbors_p1")"
echo "Part 2: $(longest_path "graph_p2" "neighbors_p2")"
