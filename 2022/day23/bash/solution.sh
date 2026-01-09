#!/usr/bin/env bash
# Day 23: Unstable Diffusion - Elf spreading simulation
# Uses AWK for the heavy computation

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

awk '
BEGIN {
    elf_count = 0
}

# Parse input - record elf positions
{
    row = NR - 1
    for (col = 1; col <= length($0); col++) {
        if (substr($0, col, 1) == "#") {
            key = row "," (col - 1)
            elves[key] = 1
            elf_count++
        }
    }
}

function has_elf(r, c) {
    return ((r "," c) in elves)
}

function check_north(r, c) {
    return !has_elf(r-1, c-1) && !has_elf(r-1, c) && !has_elf(r-1, c+1)
}

function check_south(r, c) {
    return !has_elf(r+1, c-1) && !has_elf(r+1, c) && !has_elf(r+1, c+1)
}

function check_west(r, c) {
    return !has_elf(r-1, c-1) && !has_elf(r, c-1) && !has_elf(r+1, c-1)
}

function check_east(r, c) {
    return !has_elf(r-1, c+1) && !has_elf(r, c+1) && !has_elf(r+1, c+1)
}

function has_any_neighbor(r, c) {
    return has_elf(r-1, c-1) || has_elf(r-1, c) || has_elf(r-1, c+1) || \
           has_elf(r, c-1) || has_elf(r, c+1) || \
           has_elf(r+1, c-1) || has_elf(r+1, c) || has_elf(r+1, c+1)
}

function simulate_round(    key, r, c, parts, proposal_key, new_r, new_c, d, proposed, moved, count) {
    # Clear proposals
    delete proposals
    delete proposal_counts

    # Direction order for this round (0=N, 1=S, 2=W, 3=E)
    # dir_order is a global array set before calling

    # Phase 1: Each elf proposes a move
    for (key in elves) {
        split(key, parts, ",")
        r = parts[1] + 0
        c = parts[2] + 0

        # Check if any neighbors
        if (!has_any_neighbor(r, c)) {
            continue  # No neighbors, dont move
        }

        # Try each direction in order
        proposed = 0
        for (d = 0; d < 4; d++) {
            dir = dir_order[d]
            if (dir == 0 && check_north(r, c)) {
                new_r = r - 1
                new_c = c
                proposed = 1
            } else if (dir == 1 && check_south(r, c)) {
                new_r = r + 1
                new_c = c
                proposed = 1
            } else if (dir == 2 && check_west(r, c)) {
                new_r = r
                new_c = c - 1
                proposed = 1
            } else if (dir == 3 && check_east(r, c)) {
                new_r = r
                new_c = c + 1
                proposed = 1
            }
            if (proposed) {
                proposal_key = new_r "," new_c
                proposals[key] = proposal_key
                proposal_counts[proposal_key]++
                break
            }
        }
    }

    # Phase 2: Execute moves (only if unique proposal)
    delete new_elves
    moved = 0

    for (key in elves) {
        if (key in proposals) {
            proposal_key = proposals[key]
            if (proposal_counts[proposal_key] == 1) {
                new_elves[proposal_key] = 1
                moved = 1
            } else {
                new_elves[key] = 1
            }
        } else {
            new_elves[key] = 1
        }
    }

    # Copy new_elves to elves
    delete elves
    for (key in new_elves) {
        elves[key] = 1
    }

    return moved
}

function count_empty_tiles(    key, parts, r, c, min_r, max_r, min_c, max_c, first, area, count) {
    first = 1
    count = 0
    for (key in elves) {
        split(key, parts, ",")
        r = parts[1] + 0
        c = parts[2] + 0
        count++
        if (first) {
            min_r = max_r = r
            min_c = max_c = c
            first = 0
        } else {
            if (r < min_r) min_r = r
            if (r > max_r) max_r = r
            if (c < min_c) min_c = c
            if (c > max_c) max_c = c
        }
    }
    area = (max_r - min_r + 1) * (max_c - min_c + 1)
    return area - count
}

END {
    # Initial direction order: N, S, W, E
    dir_order[0] = 0  # N
    dir_order[1] = 1  # S
    dir_order[2] = 2  # W
    dir_order[3] = 3  # E

    # Part 1: Run 10 rounds
    # Save initial state for Part 2
    for (key in elves) {
        initial_elves[key] = 1
    }

    for (round = 1; round <= 10; round++) {
        simulate_round()
        # Rotate directions
        first_dir = dir_order[0]
        dir_order[0] = dir_order[1]
        dir_order[1] = dir_order[2]
        dir_order[2] = dir_order[3]
        dir_order[3] = first_dir
    }

    part1 = count_empty_tiles()
    print "Part 1:", part1

    # Part 2: Restore initial state and run until no movement
    delete elves
    for (key in initial_elves) {
        elves[key] = 1
    }

    # Reset direction order
    dir_order[0] = 0
    dir_order[1] = 1
    dir_order[2] = 2
    dir_order[3] = 3

    round = 0
    while (1) {
        round++
        moved = simulate_round()
        if (!moved) {
            break
        }
        # Rotate directions
        first_dir = dir_order[0]
        dir_order[0] = dir_order[1]
        dir_order[1] = dir_order[2]
        dir_order[2] = dir_order[3]
        dir_order[3] = first_dir
    }

    print "Part 2:", round
}
' "$INPUT_FILE"
