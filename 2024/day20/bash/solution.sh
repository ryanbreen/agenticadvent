#!/usr/bin/env bash

# Day 20: Race Condition
# Parse grid, BFS to trace path, count cheats that save >= 100 picoseconds
# Uses AWK for the heavy computation to avoid Bash slowness

INPUT_FILE="${1:-../input.txt}"

# Use AWK for the entire solution
awk '
BEGIN {
    rows = 0
}

# Read grid
{
    grid[rows] = $0
    cols = length($0)
    for (c = 1; c <= cols; c++) {
        ch = substr($0, c, 1)
        if (ch == "S") {
            start_r = rows
            start_c = c - 1  # 0-indexed
        } else if (ch == "E") {
            end_r = rows
            end_c = c - 1  # 0-indexed
        }
    }
    rows++
}

END {
    # BFS from start to trace path
    queue[0] = start_r "," start_c
    qhead = 0
    qtail = 1
    dist[start_r "," start_c] = 0

    while (qhead < qtail) {
        split(queue[qhead], curr, ",")
        qhead++
        r = curr[1]
        c = curr[2]

        if (r == end_r && c == end_c) break

        cur_dist = dist[r "," c]

        # 4 directions: up, down, left, right
        dr[0] = -1; dc[0] = 0
        dr[1] = 1;  dc[1] = 0
        dr[2] = 0;  dc[2] = -1
        dr[3] = 0;  dc[3] = 1

        for (d = 0; d < 4; d++) {
            nr = r + dr[d]
            nc = c + dc[d]
            key = nr "," nc
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (!(key in dist)) {
                    ch = substr(grid[nr], nc + 1, 1)
                    if (ch != "#") {
                        dist[key] = cur_dist + 1
                        queue[qtail++] = key
                    }
                }
            }
        }
    }

    # Collect track positions
    n = 0
    for (key in dist) {
        split(key, parts, ",")
        pos_r[n] = parts[1]
        pos_c[n] = parts[2]
        pos_d[n] = dist[key]
        n++
    }

    # Count cheats for Part 1 (max_cheat = 2)
    count1 = 0
    for (i = 0; i < n; i++) {
        r1 = pos_r[i]
        c1 = pos_c[i]
        d1 = pos_d[i]

        for (j = 0; j < n; j++) {
            r2 = pos_r[j]
            c2 = pos_c[j]

            dr_val = r2 - r1
            dc_val = c2 - c1
            if (dr_val < 0) dr_val = -dr_val
            if (dc_val < 0) dc_val = -dc_val
            cheat_cost = dr_val + dc_val

            if (cheat_cost <= 2) {
                d2 = pos_d[j]
                savings = d2 - d1 - cheat_cost
                if (savings >= 100) count1++
            }
        }
    }

    # Count cheats for Part 2 (max_cheat = 20)
    count2 = 0
    for (i = 0; i < n; i++) {
        r1 = pos_r[i]
        c1 = pos_c[i]
        d1 = pos_d[i]

        for (j = 0; j < n; j++) {
            r2 = pos_r[j]
            c2 = pos_c[j]

            dr_val = r2 - r1
            dc_val = c2 - c1
            if (dr_val < 0) dr_val = -dr_val
            if (dc_val < 0) dc_val = -dc_val
            cheat_cost = dr_val + dc_val

            if (cheat_cost <= 20) {
                d2 = pos_d[j]
                savings = d2 - d1 - cheat_cost
                if (savings >= 100) count2++
            }
        }
    }

    print "Part 1:", count1
    print "Part 2:", count2
}
' "$INPUT_FILE"
