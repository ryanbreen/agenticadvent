#!/bin/bash

# Day 13: Distress Signal
# Uses jq for JSON parsing and comparison - optimized version

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Part 1: Process all pairs in a single jq call
part1() {
    jq -s -R '
        def cmp($l; $r):
            if ($l | type) == "number" and ($r | type) == "number" then
                if $l < $r then -1
                elif $l > $r then 1
                else 0
                end
            elif ($l | type) == "array" and ($r | type) == "array" then
                if ($l | length) == 0 and ($r | length) == 0 then 0
                elif ($l | length) == 0 then -1
                elif ($r | length) == 0 then 1
                else
                    cmp($l[0]; $r[0]) as $first |
                    if $first != 0 then $first
                    else cmp($l[1:]; $r[1:])
                    end
                end
            elif ($l | type) == "number" then
                cmp([$l]; $r)
            else
                cmp($l; [$r])
            end;

        # Split input into pairs
        split("\n\n") |
        to_entries |
        map(
            .key as $idx |
            .value | split("\n") | map(select(. != "")) |
            if length >= 2 then
                (.[0] | fromjson) as $left |
                (.[1] | fromjson) as $right |
                if cmp($left; $right) == -1 then ($idx + 1) else 0 end
            else 0
            end
        ) |
        add
    ' "$INPUT_FILE"
}

# Part 2: Sort all packets in a single jq call
part2() {
    jq -s -R '
        def cmp($l; $r):
            if ($l | type) == "number" and ($r | type) == "number" then
                if $l < $r then -1
                elif $l > $r then 1
                else 0
                end
            elif ($l | type) == "array" and ($r | type) == "array" then
                if ($l | length) == 0 and ($r | length) == 0 then 0
                elif ($l | length) == 0 then -1
                elif ($r | length) == 0 then 1
                else
                    cmp($l[0]; $r[0]) as $first |
                    if $first != 0 then $first
                    else cmp($l[1:]; $r[1:])
                    end
                end
            elif ($l | type) == "number" then
                cmp([$l]; $r)
            else
                cmp($l; [$r])
            end;

        # Insertion sort using cmp
        def insertsort:
            reduce .[] as $item ([];
                . as $arr |
                ([$item] + $arr | indices($item)[0]) as $start |
                ($arr | length) as $len |
                # Find insertion point
                {pos: 0, arr: $arr, item: $item} |
                until(.pos >= ($arr | length) or cmp(.item; .arr[.pos]) == -1;
                    .pos += 1
                ) |
                .arr[:(.pos)] + [.item] + .arr[(.pos):]
            );

        # Parse all non-empty lines as packets
        split("\n") |
        map(select(. != "" and . != null)) |
        map(fromjson) |
        # Add divider packets
        . + [[[2]], [[6]]] |
        # Sort
        insertsort |
        # Find positions
        (to_entries | map(select(.value == [[2]])) | .[0].key + 1) as $pos1 |
        (to_entries | map(select(.value == [[6]])) | .[0].key + 1) as $pos2 |
        $pos1 * $pos2
    ' "$INPUT_FILE"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
