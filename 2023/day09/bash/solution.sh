#!/bin/bash

# Day 9: Mirage Maintenance
# Read sequences, compute successive differences until all zeros,
# then extrapolate next (Part 1) and previous (Part 2) values.

INPUT_FILE="${1:-../input.txt}"

part1_sum=0
part2_sum=0

# Process each line of input
while IFS= read -r line || [[ -n "$line" ]]; do
    # Skip empty lines
    [[ -z "$line" ]] && continue

    # Parse the sequence into an array
    read -ra seq <<< "$line"
    n=${#seq[@]}

    # Build the pyramid of differences
    # sequences[level][index] stored as seq_L_I
    # Level 0 is the original sequence
    for ((i = 0; i < n; i++)); do
        declare "seq_0_$i=${seq[$i]}"
    done
    declare "len_0=$n"

    level=0
    current_len=$n

    # Keep computing differences until all zeros
    while true; do
        next_level=$((level + 1))
        next_len=$((current_len - 1))
        all_zero=1

        for ((i = 0; i < next_len; i++)); do
            # Get seq[level][i] and seq[level][i+1]
            eval "val1=\$seq_${level}_$i"
            eval "val2=\$seq_${level}_$((i + 1))"
            diff=$((val2 - val1))
            declare "seq_${next_level}_$i=$diff"

            if ((diff != 0)); then
                all_zero=0
            fi
        done

        declare "len_$next_level=$next_len"
        level=$next_level
        current_len=$next_len

        # Stop when all zeros or length is 1
        if ((all_zero == 1)) || ((current_len <= 1)); then
            break
        fi
    done

    max_level=$level

    # Part 1: Extrapolate next value (add last elements bottom-up)
    # Add a zero to the bottom level
    eval "bottom_len=\$len_$max_level"
    declare "seq_${max_level}_$bottom_len=0"
    declare "len_$max_level=$((bottom_len + 1))"

    # Work up from bottom
    for ((lv = max_level - 1; lv >= 0; lv--)); do
        eval "this_len=\$len_$lv"
        eval "below_len=\$len_$((lv + 1))"

        # Get last element of this level and below level
        eval "this_last=\$seq_${lv}_$((this_len - 1))"
        eval "below_last=\$seq_$((lv + 1))_$((below_len - 1))"

        new_val=$((this_last + below_last))
        declare "seq_${lv}_$this_len=$new_val"
        declare "len_$lv=$((this_len + 1))"
    done

    # Get the extrapolated next value (last element of level 0)
    eval "final_len=\$len_0"
    eval "next_val=\$seq_0_$((final_len - 1))"
    part1_sum=$((part1_sum + next_val))

    # Part 2: Extrapolate previous value (subtract first elements bottom-up)
    # We need to rebuild the sequences for part 2 (or we could save first elements)
    # Let's just save the first elements during construction

    # Rebuild sequences for part 2
    for ((i = 0; i < n; i++)); do
        declare "seq2_0_$i=${seq[$i]}"
    done
    declare "len2_0=$n"

    level=0
    current_len=$n

    while true; do
        next_level=$((level + 1))
        next_len=$((current_len - 1))
        all_zero=1

        for ((i = 0; i < next_len; i++)); do
            eval "val1=\$seq2_${level}_$i"
            eval "val2=\$seq2_${level}_$((i + 1))"
            diff=$((val2 - val1))
            declare "seq2_${next_level}_$i=$diff"

            if ((diff != 0)); then
                all_zero=0
            fi
        done

        declare "len2_$next_level=$next_len"
        level=$next_level
        current_len=$next_len

        if ((all_zero == 1)) || ((current_len <= 1)); then
            break
        fi
    done

    max_level=$level

    # For part 2, we prepend by subtracting: new_first = first[level] - first[level+1]
    # Start from bottom with 0
    prev_first=0
    for ((lv = max_level; lv >= 0; lv--)); do
        eval "this_first=\$seq2_${lv}_0"
        new_first=$((this_first - prev_first))
        prev_first=$new_first
    done

    part2_sum=$((part2_sum + prev_first))

done < "$INPUT_FILE"

echo "Part 1: $part1_sum"
echo "Part 2: $part2_sum"
