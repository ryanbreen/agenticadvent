#!/usr/bin/env bash

# Advent of Code 2023 - Day 4: Scratchcards

# Part 1: Calculate points based on matching numbers (2^(matches-1) per card)
# Part 2: Cascade card copies based on matches

# Read input and parse cards
declare -a matches_arr

card_index=0
while IFS= read -r line || [ -n "$line" ]; do
    # Parse line: "Card N: winning | have"
    # Remove "Card N:" prefix
    numbers="${line#*:}"

    # Split by |
    winning_part="${numbers%%|*}"
    have_part="${numbers#*|}"

    # Count matches by checking which "have" numbers are in "winning"
    # Build a pattern from winning numbers
    winning_set=" $winning_part "

    match_count=0
    for num in $have_part; do
        # Check if this number appears in winning set (with space boundaries)
        if [[ "$winning_set" == *" $num "* ]]; then
            match_count=$((match_count + 1))
        fi
    done

    matches_arr+=("$match_count")
    card_index=$((card_index + 1))
done < ../input.txt

num_cards=${#matches_arr[@]}

# Part 1: Calculate total points
part1_sum=0
for ((i=0; i<num_cards; i++)); do
    m=${matches_arr[$i]}
    if [ "$m" -gt 0 ]; then
        # Score = 2^(m-1)
        score=$((1 << (m - 1)))
        part1_sum=$((part1_sum + score))
    fi
done

# Part 2: Track card copies and cascade
# Initialize all cards with count of 1
declare -a copies
for ((i=0; i<num_cards; i++)); do
    copies+=("1")
done

# Process each card and add copies to subsequent cards
for ((i=0; i<num_cards; i++)); do
    m=${matches_arr[$i]}
    current_copies=${copies[$i]}

    # Add copies to the next m cards
    for ((j=i+1; j<i+1+m && j<num_cards; j++)); do
        copies[$j]=$((copies[$j] + current_copies))
    done
done

# Sum all copies for Part 2
part2_sum=0
for ((i=0; i<num_cards; i++)); do
    part2_sum=$((part2_sum + copies[$i]))
done

echo "Part 1: $part1_sum"
echo "Part 2: $part2_sum"
