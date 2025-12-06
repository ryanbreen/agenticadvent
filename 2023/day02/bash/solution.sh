#!/usr/bin/env bash

# Advent of Code 2023 - Day 2: Cube Conundrum

# Part 1: Determine which games are possible with 12 red, 13 green, 14 blue cubes
# Part 2: Find the minimum cubes needed per game and sum the powers

MAX_RED=12
MAX_GREEN=13
MAX_BLUE=14

part1_sum=0
part2_sum=0

while IFS= read -r line; do
    # Extract game ID
    game_id=$(echo "$line" | sed -E 's/Game ([0-9]+):.*/\1/')

    # Extract the game data (everything after "Game X: ")
    game_data=$(echo "$line" | sed -E 's/Game [0-9]+: //')

    # Part 1: Check if game is possible
    is_possible=1

    # Part 2: Track minimum cubes needed
    min_red=0
    min_green=0
    min_blue=0

    # Split by semicolons to get each draw
    IFS=';' read -ra draws <<< "$game_data"

    for draw in "${draws[@]}"; do
        # Split by commas to get each color count
        IFS=',' read -ra cubes <<< "$draw"

        for cube in "${cubes[@]}"; do
            # Trim whitespace
            cube=$(echo "$cube" | xargs)

            # Extract count and color
            count=$(echo "$cube" | awk '{print $1}')
            color=$(echo "$cube" | awk '{print $2}')

            # Part 1: Check if draw exceeds limits
            case "$color" in
                red)
                    if [ "$count" -gt "$MAX_RED" ]; then
                        is_possible=0
                    fi
                    if [ "$count" -gt "$min_red" ]; then
                        min_red=$count
                    fi
                    ;;
                green)
                    if [ "$count" -gt "$MAX_GREEN" ]; then
                        is_possible=0
                    fi
                    if [ "$count" -gt "$min_green" ]; then
                        min_green=$count
                    fi
                    ;;
                blue)
                    if [ "$count" -gt "$MAX_BLUE" ]; then
                        is_possible=0
                    fi
                    if [ "$count" -gt "$min_blue" ]; then
                        min_blue=$count
                    fi
                    ;;
            esac
        done
    done

    # Part 1: Add game ID if possible
    if [ "$is_possible" -eq 1 ]; then
        part1_sum=$((part1_sum + game_id))
    fi

    # Part 2: Calculate power and add to sum
    power=$((min_red * min_green * min_blue))
    part2_sum=$((part2_sum + power))

done < ../input.txt

echo "Part 1: $part1_sum"
echo "Part 2: $part2_sum"
