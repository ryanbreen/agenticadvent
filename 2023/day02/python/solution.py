from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    # Check which games are possible with 12 red, 13 green, 14 blue cubes
    max_cubes = {"red": 12, "green": 13, "blue": 14}
    possible_sum = 0

    for line in lines:
        # Parse game ID
        game_part, draws_part = line.split(": ")
        game_id = int(game_part.split(" ")[1])

        # Parse draws (semicolon-separated)
        draws = draws_part.split("; ")
        is_possible = True

        for draw in draws:
            # Parse individual cube counts in this draw
            cubes = draw.split(", ")
            for cube in cubes:
                count, color = cube.split(" ")
                count = int(count)

                # Check if this draw exceeds the maximum
                if count > max_cubes[color]:
                    is_possible = False
                    break

            if not is_possible:
                break

        if is_possible:
            possible_sum += game_id

    return possible_sum


def part2():
    # Find minimum cubes needed for each game and sum the powers
    total_power = 0

    for line in lines:
        # Parse game
        game_part, draws_part = line.split(": ")

        # Track minimum cubes needed for each color
        min_cubes = {"red": 0, "green": 0, "blue": 0}

        # Parse draws (semicolon-separated)
        draws = draws_part.split("; ")

        for draw in draws:
            # Parse individual cube counts in this draw
            cubes = draw.split(", ")
            for cube in cubes:
                count, color = cube.split(" ")
                count = int(count)

                # Track the maximum seen for each color (this is the minimum needed)
                min_cubes[color] = max(min_cubes[color], count)

        # Calculate power (product of minimums)
        power = min_cubes["red"] * min_cubes["green"] * min_cubes["blue"]
        total_power += power

    return total_power


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
