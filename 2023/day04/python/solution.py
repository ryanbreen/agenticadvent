from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def parse_cards():
    cards = []
    for line in lines:
        _, numbers = line.split(":")
        winning_part, have_part = numbers.split("|")
        winning = set(int(x) for x in winning_part.split())
        have = set(int(x) for x in have_part.split())
        cards.append((winning, have))
    return cards


def part1():
    cards = parse_cards()
    total = 0
    for winning, have in cards:
        matches = len(winning & have)
        if matches > 0:
            total += 2 ** (matches - 1)
    return total


def part2():
    cards = parse_cards()
    matches = [len(winning & have) for winning, have in cards]
    copies = [1] * len(cards)

    for i, m in enumerate(matches):
        for j in range(i + 1, min(i + 1 + m, len(cards))):
            copies[j] += copies[i]

    return sum(copies)


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
