from pathlib import Path
from collections import Counter

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input once at module level
hands: list[tuple[str, int]] = []
for line in input_text.split("\n"):
    parts = line.split()
    hands.append((parts[0], int(parts[1])))

# Card strength order (higher index = stronger)
CARD_STRENGTH = "23456789TJQKA"
CARD_STRENGTH_JOKER = "J23456789TQKA"  # J is weakest in Part 2


def classify_counts(counts: list[int]) -> int:
    """Classify hand type from sorted card counts (higher = stronger)."""
    if counts == [5]:          # Five of a kind
        return 6
    elif counts == [4, 1]:     # Four of a kind
        return 5
    elif counts == [3, 2]:     # Full house
        return 4
    elif counts == [3, 1, 1]:  # Three of a kind
        return 3
    elif counts == [2, 2, 1]:  # Two pair
        return 2
    elif counts == [2, 1, 1, 1]:  # One pair
        return 1
    else:                      # High card
        return 0


def get_hand_type(hand: str) -> int:
    """Return hand type as integer (higher = stronger)."""
    counts = sorted(Counter(hand).values(), reverse=True)
    return classify_counts(counts)


def get_hand_type_with_jokers(hand: str) -> int:
    """Return hand type with J as wildcards (higher = stronger)."""
    joker_count = hand.count('J')
    if joker_count == 0:
        return get_hand_type(hand)
    if joker_count == 5:
        return 6  # Five of a kind

    # Count non-joker cards and add jokers to the highest count
    non_jokers = [c for c in hand if c != 'J']
    counts = sorted(Counter(non_jokers).values(), reverse=True)
    counts[0] += joker_count

    return classify_counts(counts)


def hand_key(hand: str) -> tuple[int, tuple[int, ...]]:
    """Return sort key for a hand (type, then card strengths)."""
    hand_type = get_hand_type(hand)
    card_values = tuple(CARD_STRENGTH.index(c) for c in hand)
    return (hand_type, card_values)


def hand_key_with_jokers(hand: str) -> tuple[int, tuple[int, ...]]:
    """Return sort key for a hand with joker rules."""
    hand_type = get_hand_type_with_jokers(hand)
    card_values = tuple(CARD_STRENGTH_JOKER.index(c) for c in hand)
    return (hand_type, card_values)


def part1() -> int:
    """Calculate total winnings with standard rules."""
    sorted_hands = sorted(hands, key=lambda x: hand_key(x[0]))
    return sum(rank * bid for rank, (_, bid) in enumerate(sorted_hands, 1))


def part2() -> int:
    """Calculate total winnings with joker rules."""
    sorted_hands = sorted(hands, key=lambda x: hand_key_with_jokers(x[0]))
    return sum(rank * bid for rank, (_, bid) in enumerate(sorted_hands, 1))


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
