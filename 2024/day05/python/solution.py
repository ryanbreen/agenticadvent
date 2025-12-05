from pathlib import Path
from collections import defaultdict

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input - split into rules and updates sections
sections = input_text.split("\n\n")
rules_section = sections[0].split("\n")
updates_section = sections[1].split("\n")

# Parse rules: X|Y means X must come before Y
# Store as: rules[X] = set of pages that must come AFTER X
rules = defaultdict(set)
for rule in rules_section:
    before, after = map(int, rule.split("|"))
    rules[before].add(after)

# Parse updates
updates = [list(map(int, line.split(","))) for line in updates_section]


def is_valid_order(update):
    """Check if an update is in valid order according to rules."""
    page_positions = {page: i for i, page in enumerate(update)}

    for i, page in enumerate(update):
        # Check all pages that must come after this page
        for must_be_after in rules[page]:
            if must_be_after in page_positions:
                if page_positions[must_be_after] < i:
                    return False
    return True


def part1():
    total = 0
    for update in updates:
        if is_valid_order(update):
            middle_idx = len(update) // 2
            total += update[middle_idx]
    return total


def fix_order(update):
    """Reorder an update to satisfy all rules using a custom comparator."""
    from functools import cmp_to_key

    def compare(a, b):
        # If a must come before b, return -1
        if b in rules[a]:
            return -1
        # If b must come before a, return 1
        if a in rules[b]:
            return 1
        return 0

    return sorted(update, key=cmp_to_key(compare))


def part2():
    total = 0
    for update in updates:
        if not is_valid_order(update):
            fixed = fix_order(update)
            middle_idx = len(fixed) // 2
            total += fixed[middle_idx]
    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
