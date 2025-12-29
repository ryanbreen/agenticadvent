from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

def parse_input(text):
    lines = text.split('\n')
    return [[int(x) for x in line.split()] for line in lines]

def get_differences(seq):
    return [seq[i+1] - seq[i] for i in range(len(seq) - 1)]

def extrapolate_next(seq):
    sequences = [seq]
    current = seq
    while not all(x == 0 for x in current):
        current = get_differences(current)
        sequences.append(current)

    for i in range(len(sequences) - 2, -1, -1):
        sequences[i].append(sequences[i][-1] + sequences[i+1][-1])

    return sequences[0][-1]

def extrapolate_prev(seq):
    sequences = [seq]
    current = seq
    while not all(x == 0 for x in current):
        current = get_differences(current)
        sequences.append(current)

    for i in range(len(sequences) - 2, -1, -1):
        sequences[i].insert(0, sequences[i][0] - sequences[i+1][0])

    return sequences[0][0]

histories = parse_input(input_text)

def part1():
    return sum(extrapolate_next(h[:]) for h in histories)

def part2():
    return sum(extrapolate_prev(h[:]) for h in histories)


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
