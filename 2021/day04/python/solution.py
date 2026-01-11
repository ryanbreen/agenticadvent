#!/usr/bin/env python3
import os

def parse_input():
    input_path = os.path.join(os.path.dirname(__file__), '..', 'input.txt')
    with open(input_path) as f:
        content = f.read().strip()

    sections = content.split('\n\n')
    numbers = [int(x) for x in sections[0].split(',')]

    boards = []
    for section in sections[1:]:
        board = []
        for line in section.strip().split('\n'):
            row = [int(x) for x in line.split()]
            board.append(row)
        boards.append(board)

    return numbers, boards

def check_winner(board, marked):
    # Check rows
    for row in range(5):
        if all(marked[row][col] for col in range(5)):
            return True
    # Check columns
    for col in range(5):
        if all(marked[row][col] for row in range(5)):
            return True
    return False

def calculate_score(board, marked, last_number):
    unmarked_sum = 0
    for row in range(5):
        for col in range(5):
            if not marked[row][col]:
                unmarked_sum += board[row][col]
    return unmarked_sum * last_number

def mark_number(board, marked, number):
    for row in range(5):
        for col in range(5):
            if board[row][col] == number:
                marked[row][col] = True

def part1(numbers, boards):
    marked = [[[False] * 5 for _ in range(5)] for _ in boards]

    for number in numbers:
        for i, board in enumerate(boards):
            mark_number(board, marked[i], number)
            if check_winner(board, marked[i]):
                return calculate_score(board, marked[i], number)

    return None

def part2(numbers, boards):
    marked = [[[False] * 5 for _ in range(5)] for _ in boards]
    won = [False] * len(boards)
    last_score = None

    for number in numbers:
        for i, board in enumerate(boards):
            if won[i]:
                continue
            mark_number(board, marked[i], number)
            if check_winner(board, marked[i]):
                won[i] = True
                last_score = calculate_score(board, marked[i], number)

    return last_score

if __name__ == '__main__':
    numbers, boards = parse_input()
    print(f"Part 1: {part1(numbers, boards)}")
    print(f"Part 2: {part2(numbers, boards)}")
