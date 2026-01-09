#!/usr/bin/env python3
import os
from collections import defaultdict

def parse_input(text):
    """Parse elf positions from grid."""
    elves = set()
    for r, line in enumerate(text.strip().split('\n')):
        for c, ch in enumerate(line):
            if ch == '#':
                elves.add((r, c))
    return elves

def simulate_round(elves, directions):
    """Run one round of simulation. Returns (new_elves, moved)."""
    # Direction checks: (check positions, move delta)
    dir_checks = {
        'N': ([(-1, -1), (-1, 0), (-1, 1)], (-1, 0)),
        'S': ([(1, -1), (1, 0), (1, 1)], (1, 0)),
        'W': ([(-1, -1), (0, -1), (1, -1)], (0, -1)),
        'E': ([(-1, 1), (0, 1), (1, 1)], (0, 1)),
    }
    
    # All 8 neighbors
    all_neighbors = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    
    # Phase 1: Each elf proposes a move
    proposals = {}  # elf -> proposed position
    proposal_counts = defaultdict(int)  # position -> count
    
    for elf in elves:
        r, c = elf
        
        # Check if any neighbors
        has_neighbor = any((r + dr, c + dc) in elves for dr, dc in all_neighbors)
        
        if not has_neighbor:
            continue  # Don't move
        
        # Try each direction
        for d in directions:
            checks, (dr, dc) = dir_checks[d]
            if all((r + cr, c + cc) not in elves for cr, cc in checks):
                new_pos = (r + dr, c + dc)
                proposals[elf] = new_pos
                proposal_counts[new_pos] += 1
                break
    
    # Phase 2: Execute moves (only if unique proposal)
    new_elves = set()
    moved = False
    
    for elf in elves:
        if elf in proposals:
            new_pos = proposals[elf]
            if proposal_counts[new_pos] == 1:
                new_elves.add(new_pos)
                moved = True
            else:
                new_elves.add(elf)
        else:
            new_elves.add(elf)
    
    return new_elves, moved

def bounding_rect_empty(elves):
    """Count empty tiles in bounding rectangle."""
    min_r = min(r for r, c in elves)
    max_r = max(r for r, c in elves)
    min_c = min(c for r, c in elves)
    max_c = max(c for r, c in elves)
    
    area = (max_r - min_r + 1) * (max_c - min_c + 1)
    return area - len(elves)

def part1(text):
    """Count empty tiles after 10 rounds."""
    elves = parse_input(text)
    directions = ['N', 'S', 'W', 'E']
    
    for _ in range(10):
        elves, _ = simulate_round(elves, directions)
        directions = directions[1:] + directions[:1]
    
    return bounding_rect_empty(elves)

def part2(text):
    """Find first round where no elf moves."""
    elves = parse_input(text)
    directions = ['N', 'S', 'W', 'E']
    
    round_num = 0
    while True:
        round_num += 1
        elves, moved = simulate_round(elves, directions)
        if not moved:
            return round_num
        directions = directions[1:] + directions[:1]

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')
    
    with open(input_file) as f:
        text = f.read()
    
    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
