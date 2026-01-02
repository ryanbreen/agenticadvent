#!/usr/bin/env python3
"""Day 20: Pulse Propagation - Module communication simulation."""

from pathlib import Path
from collections import deque
from math import lcm


def parse_input(filename: str) -> dict:
    """Parse module configuration from input."""
    modules = {}

    for line in Path(filename).read_text().strip().split('\n'):
        name_part, dest_part = line.split(' -> ')
        destinations = [d.strip() for d in dest_part.split(',')]

        if name_part == 'broadcaster':
            modules['broadcaster'] = {'type': 'broadcaster', 'destinations': destinations}
        elif name_part.startswith('%'):
            name = name_part[1:]
            modules[name] = {'type': 'flip-flop', 'destinations': destinations, 'state': False}
        elif name_part.startswith('&'):
            name = name_part[1:]
            modules[name] = {'type': 'conjunction', 'destinations': destinations, 'memory': {}}

    # Initialize conjunction memory for all inputs
    for name, module in modules.items():
        for dest in module['destinations']:
            if dest in modules and modules[dest]['type'] == 'conjunction':
                modules[dest]['memory'][name] = False  # False = low pulse

    return modules


def simulate_button_press(modules: dict, button_press: int = 0, watch_nodes: set = None) -> tuple[int, int, set]:
    """
    Simulate a single button press.
    Returns (low_count, high_count, nodes_that_sent_high_to_watch)
    """
    low_count = 0
    high_count = 0
    high_senders = set()

    # Queue: (source, destination, pulse) where pulse is True for high, False for low
    queue = deque([('button', 'broadcaster', False)])

    while queue:
        source, dest, pulse = queue.popleft()

        if pulse:
            high_count += 1
        else:
            low_count += 1

        # Track if watched nodes send high pulses
        if watch_nodes and source in watch_nodes and pulse:
            high_senders.add(source)

        if dest not in modules:
            continue

        module = modules[dest]

        if module['type'] == 'broadcaster':
            for next_dest in module['destinations']:
                queue.append((dest, next_dest, pulse))

        elif module['type'] == 'flip-flop':
            if not pulse:  # Only react to low pulses
                module['state'] = not module['state']
                for next_dest in module['destinations']:
                    queue.append((dest, next_dest, module['state']))

        elif module['type'] == 'conjunction':
            module['memory'][source] = pulse
            # Send low if all inputs are high, otherwise send high
            output = not all(module['memory'].values())
            for next_dest in module['destinations']:
                queue.append((dest, next_dest, output))

    return low_count, high_count, high_senders


def part1(modules: dict) -> int:
    """Part 1: Count pulses after 1000 button presses."""
    # Reset state
    for module in modules.values():
        if module['type'] == 'flip-flop':
            module['state'] = False
        elif module['type'] == 'conjunction':
            for key in module['memory']:
                module['memory'][key] = False

    total_low = 0
    total_high = 0

    for _ in range(1000):
        low, high, _ = simulate_button_press(modules)
        total_low += low
        total_high += high

    return total_low * total_high


def part2(modules: dict) -> int:
    """
    Part 2: Find minimum button presses for rx to receive a low pulse.

    rx receives from zh (conjunction). For zh to send low, all its inputs
    must have sent high. Find the cycle length for each input and compute LCM.
    """
    # Reset state
    for module in modules.values():
        if module['type'] == 'flip-flop':
            module['state'] = False
        elif module['type'] == 'conjunction':
            for key in module['memory']:
                module['memory'][key] = False

    # Find the module that feeds into rx
    rx_input = None
    for name, module in modules.items():
        if 'rx' in module['destinations']:
            rx_input = name
            break

    if rx_input is None:
        return 0

    # Find all modules that feed into rx_input
    watch_nodes = set(modules[rx_input]['memory'].keys())
    cycle_lengths = {}

    button_press = 0
    while len(cycle_lengths) < len(watch_nodes):
        button_press += 1
        _, _, high_senders = simulate_button_press(modules, button_press, watch_nodes)

        for node in high_senders:
            if node not in cycle_lengths:
                cycle_lengths[node] = button_press

    # LCM of all cycle lengths
    result = 1
    for length in cycle_lengths.values():
        result = lcm(result, length)

    return result


def main():
    modules = parse_input(Path(__file__).parent.parent / "input.txt")
    print(f"Part 1: {part1(modules)}")

    # Re-parse for part 2 (fresh state)
    modules = parse_input(Path(__file__).parent.parent / "input.txt")
    print(f"Part 2: {part2(modules)}")


if __name__ == "__main__":
    main()
