#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <set>
#include <cmath>

struct CycleState {
    int cycle;
    int x;
};

std::vector<CycleState> simulateCpu(const std::vector<std::string>& instructions) {
    std::vector<CycleState> states;
    int x = 1;
    int cycle = 0;

    for (const auto& line : instructions) {
        if (line == "noop") {
            cycle++;
            states.push_back({cycle, x});
        } else {
            // addx V
            int v;
            std::sscanf(line.c_str(), "addx %d", &v);
            cycle++;
            states.push_back({cycle, x});
            cycle++;
            states.push_back({cycle, x});
            x += v;
        }
    }

    return states;
}

int part1(const std::vector<std::string>& instructions) {
    std::set<int> targetCycles = {20, 60, 100, 140, 180, 220};
    int total = 0;

    for (const auto& state : simulateCpu(instructions)) {
        if (targetCycles.count(state.cycle)) {
            total += state.cycle * state.x;
        }
    }

    return total;
}

std::string part2(const std::vector<std::string>& instructions) {
    std::vector<std::string> screen;
    std::string row;

    for (const auto& state : simulateCpu(instructions)) {
        int pos = (state.cycle - 1) % 40;  // CRT position in current row
        if (std::abs(pos - state.x) <= 1) {
            row += '#';
        } else {
            row += '.';
        }

        if (state.cycle % 40 == 0) {
            screen.push_back(row);
            row.clear();
        }
    }

    std::string result;
    for (size_t i = 0; i < screen.size(); i++) {
        if (i > 0) result += '\n';
        result += screen[i];
    }
    return result;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            instructions.push_back(line);
        }
    }

    std::cout << "Part 1: " << part1(instructions) << std::endl;
    std::cout << "Part 2:" << std::endl;
    std::cout << part2(instructions) << std::endl;

    return 0;
}
