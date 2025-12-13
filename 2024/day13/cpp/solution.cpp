#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <regex>
#include <optional>

struct Machine {
    long long ax, ay, bx, by, px, py;
};

std::vector<Machine> parseMachines(const std::string& text) {
    std::vector<Machine> machines;
    std::regex button_a_regex(R"(Button A: X\+(\d+), Y\+(\d+))");
    std::regex button_b_regex(R"(Button B: X\+(\d+), Y\+(\d+))");
    std::regex prize_regex(R"(Prize: X=(\d+), Y=(\d+))");

    // Split by double newline
    size_t pos = 0;
    std::string block;
    std::string input = text;

    while (pos < input.length()) {
        size_t next_pos = input.find("\n\n", pos);
        if (next_pos == std::string::npos) {
            block = input.substr(pos);
            pos = input.length();
        } else {
            block = input.substr(pos, next_pos - pos);
            pos = next_pos + 2;
        }

        if (block.empty()) continue;

        // Parse the three lines in this block
        std::smatch match;
        Machine m;

        // Find Button A
        if (std::regex_search(block, match, button_a_regex)) {
            m.ax = std::stoll(match[1].str());
            m.ay = std::stoll(match[2].str());
        }

        // Find Button B
        if (std::regex_search(block, match, button_b_regex)) {
            m.bx = std::stoll(match[1].str());
            m.by = std::stoll(match[2].str());
        }

        // Find Prize
        if (std::regex_search(block, match, prize_regex)) {
            m.px = std::stoll(match[1].str());
            m.py = std::stoll(match[2].str());
        }

        machines.push_back(m);
    }

    return machines;
}

std::optional<long long> solveMachine(long long ax, long long ay, long long bx, long long by,
                                      long long px, long long py, long long max_presses = -1) {
    /*
     * Solve using Cramer's rule:
     * a*ax + b*bx = px
     * a*ay + b*by = py
     *
     * det = ax*by - ay*bx
     * a = (px*by - py*bx) / det
     * b = (ax*py - ay*px) / det
     */

    long long det = ax * by - ay * bx;

    if (det == 0) {
        return std::nullopt;  // No unique solution
    }

    // Calculate using integer arithmetic
    long long a_num = px * by - py * bx;
    long long b_num = ax * py - ay * px;

    // Check if solutions are integers
    if (a_num % det != 0 || b_num % det != 0) {
        return std::nullopt;
    }

    long long a = a_num / det;
    long long b = b_num / det;

    // Check non-negative
    if (a < 0 || b < 0) {
        return std::nullopt;
    }

    // Check max presses constraint (Part 1)
    if (max_presses != -1 && (a > max_presses || b > max_presses)) {
        return std::nullopt;
    }

    return 3 * a + b;
}

long long part1(const std::vector<Machine>& machines) {
    long long total = 0;

    for (const auto& m : machines) {
        auto cost = solveMachine(m.ax, m.ay, m.bx, m.by, m.px, m.py, 100);
        if (cost.has_value()) {
            total += cost.value();
        }
    }

    return total;
}

long long part2(const std::vector<Machine>& machines) {
    long long total = 0;
    long long offset = 10000000000000LL;

    for (const auto& m : machines) {
        auto cost = solveMachine(m.ax, m.ay, m.bx, m.by,
                                 m.px + offset, m.py + offset, -1);
        if (cost.has_value()) {
            total += cost.value();
        }
    }

    return total;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }

    std::string input((std::istreambuf_iterator<char>(file)),
                      std::istreambuf_iterator<char>());
    file.close();

    // Trim trailing whitespace
    while (!input.empty() && (input.back() == '\n' || input.back() == ' ')) {
        input.pop_back();
    }

    std::vector<Machine> machines = parseMachines(input);

    std::cout << "Part 1: " << part1(machines) << std::endl;
    std::cout << "Part 2: " << part2(machines) << std::endl;

    return 0;
}
