#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <regex>

struct Move {
    int count;
    int from;
    int to;
};

std::pair<std::vector<std::vector<char>>, std::vector<Move>> parseInput(const std::string& filename) {
    std::ifstream file(filename);
    std::string content((std::istreambuf_iterator<char>(file)),
                        std::istreambuf_iterator<char>());

    // Split by double newline
    size_t splitPos = content.find("\n\n");
    std::string stackPart = content.substr(0, splitPos);
    std::string movePart = content.substr(splitPos + 2);

    // Parse stack lines
    std::vector<std::string> stackLines;
    std::istringstream stackStream(stackPart);
    std::string line;
    while (std::getline(stackStream, line)) {
        stackLines.push_back(line);
    }

    // Find number of stacks from the last line
    std::string numberLine = stackLines.back();
    int numStacks = 0;
    std::istringstream numStream(numberLine);
    int num;
    while (numStream >> num) {
        numStacks++;
    }

    // Parse stacks (excluding the number line)
    std::vector<std::vector<char>> stacks(numStacks);
    for (size_t i = 0; i < stackLines.size() - 1; i++) {
        const std::string& stackLine = stackLines[i];
        for (int j = 0; j < numStacks; j++) {
            int pos = 1 + j * 4;  // Position of crate letter
            if (pos < static_cast<int>(stackLine.size()) && stackLine[pos] != ' ') {
                stacks[j].push_back(stackLine[pos]);
            }
        }
    }

    // Reverse so bottom is at index 0
    for (auto& stack : stacks) {
        std::reverse(stack.begin(), stack.end());
    }

    // Parse moves
    std::vector<Move> moves;
    std::regex moveRegex(R"(move (\d+) from (\d+) to (\d+))");
    std::istringstream moveStream(movePart);
    while (std::getline(moveStream, line)) {
        std::smatch match;
        if (std::regex_match(line, match, moveRegex)) {
            Move m;
            m.count = std::stoi(match[1]);
            m.from = std::stoi(match[2]) - 1;  // 0-indexed
            m.to = std::stoi(match[3]) - 1;
            moves.push_back(m);
        }
    }

    return {stacks, moves};
}

std::string part1(std::vector<std::vector<char>> stacks, const std::vector<Move>& moves) {
    for (const auto& move : moves) {
        for (int i = 0; i < move.count; i++) {
            char crate = stacks[move.from].back();
            stacks[move.from].pop_back();
            stacks[move.to].push_back(crate);
        }
    }

    std::string result;
    for (const auto& stack : stacks) {
        if (!stack.empty()) {
            result += stack.back();
        }
    }
    return result;
}

std::string part2(std::vector<std::vector<char>> stacks, const std::vector<Move>& moves) {
    for (const auto& move : moves) {
        // Move multiple crates at once (preserve order)
        std::vector<char> crates(stacks[move.from].end() - move.count, stacks[move.from].end());
        stacks[move.from].resize(stacks[move.from].size() - move.count);
        stacks[move.to].insert(stacks[move.to].end(), crates.begin(), crates.end());
    }

    std::string result;
    for (const auto& stack : stacks) {
        if (!stack.empty()) {
            result += stack.back();
        }
    }
    return result;
}

int main() {
    auto [stacks, moves] = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(stacks, moves) << std::endl;
    std::cout << "Part 2: " << part2(stacks, moves) << std::endl;

    return 0;
}
