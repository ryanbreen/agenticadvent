#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <numeric>

struct Node {
    std::string left;
    std::string right;
};

std::pair<std::string, std::unordered_map<std::string, Node>> parseInput(const std::string& filename) {
    std::ifstream file(filename);
    std::string instructions;
    std::unordered_map<std::string, Node> network;

    std::getline(file, instructions);

    std::string line;
    std::getline(file, line); // skip empty line

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        // Parse: AAA = (BBB, CCC)
        std::string node = line.substr(0, 3);
        std::string left = line.substr(7, 3);
        std::string right = line.substr(12, 3);

        network[node] = {left, right};
    }

    return {instructions, network};
}

long long part1(const std::string& instructions, const std::unordered_map<std::string, Node>& network) {
    std::string current = "AAA";
    long long steps = 0;
    size_t instructionLen = instructions.size();

    while (current != "ZZZ") {
        char instruction = instructions[steps % instructionLen];
        const Node& node = network.at(current);
        if (instruction == 'L') {
            current = node.left;
        } else {
            current = node.right;
        }
        steps++;
    }

    return steps;
}

long long part2(const std::string& instructions, const std::unordered_map<std::string, Node>& network) {
    // Find all starting nodes (ending in 'A')
    std::vector<std::string> startingNodes;
    for (const auto& [key, value] : network) {
        if (key[2] == 'A') {
            startingNodes.push_back(key);
        }
    }

    size_t instructionLen = instructions.size();
    std::vector<long long> cycleLengths;

    // For each starting node, find steps to reach a Z node
    for (const std::string& startNode : startingNodes) {
        std::string current = startNode;
        long long steps = 0;

        while (current[2] != 'Z') {
            char instruction = instructions[steps % instructionLen];
            const Node& node = network.at(current);
            if (instruction == 'L') {
                current = node.left;
            } else {
                current = node.right;
            }
            steps++;
        }
        cycleLengths.push_back(steps);
    }

    // Find LCM of all cycle lengths
    long long result = cycleLengths[0];
    for (size_t i = 1; i < cycleLengths.size(); i++) {
        result = std::lcm(result, cycleLengths[i]);
    }

    return result;
}

int main() {
    auto [instructions, network] = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(instructions, network) << std::endl;
    std::cout << "Part 2: " << part2(instructions, network) << std::endl;

    return 0;
}
