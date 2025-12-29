#include <iostream>
#include <fstream>
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

template<typename EndCondition>
long long navigate(const std::string& instructions,
                   const std::unordered_map<std::string, Node>& network,
                   std::string current,
                   EndCondition isEnd) {
    long long steps = 0;
    const size_t instructionLen = instructions.size();

    while (!isEnd(current)) {
        char instruction = instructions[steps % instructionLen];
        const Node& node = network.at(current);
        current = (instruction == 'L') ? node.left : node.right;
        steps++;
    }

    return steps;
}

long long part1(const std::string& instructions, const std::unordered_map<std::string, Node>& network) {
    return navigate(instructions, network, "AAA", [](const std::string& s) { return s == "ZZZ"; });
}

long long part2(const std::string& instructions, const std::unordered_map<std::string, Node>& network) {
    // Find all starting nodes (ending in 'A')
    std::vector<std::string> startingNodes;
    for (const auto& [key, value] : network) {
        if (key[2] == 'A') {
            startingNodes.push_back(key);
        }
    }

    // For each starting node, find steps to reach a Z node
    std::vector<long long> cycleLengths;
    cycleLengths.reserve(startingNodes.size());
    for (const std::string& startNode : startingNodes) {
        cycleLengths.push_back(
            navigate(instructions, network, startNode, [](const std::string& s) { return s[2] == 'Z'; })
        );
    }

    // Find LCM of all cycle lengths using std::reduce
    return std::reduce(cycleLengths.begin(), cycleLengths.end(), 1LL,
                       [](long long a, long long b) { return std::lcm(a, b); });
}

int main() {
    auto [instructions, network] = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(instructions, network) << std::endl;
    std::cout << "Part 2: " << part2(instructions, network) << std::endl;

    return 0;
}
