#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <string>

std::vector<int> parseInput(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open file " << filename << std::endl;
        exit(1);
    }

    std::vector<int> elves;
    std::string line;
    int currentTotal = 0;

    while (std::getline(file, line)) {
        if (line.empty()) {
            // End of current elf's items
            elves.push_back(currentTotal);
            currentTotal = 0;
        } else {
            currentTotal += std::stoi(line);
        }
    }

    // Don't forget the last elf if file doesn't end with blank line
    if (currentTotal > 0) {
        elves.push_back(currentTotal);
    }

    return elves;
}

int part1(const std::vector<int>& elves) {
    return *std::max_element(elves.begin(), elves.end());
}

int part2(std::vector<int> elves) {
    // Sort in descending order
    std::sort(elves.begin(), elves.end(), std::greater<int>());
    // Sum the top 3
    return elves[0] + elves[1] + elves[2];
}

int main() {
    std::vector<int> elves = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(elves) << std::endl;
    std::cout << "Part 2: " << part2(elves) << std::endl;

    return 0;
}
