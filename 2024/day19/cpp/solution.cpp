#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <cstdint>

std::vector<std::string> patterns;

// Count number of ways to form design from patterns using memoization
int64_t countWays(const std::string& design, size_t pos,
                  std::unordered_map<size_t, int64_t>& memo) {
    if (pos == design.length()) {
        return 1;
    }

    auto it = memo.find(pos);
    if (it != memo.end()) {
        return it->second;
    }

    int64_t total = 0;
    for (const auto& pattern : patterns) {
        size_t plen = pattern.length();
        if (pos + plen <= design.length() &&
            design.compare(pos, plen, pattern) == 0) {
            total += countWays(design, pos + plen, memo);
        }
    }

    memo[pos] = total;
    return total;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Could not open input.txt" << std::endl;
        return 1;
    }

    std::string line;

    // Parse patterns (first line)
    std::getline(file, line);
    std::stringstream ss(line);
    std::string pattern;
    while (std::getline(ss, pattern, ',')) {
        // Trim whitespace
        size_t start = pattern.find_first_not_of(" ");
        size_t end = pattern.find_last_not_of(" ");
        if (start != std::string::npos) {
            patterns.push_back(pattern.substr(start, end - start + 1));
        }
    }

    // Skip blank line
    std::getline(file, line);

    // Parse designs
    std::vector<std::string> designs;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            designs.push_back(line);
        }
    }

    int64_t part1 = 0;
    int64_t part2 = 0;

    for (const auto& design : designs) {
        std::unordered_map<size_t, int64_t> memo;
        int64_t ways = countWays(design, 0, memo);
        if (ways > 0) {
            part1++;
        }
        part2 += ways;
    }

    std::cout << "Part 1: " << part1 << std::endl;
    std::cout << "Part 2: " << part2 << std::endl;

    return 0;
}
