#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cstdint>

std::vector<int64_t> parseInput(const std::string& text) {
    std::vector<int64_t> numbers;
    std::istringstream iss(text);
    std::string line;
    while (std::getline(iss, line)) {
        if (!line.empty()) {
            numbers.push_back(std::stoll(line));
        }
    }
    return numbers;
}

std::vector<int64_t> mix(const std::vector<int64_t>& numbers, int times) {
    size_t n = numbers.size();

    // Store (original_index, value) pairs
    std::vector<std::pair<size_t, int64_t>> indexed;
    indexed.reserve(n);
    for (size_t i = 0; i < n; ++i) {
        indexed.emplace_back(i, numbers[i]);
    }

    for (int round = 0; round < times; ++round) {
        for (size_t origIdx = 0; origIdx < n; ++origIdx) {
            // Find current position of this element
            size_t currPos = 0;
            for (size_t i = 0; i < n; ++i) {
                if (indexed[i].first == origIdx) {
                    currPos = i;
                    break;
                }
            }

            int64_t val = indexed[currPos].second;

            // Remove from current position
            auto elem = indexed[currPos];
            indexed.erase(indexed.begin() + currPos);

            // Calculate new position (modulo n-1 because we removed the element)
            int64_t mod = static_cast<int64_t>(n - 1);
            int64_t newPos = ((static_cast<int64_t>(currPos) + val) % mod + mod) % mod;

            // Insert at new position
            indexed.insert(indexed.begin() + newPos, elem);
        }
    }

    std::vector<int64_t> result;
    result.reserve(n);
    for (const auto& p : indexed) {
        result.push_back(p.second);
    }
    return result;
}

int64_t groveCoordinates(const std::vector<int64_t>& mixed) {
    size_t n = mixed.size();

    // Find index of 0
    size_t zeroIdx = 0;
    for (size_t i = 0; i < n; ++i) {
        if (mixed[i] == 0) {
            zeroIdx = i;
            break;
        }
    }

    int64_t sum = 0;
    for (int offset : {1000, 2000, 3000}) {
        sum += mixed[(zeroIdx + offset) % n];
    }
    return sum;
}

int64_t part1(const std::string& text) {
    auto numbers = parseInput(text);
    auto mixed = mix(numbers, 1);
    return groveCoordinates(mixed);
}

int64_t part2(const std::string& text) {
    auto numbers = parseInput(text);
    const int64_t decryptionKey = 811589153;
    for (auto& num : numbers) {
        num *= decryptionKey;
    }
    auto mixed = mix(numbers, 10);
    return groveCoordinates(mixed);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error opening input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();

    std::cout << "Part 1: " << part1(text) << std::endl;
    std::cout << "Part 2: " << part2(text) << std::endl;

    return 0;
}
