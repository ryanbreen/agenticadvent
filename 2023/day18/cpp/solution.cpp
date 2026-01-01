/**
 * Day 18: Lavaduct Lagoon
 *
 * Algorithm: Shoelace formula + Pick's theorem for polygon area calculation.
 * - Trace polygon vertices by following dig instructions
 * - Calculate interior area with Shoelace formula
 * - Use Pick's theorem to get total points: area + perimeter/2 + 1
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cstdint>
#include <cmath>
#include <unordered_map>

struct Instruction {
    char direction;
    int distance;
    std::string color;
};

std::vector<Instruction> parseInput(const std::string& filename) {
    std::vector<Instruction> instructions;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        std::istringstream iss(line);
        Instruction instr;
        std::string colorPart;

        iss >> instr.direction >> instr.distance >> colorPart;
        // Remove "(#" and ")" from color
        instr.color = colorPart.substr(2, colorPart.length() - 3);
        instructions.push_back(instr);
    }

    return instructions;
}

int64_t calculateArea(const std::vector<std::pair<int64_t, int64_t>>& vertices, int64_t perimeter) {
    // Shoelace formula for polygon area
    int64_t area = 0;
    size_t n = vertices.size();

    for (size_t i = 0; i < n; ++i) {
        size_t j = (i + 1) % n;
        area += vertices[i].first * vertices[j].second;
        area -= vertices[j].first * vertices[i].second;
    }
    area = std::abs(area) / 2;

    // Total points = interior + boundary
    // From Pick's theorem: interior = area - boundary/2 + 1
    // Total = interior + boundary = area + boundary/2 + 1
    return area + perimeter / 2 + 1;
}

int64_t part1(const std::vector<Instruction>& instructions) {
    std::unordered_map<char, std::pair<int, int>> directionMap = {
        {'R', {0, 1}},
        {'D', {1, 0}},
        {'L', {0, -1}},
        {'U', {-1, 0}}
    };

    std::vector<std::pair<int64_t, int64_t>> vertices;
    vertices.emplace_back(0, 0);
    int64_t perimeter = 0;
    int64_t r = 0, c = 0;

    for (const auto& instr : instructions) {
        auto [dr, dc] = directionMap[instr.direction];
        r += static_cast<int64_t>(dr) * instr.distance;
        c += static_cast<int64_t>(dc) * instr.distance;
        vertices.emplace_back(r, c);
        perimeter += instr.distance;
    }

    return calculateArea(vertices, perimeter);
}

int64_t part2(const std::vector<Instruction>& instructions) {
    // Last digit of hex: 0=R, 1=D, 2=L, 3=U
    // First 5 digits: distance in hex
    std::unordered_map<char, std::pair<int, int>> directionMap = {
        {'0', {0, 1}},   // R
        {'1', {1, 0}},   // D
        {'2', {0, -1}},  // L
        {'3', {-1, 0}}   // U
    };

    std::vector<std::pair<int64_t, int64_t>> vertices;
    vertices.emplace_back(0, 0);
    int64_t perimeter = 0;
    int64_t r = 0, c = 0;

    for (const auto& instr : instructions) {
        // Parse first 5 hex digits as distance
        std::string distHex = instr.color.substr(0, 5);
        int64_t distance = std::stoll(distHex, nullptr, 16);

        // Last digit is direction
        char dirCode = instr.color[5];
        auto [dr, dc] = directionMap[dirCode];

        r += static_cast<int64_t>(dr) * distance;
        c += static_cast<int64_t>(dc) * distance;
        vertices.emplace_back(r, c);
        perimeter += distance;
    }

    return calculateArea(vertices, perimeter);
}

int main() {
    auto instructions = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(instructions) << std::endl;
    std::cout << "Part 2: " << part2(instructions) << std::endl;

    return 0;
}
