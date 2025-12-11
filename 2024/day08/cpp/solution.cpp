#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <utility>

struct ParsedInput {
    int rows;
    int cols;
    std::map<char, std::vector<std::pair<int, int>>> antennas;
};

ParsedInput parse_input(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        std::cerr << "Failed to open input file: " << filename << std::endl;
        exit(1);
    }

    std::vector<std::string> grid;
    std::string line;

    while (std::getline(file, line)) {
        grid.push_back(line);
    }

    int rows = static_cast<int>(grid.size());
    int cols = rows > 0 ? static_cast<int>(grid[0].size()) : 0;

    std::map<char, std::vector<std::pair<int, int>>> antennas;

    for (int r = 0; r < rows; r++) {
        int row_size = static_cast<int>(grid[static_cast<size_t>(r)].size());
        for (int c = 0; c < row_size; c++) {
            char ch = grid[static_cast<size_t>(r)][static_cast<size_t>(c)];
            if (ch != '.') {
                antennas[ch].emplace_back(r, c);
            }
        }
    }

    return {rows, cols, std::move(antennas)};
}

int part1() {
    auto [rows, cols, antennas] = parse_input("../input.txt");

    std::set<std::pair<int, int>> antinodes;

    for (const auto& [freq, positions] : antennas) {
        int num_positions = static_cast<int>(positions.size());

        // For each pair of antennas with same frequency
        for (int i = 0; i < num_positions; i++) {
            for (int j = i + 1; j < num_positions; j++) {
                auto [r1, c1] = positions[static_cast<size_t>(i)];
                auto [r2, c2] = positions[static_cast<size_t>(j)];

                // Calculate the two antinodes
                // Antinode beyond antenna 1 (away from antenna 2)
                int ar1 = 2 * r1 - r2;
                int ac1 = 2 * c1 - c2;

                // Antinode beyond antenna 2 (away from antenna 1)
                int ar2 = 2 * r2 - r1;
                int ac2 = 2 * c2 - c1;

                // Add if within bounds
                if (ar1 >= 0 && ar1 < rows && ac1 >= 0 && ac1 < cols) {
                    antinodes.emplace(ar1, ac1);
                }
                if (ar2 >= 0 && ar2 < rows && ac2 >= 0 && ac2 < cols) {
                    antinodes.emplace(ar2, ac2);
                }
            }
        }
    }

    return static_cast<int>(antinodes.size());
}

int part2() {
    auto [rows, cols, antennas] = parse_input("../input.txt");

    std::set<std::pair<int, int>> antinodes;

    for (const auto& [freq, positions] : antennas) {
        int num_positions = static_cast<int>(positions.size());

        // For each pair of antennas with same frequency
        for (int i = 0; i < num_positions; i++) {
            for (int j = i + 1; j < num_positions; j++) {
                auto [r1, c1] = positions[static_cast<size_t>(i)];
                auto [r2, c2] = positions[static_cast<size_t>(j)];

                int dr = r2 - r1;
                int dc = c2 - c1;

                // Extend in both directions along the line
                // Direction 1: from antenna 1 towards and beyond antenna 2
                int r = r1, c = c1;
                while (r >= 0 && r < rows && c >= 0 && c < cols) {
                    antinodes.emplace(r, c);
                    r += dr;
                    c += dc;
                }

                // Direction 2: from antenna 1 away from antenna 2
                r = r1 - dr;
                c = c1 - dc;
                while (r >= 0 && r < rows && c >= 0 && c < cols) {
                    antinodes.emplace(r, c);
                    r -= dr;
                    c -= dc;
                }
            }
        }
    }

    return static_cast<int>(antinodes.size());
}

int main() {
    std::cout << "Part 1: " << part1() << std::endl;
    std::cout << "Part 2: " << part2() << std::endl;
    return 0;
}
