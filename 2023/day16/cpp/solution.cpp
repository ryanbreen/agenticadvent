#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <queue>
#include <cstring>

constexpr int MAX_SIZE = 256;

// Directions: 0=right, 1=down, 2=left, 3=up
constexpr int DR[] = {0, 1, 0, -1};
constexpr int DC[] = {1, 0, -1, 0};

std::vector<std::string> parseInput(const std::string& filename) {
    std::vector<std::string> grid;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }

    return grid;
}

int countEnergized(const std::vector<std::string>& grid, int startRow, int startCol, int startDir) {
    int rows = static_cast<int>(grid.size());
    int cols = static_cast<int>(grid[0].size());

    // O(1) lookup using 3D array
    static bool visited[MAX_SIZE][MAX_SIZE][4];
    std::memset(visited, false, sizeof(visited));

    // BFS queue
    std::queue<std::tuple<int, int, int>> q;
    q.push({startRow, startCol, startDir});

    while (!q.empty()) {
        auto [r, c, d] = q.front();
        q.pop();

        // Bounds check
        if (r < 0 || r >= rows || c < 0 || c >= cols) {
            continue;
        }

        // State check with O(1) array lookup
        if (visited[r][c][d]) {
            continue;
        }
        visited[r][c][d] = true;

        char cell = grid[r][c];

        if (cell == '.') {
            q.push({r + DR[d], c + DC[d], d});
        } else if (cell == '/') {
            int nd = (d == 0) ? 3 : (d == 1) ? 2 : (d == 2) ? 1 : 0;
            q.push({r + DR[nd], c + DC[nd], nd});
        } else if (cell == '\\') {
            int nd = (d == 0) ? 1 : (d == 1) ? 0 : (d == 2) ? 3 : 2;
            q.push({r + DR[nd], c + DC[nd], nd});
        } else if (cell == '|') {
            if (d == 0 || d == 2) {
                q.push({r + DR[1], c + DC[1], 1});
                q.push({r + DR[3], c + DC[3], 3});
            } else {
                q.push({r + DR[d], c + DC[d], d});
            }
        } else if (cell == '-') {
            if (d == 1 || d == 3) {
                q.push({r + DR[0], c + DC[0], 0});
                q.push({r + DR[2], c + DC[2], 2});
            } else {
                q.push({r + DR[d], c + DC[d], d});
            }
        }
    }

    // Count unique (row, col) positions
    int count = 0;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (visited[r][c][0] || visited[r][c][1] || visited[r][c][2] || visited[r][c][3]) {
                ++count;
            }
        }
    }

    return count;
}

int part1(const std::vector<std::string>& grid) {
    return countEnergized(grid, 0, 0, 0);  // Start at (0,0) heading right
}

int part2(const std::vector<std::string>& grid) {
    int rows = static_cast<int>(grid.size());
    int cols = static_cast<int>(grid[0].size());
    int maxEnergized = 0;

    // Top row, heading down
    for (int c = 0; c < cols; ++c) {
        maxEnergized = std::max(maxEnergized, countEnergized(grid, 0, c, 1));
    }

    // Bottom row, heading up
    for (int c = 0; c < cols; ++c) {
        maxEnergized = std::max(maxEnergized, countEnergized(grid, rows - 1, c, 3));
    }

    // Left column, heading right
    for (int r = 0; r < rows; ++r) {
        maxEnergized = std::max(maxEnergized, countEnergized(grid, r, 0, 0));
    }

    // Right column, heading left
    for (int r = 0; r < rows; ++r) {
        maxEnergized = std::max(maxEnergized, countEnergized(grid, r, cols - 1, 2));
    }

    return maxEnergized;
}

int main() {
    std::vector<std::string> grid = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(grid) << std::endl;
    std::cout << "Part 2: " << part2(grid) << std::endl;

    return 0;
}
