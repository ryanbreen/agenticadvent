#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <queue>
#include <set>
#include <array>

// Directions: up, down, left, right
constexpr std::array<std::array<int, 2>, 4> DIRS = {{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}};

std::vector<std::pair<int, int>> find_trailheads(const std::vector<std::vector<int>>& grid) {
    std::vector<std::pair<int, int>> trailheads;
    const int rows = grid.size();
    const int cols = grid[0].size();

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == 0) {
                trailheads.push_back({r, c});
            }
        }
    }
    return trailheads;
}

int count_reachable_nines(const std::vector<std::vector<int>>& grid, int start_r, int start_c) {
    const int rows = grid.size();
    const int cols = grid[0].size();

    std::set<std::pair<int, int>> visited;
    std::set<std::pair<int, int>> nines;
    std::queue<std::pair<int, int>> q;

    visited.insert({start_r, start_c});
    q.push({start_r, start_c});

    while (!q.empty()) {
        const auto [r, c] = q.front();
        q.pop();

        const int current_height = grid[r][c];

        if (current_height == 9) {
            nines.insert({r, c});
            continue;
        }

        // Try all four directions
        for (const auto& dir : DIRS) {
            const int nr = r + dir[0];
            const int nc = c + dir[1];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (visited.find({nr, nc}) == visited.end()) {
                    if (grid[nr][nc] == current_height + 1) {
                        visited.insert({nr, nc});
                        q.push({nr, nc});
                    }
                }
            }
        }
    }

    return nines.size();
}

int dfs(const std::vector<std::vector<int>>& grid, int r, int c) {
    const int rows = grid.size();
    const int cols = grid[0].size();
    const int current_height = grid[r][c];

    if (current_height == 9) {
        return 1;
    }

    int total = 0;
    for (const auto& dir : DIRS) {
        const int nr = r + dir[0];
        const int nc = c + dir[1];

        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            if (grid[nr][nc] == current_height + 1) {
                total += dfs(grid, nr, nc);
            }
        }
    }

    return total;
}

int part1(const std::vector<std::vector<int>>& grid) {
    const std::vector<std::pair<int, int>> trailheads = find_trailheads(grid);
    int total_score = 0;
    for (const auto [r, c] : trailheads) {
        total_score += count_reachable_nines(grid, r, c);
    }
    return total_score;
}

int part2(const std::vector<std::vector<int>>& grid) {
    const std::vector<std::pair<int, int>> trailheads = find_trailheads(grid);
    int total_rating = 0;
    for (const auto [r, c] : trailheads) {
        total_rating += dfs(grid, r, c);
    }
    return total_rating;
}

int main() {
    std::ifstream input("../input.txt");
    std::string line;
    std::vector<std::vector<int>> grid;

    while (std::getline(input, line)) {
        std::vector<int> row;
        for (char c : line) {
            row.push_back(c - '0');
        }
        grid.push_back(row);
    }

    input.close();

    std::cout << "Part 1: " << part1(grid) << std::endl;
    std::cout << "Part 2: " << part2(grid) << std::endl;

    return 0;
}
