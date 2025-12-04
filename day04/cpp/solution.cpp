#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

// Directions for 8 neighbors (including diagonals)
const int directions[8][2] = {
    {-1, -1}, {-1, 0}, {-1, 1},
    {0, -1},           {0, 1},
    {1, -1},  {1, 0},  {1, 1}
};

// Count adjacent rolls for a given position
int countAdjacentRolls(const vector<string>& grid, int r, int c) {
    int rows = grid.size();
    int cols = grid[0].size();
    int count = 0;

    for (int i = 0; i < 8; i++) {
        int nr = r + directions[i][0];
        int nc = c + directions[i][1];

        // Check bounds
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            if (grid[nr][nc] == '@') {
                count++;
            }
        }
    }

    return count;
}

int part1(const vector<string>& lines) {
    int rows = lines.size();
    int cols = lines[0].size();
    int accessible_count = 0;

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (lines[r][c] == '@') {
                int adjacent = countAdjacentRolls(lines, r, c);
                if (adjacent < 4) {
                    accessible_count++;
                }
            }
        }
    }

    return accessible_count;
}

int part2(const vector<string>& lines) {
    // Create a mutable copy of the grid
    vector<string> grid = lines;
    int rows = grid.size();
    int cols = grid[0].size();
    int total_removed = 0;

    while (true) {
        // Find all rolls that can be removed in this iteration
        vector<pair<int, int>> removable;

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid[r][c] == '@') {
                    int adjacent = countAdjacentRolls(grid, r, c);
                    if (adjacent < 4) {
                        removable.push_back({r, c});
                    }
                }
            }
        }

        // If no rolls can be removed, we're done
        if (removable.empty()) {
            break;
        }

        // Remove all accessible rolls
        for (const auto& pos : removable) {
            grid[pos.first][pos.second] = '.';
        }

        total_removed += removable.size();
    }

    return total_removed;
}

int main() {
    // Read input file
    ifstream file("../input.txt");
    if (!file.is_open()) {
        cerr << "Error: Could not open input file" << endl;
        return 1;
    }

    vector<string> lines;
    string line;
    while (getline(file, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }
    file.close();

    if (lines.empty()) {
        cerr << "Error: No input data" << endl;
        return 1;
    }

    cout << "Part 1: " << part1(lines) << endl;
    cout << "Part 2: " << part2(lines) << endl;

    return 0;
}
