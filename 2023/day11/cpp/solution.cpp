#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <set>
#include <cstdint>

using namespace std;

struct Galaxy {
    int row;
    int col;
};

vector<Galaxy> parse_grid(const vector<string>& lines) {
    vector<Galaxy> galaxies;
    for (int r = 0; r < static_cast<int>(lines.size()); ++r) {
        for (int c = 0; c < static_cast<int>(lines[r].size()); ++c) {
            if (lines[r][c] == '#') {
                galaxies.push_back({r, c});
            }
        }
    }
    return galaxies;
}

pair<set<int>, set<int>> find_empty_rows_and_cols(const vector<string>& lines) {
    set<int> empty_rows;
    set<int> empty_cols;

    int rows = static_cast<int>(lines.size());
    int cols = rows > 0 ? static_cast<int>(lines[0].size()) : 0;

    // Find empty rows
    for (int r = 0; r < rows; ++r) {
        if (lines[r].find('#') == string::npos) {
            empty_rows.insert(r);
        }
    }

    // Find empty columns
    for (int c = 0; c < cols; ++c) {
        bool has_galaxy = false;
        for (int r = 0; r < rows; ++r) {
            if (lines[r][c] == '#') {
                has_galaxy = true;
                break;
            }
        }
        if (!has_galaxy) {
            empty_cols.insert(c);
        }
    }

    return {empty_rows, empty_cols};
}

int64_t calculate_distances(const vector<Galaxy>& galaxies,
                            const set<int>& empty_rows,
                            const set<int>& empty_cols,
                            int64_t expansion_factor) {
    int64_t total = 0;

    for (size_t i = 0; i < galaxies.size(); ++i) {
        for (size_t j = i + 1; j < galaxies.size(); ++j) {
            int r1 = galaxies[i].row;
            int c1 = galaxies[i].col;
            int r2 = galaxies[j].row;
            int c2 = galaxies[j].col;

            // Calculate row distance with expansion
            int min_r = min(r1, r2);
            int max_r = max(r1, r2);
            int64_t row_dist = max_r - min_r;
            for (int r = min_r; r < max_r; ++r) {
                if (empty_rows.count(r)) {
                    row_dist += expansion_factor - 1;
                }
            }

            // Calculate column distance with expansion
            int min_c = min(c1, c2);
            int max_c = max(c1, c2);
            int64_t col_dist = max_c - min_c;
            for (int c = min_c; c < max_c; ++c) {
                if (empty_cols.count(c)) {
                    col_dist += expansion_factor - 1;
                }
            }

            total += row_dist + col_dist;
        }
    }

    return total;
}

int64_t part1(const vector<string>& lines) {
    auto galaxies = parse_grid(lines);
    auto [empty_rows, empty_cols] = find_empty_rows_and_cols(lines);
    return calculate_distances(galaxies, empty_rows, empty_cols, 2);
}

int64_t part2(const vector<string>& lines) {
    auto galaxies = parse_grid(lines);
    auto [empty_rows, empty_cols] = find_empty_rows_and_cols(lines);
    return calculate_distances(galaxies, empty_rows, empty_cols, 1000000);
}

int main() {
    ifstream file("../input.txt");
    if (!file) {
        cerr << "Error: Could not open ../input.txt" << endl;
        return 1;
    }

    vector<string> lines;
    string line;
    while (getline(file, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }

    cout << "Part 1: " << part1(lines) << endl;
    cout << "Part 2: " << part2(lines) << endl;

    return 0;
}
