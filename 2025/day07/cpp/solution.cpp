#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>
#include <map>

using namespace std;

vector<string> readInput() {
    ifstream file("../input.txt");
    vector<string> lines;
    string line;

    while (getline(file, line)) {
        lines.push_back(line);
    }

    return lines;
}

int part1(const vector<string>& lines) {
    int rows = lines.size();
    if (rows == 0) return 0;
    int cols = lines[0].size();

    // Find starting position S
    int start_col = -1;
    for (int col = 0; col < cols; col++) {
        if (lines[0][col] == 'S') {
            start_col = col;
            break;
        }
    }

    if (start_col == -1) {
        return 0;
    }

    // Track active beam columns at each row
    // Use a set to handle beam merging
    set<int> active_beams;
    active_beams.insert(start_col);
    int split_count = 0;

    // Process row by row starting from row 1 (below S)
    for (int row = 1; row < rows; row++) {
        set<int> new_beams;

        for (int col : active_beams) {
            if (col >= 0 && col < cols) {
                char cell = lines[row][col];
                if (cell == '^') {
                    // Beam hits splitter - count it and emit left/right
                    split_count++;
                    // Left beam goes to col-1, right beam goes to col+1
                    if (col - 1 >= 0) {
                        new_beams.insert(col - 1);
                    }
                    if (col + 1 < cols) {
                        new_beams.insert(col + 1);
                    }
                } else if (cell == '.') {
                    // Beam continues straight down
                    new_beams.insert(col);
                } else {
                    // If cell is something else (like S), beam continues
                    new_beams.insert(col);
                }
            }
        }

        active_beams = new_beams;

        // If no more beams, stop
        if (active_beams.empty()) {
            break;
        }
    }

    return split_count;
}

unsigned long long part2(const vector<string>& lines) {
    int rows = lines.size();
    if (rows == 0) return 0;
    int cols = lines[0].size();

    // Find starting position S
    int start_col = -1;
    for (int col = 0; col < cols; col++) {
        if (lines[0][col] == 'S') {
            start_col = col;
            break;
        }
    }

    if (start_col == -1) {
        return 0;
    }

    // Track number of timelines at each column position
    // Use a map: col -> count of timelines at that position
    map<int, unsigned long long> timelines;
    timelines[start_col] = 1;

    // Process row by row starting from row 1 (below S)
    for (int row = 1; row < rows; row++) {
        map<int, unsigned long long> new_timelines;

        for (const auto& [col, count] : timelines) {
            if (col >= 0 && col < cols) {
                char cell = lines[row][col];
                if (cell == '^') {
                    // Each timeline splits into 2 (left and right)
                    if (col - 1 >= 0) {
                        new_timelines[col - 1] += count;
                    }
                    if (col + 1 < cols) {
                        new_timelines[col + 1] += count;
                    }
                } else if (cell == '.') {
                    // Timelines continue straight down
                    new_timelines[col] += count;
                } else {
                    // Other characters - timelines continue
                    new_timelines[col] += count;
                }
            }
        }

        timelines = new_timelines;

        // If no more timelines, stop
        if (timelines.empty()) {
            break;
        }
    }

    // Total number of timelines
    unsigned long long total = 0;
    for (const auto& [col, count] : timelines) {
        total += count;
    }

    return total;
}

int main() {
    vector<string> lines = readInput();

    cout << "Part 1: " << part1(lines) << endl;
    cout << "Part 2: " << part2(lines) << endl;

    return 0;
}
