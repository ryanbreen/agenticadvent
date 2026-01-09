#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <tuple>
#include <cctype>
#include <algorithm>

using namespace std;

// Direction deltas: 0=right, 1=down, 2=left, 3=up
const int DR[] = {0, 1, 0, -1};
const int DC[] = {1, 0, -1, 0};

struct ParsedInput {
    vector<string> grid;
    vector<pair<bool, int>> instructions; // (is_turn, value) where value is steps or turn direction (0=L, 1=R)
};

ParsedInput parseInput(const string& text) {
    ParsedInput result;

    // Split on double newline
    size_t sep = text.find("\n\n");
    string gridPart = text.substr(0, sep);
    string pathPart = text.substr(sep + 2);

    // Parse grid
    istringstream gridStream(gridPart);
    string line;
    size_t maxWidth = 0;
    while (getline(gridStream, line)) {
        result.grid.push_back(line);
        maxWidth = max(maxWidth, line.size());
    }

    // Pad all lines to same width
    for (auto& row : result.grid) {
        while (row.size() < maxWidth) {
            row += ' ';
        }
    }

    // Parse path
    size_t i = 0;
    while (i < pathPart.size() && pathPart[i] != '\n') {
        if (isdigit(pathPart[i])) {
            size_t j = i;
            while (j < pathPart.size() && isdigit(pathPart[j])) {
                j++;
            }
            int steps = stoi(pathPart.substr(i, j - i));
            result.instructions.push_back({false, steps}); // false = move
            i = j;
        } else if (pathPart[i] == 'L' || pathPart[i] == 'R') {
            result.instructions.push_back({true, pathPart[i] == 'R' ? 1 : 0}); // true = turn
            i++;
        } else {
            i++;
        }
    }

    return result;
}

int part1(const vector<string>& grid, const vector<pair<bool, int>>& instructions) {
    int height = grid.size();
    int width = grid[0].size();

    // Find starting position (leftmost open tile on top row)
    int row = 0;
    int col = grid[0].find('.');
    int facing = 0; // Start facing right

    for (const auto& [isTurn, value] : instructions) {
        if (!isTurn) {
            // Move forward
            int steps = value;
            for (int s = 0; s < steps; s++) {
                int nr = row + DR[facing];
                int nc = col + DC[facing];

                // Wrap around if needed
                if (facing == 0) { // Right
                    if (nc >= width || grid[nr][nc] == ' ') {
                        nc = 0;
                        while (grid[nr][nc] == ' ') nc++;
                    }
                } else if (facing == 2) { // Left
                    if (nc < 0 || grid[nr][nc] == ' ') {
                        nc = width - 1;
                        while (grid[nr][nc] == ' ') nc--;
                    }
                } else if (facing == 1) { // Down
                    if (nr >= height || grid[nr][nc] == ' ') {
                        nr = 0;
                        while (grid[nr][nc] == ' ') nr++;
                    }
                } else if (facing == 3) { // Up
                    if (nr < 0 || grid[nr][nc] == ' ') {
                        nr = height - 1;
                        while (grid[nr][nc] == ' ') nr--;
                    }
                }

                // Check if we hit a wall
                if (grid[nr][nc] == '#') {
                    break;
                }

                row = nr;
                col = nc;
            }
        } else {
            // Turn
            if (value == 1) { // R
                facing = (facing + 1) % 4;
            } else { // L
                facing = (facing + 3) % 4;
            }
        }
    }

    return 1000 * (row + 1) + 4 * (col + 1) + facing;
}

tuple<int, int, int> getCubeFaceAndLocal(int row, int col, int faceSize) {
    // Cube layout:
    //   12
    //   3
    //  45
    //  6
    int faceRow = row / faceSize;
    int faceCol = col / faceSize;
    int localR = row % faceSize;
    int localC = col % faceSize;

    int face = -1;
    if (faceRow == 0 && faceCol == 1) face = 1;
    else if (faceRow == 0 && faceCol == 2) face = 2;
    else if (faceRow == 1 && faceCol == 1) face = 3;
    else if (faceRow == 2 && faceCol == 0) face = 4;
    else if (faceRow == 2 && faceCol == 1) face = 5;
    else if (faceRow == 3 && faceCol == 0) face = 6;

    return {face, localR, localC};
}

tuple<int, int, int> wrapCube(int row, int col, int facing, int faceSize) {
    int S = faceSize;
    auto [face, lr, lc] = getCubeFaceAndLocal(row, col, S);

    if (face == 1) {
        if (facing == 3) { // Up: goes to face 6, from left, facing right
            return {3*S + lc, 0, 0};
        } else if (facing == 2) { // Left: goes to face 4, from left, facing right (inverted)
            return {3*S - 1 - lr, 0, 0};
        }
    } else if (face == 2) {
        if (facing == 0) { // Right: goes to face 5, from right, facing left (inverted)
            return {3*S - 1 - lr, 2*S - 1, 2};
        } else if (facing == 1) { // Down: goes to face 3, from right, facing left
            return {S + lc, 2*S - 1, 2};
        } else if (facing == 3) { // Up: goes to face 6, from bottom, facing up
            return {4*S - 1, lc, 3};
        }
    } else if (face == 3) {
        if (facing == 0) { // Right: goes to face 2, from bottom, facing up
            return {S - 1, 2*S + lr, 3};
        } else if (facing == 2) { // Left: goes to face 4, from top, facing down
            return {2*S, lr, 1};
        }
    } else if (face == 4) {
        if (facing == 3) { // Up: goes to face 3, from left, facing right
            return {S + lc, S, 0};
        } else if (facing == 2) { // Left: goes to face 1, from left, facing right (inverted)
            return {S - 1 - lr, S, 0};
        }
    } else if (face == 5) {
        if (facing == 0) { // Right: goes to face 2, from right, facing left (inverted)
            return {S - 1 - lr, 3*S - 1, 2};
        } else if (facing == 1) { // Down: goes to face 6, from right, facing left
            return {3*S + lc, S - 1, 2};
        }
    } else if (face == 6) {
        if (facing == 0) { // Right: goes to face 5, from bottom, facing up
            return {3*S - 1, S + lr, 3};
        } else if (facing == 1) { // Down: goes to face 2, from top, facing down
            return {0, 2*S + lc, 1};
        } else if (facing == 2) { // Left: goes to face 1, from top, facing down
            return {0, S + lr, 1};
        }
    }

    return {row, col, facing};
}

int part2(const vector<string>& grid, const vector<pair<bool, int>>& instructions) {
    int height = grid.size();
    int width = grid[0].size();
    int faceSize = (height > 50) ? 50 : 4;

    // Find starting position
    int row = 0;
    int col = grid[0].find('.');
    int facing = 0;

    for (const auto& [isTurn, value] : instructions) {
        if (!isTurn) {
            int steps = value;
            for (int s = 0; s < steps; s++) {
                int nr = row + DR[facing];
                int nc = col + DC[facing];
                int nf = facing;

                // Check if we need to wrap
                bool needWrap = false;
                if (nr < 0 || nr >= height || nc < 0 || nc >= width) {
                    needWrap = true;
                } else if (grid[nr][nc] == ' ') {
                    needWrap = true;
                }

                if (needWrap) {
                    auto [wr, wc, wf] = wrapCube(row, col, facing, faceSize);
                    nr = wr;
                    nc = wc;
                    nf = wf;
                }

                // Check for wall
                if (grid[nr][nc] == '#') {
                    break;
                }

                row = nr;
                col = nc;
                facing = nf;
            }
        } else {
            if (value == 1) { // R
                facing = (facing + 1) % 4;
            } else { // L
                facing = (facing + 3) % 4;
            }
        }
    }

    return 1000 * (row + 1) + 4 * (col + 1) + facing;
}

int main() {
    // Read input
    ifstream file("../input.txt");
    if (!file) {
        cerr << "Error opening input file" << endl;
        return 1;
    }

    stringstream buffer;
    buffer << file.rdbuf();
    string text = buffer.str();

    ParsedInput input = parseInput(text);

    cout << "Part 1: " << part1(input.grid, input.instructions) << endl;
    cout << "Part 2: " << part2(input.grid, input.instructions) << endl;

    return 0;
}
