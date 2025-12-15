#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <set>
#include <algorithm>

using namespace std;

struct Position {
    int r, c;
    bool operator<(const Position& other) const {
        if (r != other.r) return r < other.r;
        return c < other.c;
    }
};

pair<vector<vector<char>>, string> parse_input(const string& filename) {
    ifstream file(filename);
    string line;
    vector<vector<char>> grid;
    string moves;
    bool reading_moves = false;

    while (getline(file, line)) {
        if (line.empty()) {
            reading_moves = true;
            continue;
        }
        if (!reading_moves) {
            grid.push_back(vector<char>(line.begin(), line.end()));
        } else {
            moves += line;
        }
    }

    return {grid, moves};
}

Position find_robot(const vector<vector<char>>& grid) {
    for (int r = 0; r < grid.size(); r++) {
        for (int c = 0; c < grid[r].size(); c++) {
            if (grid[r][c] == '@') {
                return {r, c};
            }
        }
    }
    return {-1, -1};
}

Position move_robot(vector<vector<char>>& grid, Position robot_pos, char direction) {
    int dr = 0, dc = 0;

    if (direction == '<') { dr = 0; dc = -1; }
    else if (direction == '>') { dr = 0; dc = 1; }
    else if (direction == '^') { dr = -1; dc = 0; }
    else if (direction == 'v') { dr = 1; dc = 0; }

    int r = robot_pos.r, c = robot_pos.c;
    int nr = r + dr, nc = c + dc;

    if (grid[nr][nc] == '#') {
        return robot_pos;
    }

    if (grid[nr][nc] == '.') {
        grid[r][c] = '.';
        grid[nr][nc] = '@';
        return {nr, nc};
    }

    if (grid[nr][nc] == 'O') {
        int check_r = nr, check_c = nc;
        while (grid[check_r][check_c] == 'O') {
            check_r += dr;
            check_c += dc;
        }

        if (grid[check_r][check_c] == '#') {
            return robot_pos;
        }

        grid[check_r][check_c] = 'O';
        grid[r][c] = '.';
        grid[nr][nc] = '@';
        return {nr, nc};
    }

    return robot_pos;
}

long long calculate_gps(const vector<vector<char>>& grid, char box_char = 'O') {
    long long total = 0;
    for (int r = 0; r < grid.size(); r++) {
        for (int c = 0; c < grid[r].size(); c++) {
            if (grid[r][c] == box_char) {
                total += 100 * r + c;
            }
        }
    }
    return total;
}

long long part1(const string& filename) {
    auto [grid, moves] = parse_input(filename);
    Position robot_pos = find_robot(grid);

    for (char move : moves) {
        robot_pos = move_robot(grid, robot_pos, move);
    }

    return calculate_gps(grid);
}

vector<vector<char>> scale_grid(const vector<vector<char>>& grid) {
    vector<vector<char>> new_grid;
    for (const auto& row : grid) {
        vector<char> new_row;
        for (char cell : row) {
            if (cell == '#') {
                new_row.push_back('#');
                new_row.push_back('#');
            } else if (cell == 'O') {
                new_row.push_back('[');
                new_row.push_back(']');
            } else if (cell == '.') {
                new_row.push_back('.');
                new_row.push_back('.');
            } else if (cell == '@') {
                new_row.push_back('@');
                new_row.push_back('.');
            }
        }
        new_grid.push_back(new_row);
    }
    return new_grid;
}

bool can_move_box_vertical(const vector<vector<char>>& grid, int box_left_c, int r, int dr) {
    int nr = r + dr;
    int left_c = box_left_c;
    int right_c = box_left_c + 1;

    char left_target = grid[nr][left_c];
    char right_target = grid[nr][right_c];

    if (left_target == '#' || right_target == '#') {
        return false;
    }

    set<Position> boxes_to_check;

    if (left_target == '[') {
        boxes_to_check.insert({nr, left_c});
    } else if (left_target == ']') {
        boxes_to_check.insert({nr, left_c - 1});
    }

    if (right_target == '[') {
        boxes_to_check.insert({nr, right_c});
    } else if (right_target == ']') {
        boxes_to_check.insert({nr, right_c - 1});
    }

    for (const auto& box : boxes_to_check) {
        if (!can_move_box_vertical(grid, box.c, box.r, dr)) {
            return false;
        }
    }

    return true;
}

void collect_boxes_vertical(const vector<vector<char>>& grid, int box_left_c, int r, int dr, set<Position>& collected) {
    collected.insert({r, box_left_c});
    int nr = r + dr;
    int left_c = box_left_c;
    int right_c = box_left_c + 1;

    char left_target = grid[nr][left_c];
    char right_target = grid[nr][right_c];

    set<Position> boxes_to_check;

    if (left_target == '[') {
        boxes_to_check.insert({nr, left_c});
    } else if (left_target == ']') {
        boxes_to_check.insert({nr, left_c - 1});
    }

    if (right_target == '[') {
        boxes_to_check.insert({nr, right_c});
    } else if (right_target == ']') {
        boxes_to_check.insert({nr, right_c - 1});
    }

    for (const auto& box : boxes_to_check) {
        if (collected.find(box) == collected.end()) {
            collect_boxes_vertical(grid, box.c, box.r, dr, collected);
        }
    }
}

Position move_robot_wide(vector<vector<char>>& grid, Position robot_pos, char direction) {
    int dr = 0, dc = 0;

    if (direction == '<') { dr = 0; dc = -1; }
    else if (direction == '>') { dr = 0; dc = 1; }
    else if (direction == '^') { dr = -1; dc = 0; }
    else if (direction == 'v') { dr = 1; dc = 0; }

    int r = robot_pos.r, c = robot_pos.c;
    int nr = r + dr, nc = c + dc;

    char target = grid[nr][nc];

    if (target == '#') {
        return robot_pos;
    }

    if (target == '.') {
        grid[r][c] = '.';
        grid[nr][nc] = '@';
        return {nr, nc};
    }

    if (target == '[' || target == ']') {
        if (dc != 0) {  // Horizontal movement
            int check_c = nc;
            while (grid[r][check_c] == '[' || grid[r][check_c] == ']') {
                check_c += dc;
            }

            if (grid[r][check_c] == '#') {
                return robot_pos;
            }

            // Shift all boxes
            if (dc > 0) {  // Moving right
                for (int col = check_c; col > nc; col--) {
                    grid[r][col] = grid[r][col - 1];
                }
            } else {  // Moving left
                for (int col = check_c; col < nc; col++) {
                    grid[r][col] = grid[r][col + 1];
                }
            }

            grid[r][c] = '.';
            grid[nr][nc] = '@';
            return {nr, nc};
        } else {  // Vertical movement
            int box_left_c = (target == '[') ? nc : nc - 1;

            if (!can_move_box_vertical(grid, box_left_c, nr, dr)) {
                return robot_pos;
            }

            set<Position> boxes_to_move;
            collect_boxes_vertical(grid, box_left_c, nr, dr, boxes_to_move);

            // Convert set to vector and sort
            vector<Position> sorted_boxes(boxes_to_move.begin(), boxes_to_move.end());
            if (dr > 0) {
                sort(sorted_boxes.begin(), sorted_boxes.end(), [](const Position& a, const Position& b) {
                    return a.r > b.r;
                });
            } else {
                sort(sorted_boxes.begin(), sorted_boxes.end(), [](const Position& a, const Position& b) {
                    return a.r < b.r;
                });
            }

            // Move all boxes
            for (const auto& box : sorted_boxes) {
                grid[box.r][box.c] = '.';
                grid[box.r][box.c + 1] = '.';
                grid[box.r + dr][box.c] = '[';
                grid[box.r + dr][box.c + 1] = ']';
            }

            // Move robot
            grid[r][c] = '.';
            grid[nr][nc] = '@';
            return {nr, nc};
        }
    }

    return robot_pos;
}

long long part2(const string& filename) {
    auto [grid, moves] = parse_input(filename);
    grid = scale_grid(grid);
    Position robot_pos = find_robot(grid);

    for (char move : moves) {
        robot_pos = move_robot_wide(grid, robot_pos, move);
    }

    return calculate_gps(grid, '[');
}

int main() {
    string filename = "../input.txt";

    cout << "Part 1: " << part1(filename) << endl;
    cout << "Part 2: " << part2(filename) << endl;

    return 0;
}
