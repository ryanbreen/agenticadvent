#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <tuple>

using namespace std;

// Rock shapes as list of (dx, dy) offsets from bottom-left
const vector<vector<pair<int, int>>> ROCKS = {
    {{0, 0}, {1, 0}, {2, 0}, {3, 0}},           // Horizontal line
    {{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}},   // Plus
    {{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}},   // L shape
    {{0, 0}, {0, 1}, {0, 2}, {0, 3}},           // Vertical line
    {{0, 0}, {1, 0}, {0, 1}, {1, 1}}            // Square
};

const int WIDTH = 7;

struct PairHash {
    size_t operator()(const pair<int, long long>& p) const {
        return hash<long long>()(p.first) ^ (hash<long long>()(p.second) << 32);
    }
};

// Custom hash for state tuple
struct StateHash {
    size_t operator()(const tuple<int, int, vector<int>>& state) const {
        size_t h = hash<int>()(get<0>(state));
        h ^= hash<int>()(get<1>(state)) << 1;
        for (int v : get<2>(state)) {
            h ^= hash<int>()(v) + 0x9e3779b9 + (h << 6) + (h >> 2);
        }
        return h;
    }
};

long long simulate(const string& jets, long long num_rocks) {
    unordered_set<long long> occupied;  // Encode (x, y) as y * WIDTH + x
    long long height = 0;
    int jet_idx = 0;

    // For cycle detection
    unordered_map<tuple<int, int, vector<int>>, long long, StateHash> states;
    vector<long long> heights;

    auto encode = [](int x, long long y) -> long long {
        return y * WIDTH + x;
    };

    for (long long rock_num = 0; rock_num < num_rocks; rock_num++) {
        int rock_type = rock_num % 5;
        const auto& rock = ROCKS[rock_type];

        // Starting position: left edge at x=2, bottom at y=height+3
        int x = 2;
        long long y = height + 3;

        while (true) {
            // Jet push
            char jet = jets[jet_idx];
            jet_idx = (jet_idx + 1) % jets.size();

            int dx = (jet == '>') ? 1 : -1;

            // Check if can move horizontally
            bool can_move = true;
            for (const auto& [rx, ry] : rock) {
                int nx = x + rx + dx;
                long long ny = y + ry;
                if (nx < 0 || nx >= WIDTH || occupied.count(encode(nx, ny))) {
                    can_move = false;
                    break;
                }
            }

            if (can_move) {
                x += dx;
            }

            // Fall down
            bool can_fall = true;
            for (const auto& [rx, ry] : rock) {
                int nx = x + rx;
                long long ny = y + ry - 1;
                if (ny < 0 || occupied.count(encode(nx, ny))) {
                    can_fall = false;
                    break;
                }
            }

            if (can_fall) {
                y -= 1;
            } else {
                // Rock stops
                for (const auto& [rx, ry] : rock) {
                    occupied.insert(encode(x + rx, y + ry));
                    height = max(height, y + ry + 1);
                }
                break;
            }
        }

        heights.push_back(height);

        // Cycle detection for Part 2
        if (num_rocks > 10000) {
            // Create state key from surface profile
            // Use top 30 rows relative positions
            const int profile_depth = 30;
            vector<int> profile(WIDTH);

            for (int col = 0; col < WIDTH; col++) {
                for (int row = 0; row < profile_depth; row++) {
                    if (occupied.count(encode(col, height - 1 - row))) {
                        profile[col] = row;
                        break;
                    } else if (row == profile_depth - 1) {
                        profile[col] = profile_depth;
                    }
                }
            }

            auto state = make_tuple(rock_type, jet_idx, profile);

            auto it = states.find(state);
            if (it != states.end()) {
                // Found cycle
                long long cycle_start = it->second;
                long long cycle_len = rock_num - cycle_start;
                long long cycle_height = height - heights[cycle_start];

                // Calculate final height
                long long remaining = num_rocks - rock_num - 1;
                long long full_cycles = remaining / cycle_len;
                long long leftover = remaining % cycle_len;

                long long final_height = height + full_cycles * cycle_height;
                if (leftover > 0) {
                    final_height += heights[cycle_start + leftover] - heights[cycle_start];
                }

                return final_height;
            }

            states[state] = rock_num;
        }
    }

    return height;
}

int main() {
    ifstream file("../input.txt");
    string jets;
    getline(file, jets);

    // Trim whitespace
    while (!jets.empty() && (jets.back() == '\n' || jets.back() == '\r' || jets.back() == ' ')) {
        jets.pop_back();
    }

    cout << "Part 1: " << simulate(jets, 2022) << endl;
    cout << "Part 2: " << simulate(jets, 1000000000000LL) << endl;

    return 0;
}
