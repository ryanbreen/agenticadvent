#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <array>

using namespace std;

// Parse input into locks and keys
void parse_input(const string& text, vector<array<int, 5>>& locks, vector<array<int, 5>>& keys) {
    istringstream stream(text);
    string line;
    vector<string> current_schematic;

    while (getline(stream, line)) {
        if (line.empty()) {
            if (!current_schematic.empty()) {
                // Process the schematic
                array<int, 5> heights = {0, 0, 0, 0, 0};

                // Check if it's a lock (top row all #) or key (top row all .)
                if (current_schematic[0] == "#####") {
                    // It's a lock - count # from top (excluding top row)
                    for (int col = 0; col < 5; col++) {
                        int height = 0;
                        for (int row = 1; row < 7; row++) {
                            if (current_schematic[row][col] == '#') {
                                height++;
                            } else {
                                break;
                            }
                        }
                        heights[col] = height;
                    }
                    locks.push_back(heights);
                } else {
                    // It's a key - count # from bottom (excluding bottom row)
                    for (int col = 0; col < 5; col++) {
                        int height = 0;
                        for (int row = 5; row >= 0; row--) {
                            if (current_schematic[row][col] == '#') {
                                height++;
                            } else {
                                break;
                            }
                        }
                        heights[col] = height;
                    }
                    keys.push_back(heights);
                }

                current_schematic.clear();
            }
        } else {
            current_schematic.push_back(line);
        }
    }

    // Process last schematic if file doesn't end with blank line
    if (!current_schematic.empty()) {
        array<int, 5> heights = {0, 0, 0, 0, 0};

        if (current_schematic[0] == "#####") {
            // It's a lock
            for (int col = 0; col < 5; col++) {
                int height = 0;
                for (int row = 1; row < 7; row++) {
                    if (current_schematic[row][col] == '#') {
                        height++;
                    } else {
                        break;
                    }
                }
                heights[col] = height;
            }
            locks.push_back(heights);
        } else {
            // It's a key
            for (int col = 0; col < 5; col++) {
                int height = 0;
                for (int row = 5; row >= 0; row--) {
                    if (current_schematic[row][col] == '#') {
                        height++;
                    } else {
                        break;
                    }
                }
                heights[col] = height;
            }
            keys.push_back(heights);
        }
    }
}

// Check if a key fits a lock (no column exceeds 5)
bool fits(const array<int, 5>& lock, const array<int, 5>& key) {
    for (int i = 0; i < 5; i++) {
        if (lock[i] + key[i] > 5) {
            return false;
        }
    }
    return true;
}

// Count unique lock/key pairs that fit together
int part1(const vector<array<int, 5>>& locks, const vector<array<int, 5>>& keys) {
    int count = 0;
    for (const auto& lock : locks) {
        for (const auto& key : keys) {
            if (fits(lock, key)) {
                count++;
            }
        }
    }
    return count;
}

int main() {
    ifstream file("../input.txt");
    if (!file.is_open()) {
        cerr << "Error: Could not open input.txt" << endl;
        return 1;
    }

    stringstream buffer;
    buffer << file.rdbuf();
    string text = buffer.str();
    file.close();

    vector<array<int, 5>> locks;
    vector<array<int, 5>> keys;

    parse_input(text, locks, keys);

    int answer1 = part1(locks, keys);
    cout << "Part 1: " << answer1 << endl;

    // Day 25 typically only has Part 1
    cout << "Part 2: Merry Christmas!" << endl;

    return 0;
}
