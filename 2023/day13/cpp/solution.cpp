#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

// Parse input into patterns
vector<vector<string>> parse_input(const string& text) {
    vector<vector<string>> patterns;
    vector<string> current_pattern;

    size_t start = 0;
    size_t end = text.find('\n');

    while (end != string::npos || start < text.length()) {
        string line = (end == string::npos) ? text.substr(start) : text.substr(start, end - start);

        if (line.empty()) {
            if (!current_pattern.empty()) {
                patterns.push_back(current_pattern);
                current_pattern.clear();
            }
        } else {
            current_pattern.push_back(line);
        }

        if (end == string::npos) break;
        start = end + 1;
        end = text.find('\n', start);
    }

    if (!current_pattern.empty()) {
        patterns.push_back(current_pattern);
    }

    return patterns;
}

// Find vertical line of reflection
int find_vertical_reflection(const vector<string>& pattern) {
    if (pattern.empty()) return 0;
    int width = pattern[0].length();

    for (int col = 1; col < width; col++) {
        bool is_reflection = true;

        for (const string& row : pattern) {
            // Compare left and right sides
            int left_idx = col - 1;
            int right_idx = col;

            while (left_idx >= 0 && right_idx < width) {
                if (row[left_idx] != row[right_idx]) {
                    is_reflection = false;
                    break;
                }
                left_idx--;
                right_idx++;
            }

            if (!is_reflection) break;
        }

        if (is_reflection) return col;
    }

    return 0;
}

// Find horizontal line of reflection
int find_horizontal_reflection(const vector<string>& pattern) {
    if (pattern.empty()) return 0;
    int height = pattern.size();

    for (int row = 1; row < height; row++) {
        bool is_reflection = true;

        int top_idx = row - 1;
        int bottom_idx = row;

        while (top_idx >= 0 && bottom_idx < height) {
            if (pattern[top_idx] != pattern[bottom_idx]) {
                is_reflection = false;
                break;
            }
            top_idx--;
            bottom_idx++;
        }

        if (is_reflection) return row;
    }

    return 0;
}

// Summarize pattern
int summarize_pattern(const vector<string>& pattern) {
    int v = find_vertical_reflection(pattern);
    if (v > 0) return v;
    int h = find_horizontal_reflection(pattern);
    return h * 100;
}

// Count differences between two strings
int count_differences(const string& s1, const string& s2) {
    int count = 0;
    int min_len = min(s1.length(), s2.length());
    for (int i = 0; i < min_len; i++) {
        if (s1[i] != s2[i]) count++;
    }
    return count;
}

// Find vertical line with exactly one smudge
int find_vertical_reflection_with_smudge(const vector<string>& pattern) {
    if (pattern.empty()) return 0;
    int width = pattern[0].length();

    for (int col = 1; col < width; col++) {
        int total_diff = 0;

        for (const string& row : pattern) {
            int left_idx = col - 1;
            int right_idx = col;

            while (left_idx >= 0 && right_idx < width) {
                if (row[left_idx] != row[right_idx]) {
                    total_diff++;
                }
                left_idx--;
                right_idx++;
            }

            if (total_diff > 1) break;
        }

        if (total_diff == 1) return col;
    }

    return 0;
}

// Find horizontal line with exactly one smudge
int find_horizontal_reflection_with_smudge(const vector<string>& pattern) {
    if (pattern.empty()) return 0;
    int height = pattern.size();

    for (int row = 1; row < height; row++) {
        int total_diff = 0;

        int top_idx = row - 1;
        int bottom_idx = row;

        while (top_idx >= 0 && bottom_idx < height) {
            total_diff += count_differences(pattern[top_idx], pattern[bottom_idx]);
            if (total_diff > 1) break;
            top_idx--;
            bottom_idx++;
        }

        if (total_diff == 1) return row;
    }

    return 0;
}

// Summarize pattern with smudge
int summarize_pattern_with_smudge(const vector<string>& pattern) {
    int v = find_vertical_reflection_with_smudge(pattern);
    if (v > 0) return v;
    int h = find_horizontal_reflection_with_smudge(pattern);
    return h * 100;
}

// Part 1
int part1(const vector<vector<string>>& patterns) {
    int sum = 0;
    for (const auto& pattern : patterns) {
        sum += summarize_pattern(pattern);
    }
    return sum;
}

// Part 2
int part2(const vector<vector<string>>& patterns) {
    int sum = 0;
    for (const auto& pattern : patterns) {
        sum += summarize_pattern_with_smudge(pattern);
    }
    return sum;
}

int main() {
    // Read input file
    ifstream file("../input.txt");
    if (!file.is_open()) {
        cerr << "Error: Could not open input.txt" << endl;
        return 1;
    }

    string text;
    string line;
    while (getline(file, line)) {
        text += line + "\n";
    }
    file.close();

    // Parse patterns
    vector<vector<string>> patterns = parse_input(text);

    // Solve both parts
    cout << "Part 1: " << part1(patterns) << endl;
    cout << "Part 2: " << part2(patterns) << endl;

    return 0;
}
