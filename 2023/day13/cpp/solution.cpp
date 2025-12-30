#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

// Count character differences between two string views
int count_differences(std::string_view s1, std::string_view s2) {
    auto len = std::min(s1.length(), s2.length());
    return std::transform_reduce(
        s1.begin(), s1.begin() + len,
        s2.begin(),
        0,
        std::plus<>{},
        [](char a, char b) { return a != b ? 1 : 0; }
    );
}

// Find vertical line of reflection with exactly target_diff differences
// target_diff=0 for Part 1 (perfect reflection), target_diff=1 for Part 2 (one smudge)
int find_vertical_reflection(const std::vector<std::string>& pattern, int target_diff) {
    if (pattern.empty()) return 0;
    auto width = static_cast<int>(pattern[0].length());

    for (int col = 1; col < width; ++col) {
        int total_diff = 0;

        for (const auto& row : pattern) {
            int left = col - 1;
            int right = col;

            while (left >= 0 && right < width && total_diff <= target_diff) {
                if (row[left] != row[right]) {
                    ++total_diff;
                }
                --left;
                ++right;
            }

            if (total_diff > target_diff) break;
        }

        if (total_diff == target_diff) return col;
    }

    return 0;
}

// Find horizontal line of reflection with exactly target_diff differences
int find_horizontal_reflection(const std::vector<std::string>& pattern, int target_diff) {
    if (pattern.empty()) return 0;
    auto height = static_cast<int>(pattern.size());

    for (int row = 1; row < height; ++row) {
        int total_diff = 0;
        int top = row - 1;
        int bottom = row;

        while (top >= 0 && bottom < height && total_diff <= target_diff) {
            total_diff += count_differences(pattern[top], pattern[bottom]);
            --top;
            ++bottom;
        }

        if (total_diff == target_diff) return row;
    }

    return 0;
}

// Summarize a pattern: vertical reflection returns col, horizontal returns row * 100
int summarize_pattern(const std::vector<std::string>& pattern, int target_diff) {
    int v = find_vertical_reflection(pattern, target_diff);
    if (v > 0) return v;
    return find_horizontal_reflection(pattern, target_diff) * 100;
}

// Parse input text into vector of patterns
std::vector<std::vector<std::string>> parse_input(const std::string& text) {
    std::vector<std::vector<std::string>> patterns;
    std::vector<std::string> current_pattern;
    std::istringstream stream(text);
    std::string line;

    while (std::getline(stream, line)) {
        if (line.empty()) {
            if (!current_pattern.empty()) {
                patterns.push_back(std::move(current_pattern));
                current_pattern.clear();
            }
        } else {
            current_pattern.push_back(std::move(line));
        }
    }

    if (!current_pattern.empty()) {
        patterns.push_back(std::move(current_pattern));
    }

    return patterns;
}

// Solve for given target_diff across all patterns
int solve(const std::vector<std::vector<std::string>>& patterns, int target_diff) {
    return std::transform_reduce(
        patterns.begin(), patterns.end(),
        0,
        std::plus<>{},
        [target_diff](const auto& pattern) {
            return summarize_pattern(pattern, target_diff);
        }
    );
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Could not open input.txt\n";
        return 1;
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();

    auto patterns = parse_input(text);

    std::cout << "Part 1: " << solve(patterns, 0) << '\n';
    std::cout << "Part 2: " << solve(patterns, 1) << '\n';

    return 0;
}
