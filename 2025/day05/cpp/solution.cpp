#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

struct Range {
    long long start;
    long long end;
};

pair<vector<Range>, vector<long long>> parseInput(const string& filename) {
    ifstream file(filename);
    vector<Range> ranges;
    vector<long long> ingredientIds;
    string line;
    bool inSecondSection = false;

    while (getline(file, line)) {
        if (line.empty()) {
            inSecondSection = true;
            continue;
        }

        if (!inSecondSection) {
            // Parse range "start-end"
            size_t dashPos = line.find('-');
            long long start = stoll(line.substr(0, dashPos));
            long long end = stoll(line.substr(dashPos + 1));
            ranges.push_back({start, end});
        } else {
            // Parse ingredient ID
            ingredientIds.push_back(stoll(line));
        }
    }

    return {ranges, ingredientIds};
}

int part1(const vector<Range>& ranges, const vector<long long>& ingredientIds) {
    int freshCount = 0;

    for (long long id : ingredientIds) {
        bool found = false;
        for (const Range& range : ranges) {
            if (id >= range.start && id <= range.end) {
                freshCount++;
                found = true;
                break;  // Found a match, no need to check other ranges
            }
        }
    }

    return freshCount;
}

long long part2(vector<Range> ranges) {
    // Sort ranges by start position
    sort(ranges.begin(), ranges.end(), [](const Range& a, const Range& b) {
        return a.start < b.start;
    });

    // Merge overlapping ranges
    vector<Range> merged;
    for (const Range& range : ranges) {
        if (!merged.empty() && range.start <= merged.back().end + 1) {
            // Overlapping or adjacent - merge with the last range
            merged.back().end = max(merged.back().end, range.end);
        } else {
            // No overlap - add as new range
            merged.push_back(range);
        }
    }

    // Count total unique IDs covered by merged ranges
    long long totalCount = 0;
    for (const Range& range : merged) {
        totalCount += (range.end - range.start + 1);
    }

    return totalCount;
}

int main() {
    auto [ranges, ingredientIds] = parseInput("../input.txt");

    cout << "Part 1: " << part1(ranges, ingredientIds) << endl;
    cout << "Part 2: " << part2(ranges) << endl;

    return 0;
}
