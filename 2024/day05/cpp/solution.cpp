#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>

using namespace std;

// Parse input file and extract rules and updates
void parseInput(const string& filename,
                unordered_map<int, unordered_set<int>>& rules,
                vector<vector<int>>& updates) {
    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "Error: Cannot open file " << filename << endl;
        exit(1);
    }

    string line;
    bool parsingRules = true;

    while (getline(file, line)) {
        if (line.empty()) {
            parsingRules = false;
            continue;
        }

        if (parsingRules) {
            // Parse rule: X|Y
            size_t pos = line.find('|');
            int before = stoi(line.substr(0, pos));
            int after = stoi(line.substr(pos + 1));
            rules[before].insert(after);
        } else {
            // Parse update: comma-separated numbers
            vector<int> update;
            stringstream ss(line);
            string num;
            while (getline(ss, num, ',')) {
                update.push_back(stoi(num));
            }
            updates.push_back(update);
        }
    }

    file.close();
}

// Check if an update is in valid order according to rules
bool isValidOrder(const vector<int>& update,
                  const unordered_map<int, unordered_set<int>>& rules) {
    // Create a map of page positions
    unordered_map<int, int> pagePositions;
    for (int i = 0; i < update.size(); i++) {
        pagePositions[update[i]] = i;
    }

    // Check each page and its constraints
    for (int i = 0; i < update.size(); i++) {
        int page = update[i];

        // Check if this page has any ordering rules
        auto it = rules.find(page);
        if (it != rules.end()) {
            // Check all pages that must come after this page
            for (int mustBeAfter : it->second) {
                // If mustBeAfter is in this update
                auto posIt = pagePositions.find(mustBeAfter);
                if (posIt != pagePositions.end()) {
                    // It must come after current page
                    if (posIt->second < i) {
                        return false;
                    }
                }
            }
        }
    }

    return true;
}

// Fix order of an update using custom comparator
vector<int> fixOrder(const vector<int>& update,
                     const unordered_map<int, unordered_set<int>>& rules) {
    vector<int> result = update;

    // Sort using custom comparator
    sort(result.begin(), result.end(), [&rules](int a, int b) {
        // If a must come before b, return true
        auto it = rules.find(a);
        if (it != rules.end() && it->second.count(b) > 0) {
            return true;
        }

        // If b must come before a, return false
        auto it2 = rules.find(b);
        if (it2 != rules.end() && it2->second.count(a) > 0) {
            return false;
        }

        return false;
    });

    return result;
}

// Part 1: Sum middle page numbers of correctly-ordered updates
int part1(const vector<vector<int>>& updates,
          const unordered_map<int, unordered_set<int>>& rules) {
    int total = 0;

    for (const auto& update : updates) {
        if (isValidOrder(update, rules)) {
            int middleIdx = update.size() / 2;
            total += update[middleIdx];
        }
    }

    return total;
}

// Part 2: Sum middle page numbers of fixed incorrectly-ordered updates
int part2(const vector<vector<int>>& updates,
          const unordered_map<int, unordered_set<int>>& rules) {
    int total = 0;

    for (const auto& update : updates) {
        if (!isValidOrder(update, rules)) {
            vector<int> fixed = fixOrder(update, rules);
            int middleIdx = fixed.size() / 2;
            total += fixed[middleIdx];
        }
    }

    return total;
}

int main() {
    unordered_map<int, unordered_set<int>> rules;
    vector<vector<int>> updates;

    parseInput("../input.txt", rules, updates);

    cout << "Part 1: " << part1(updates, rules) << endl;
    cout << "Part 2: " << part2(updates, rules) << endl;

    return 0;
}
