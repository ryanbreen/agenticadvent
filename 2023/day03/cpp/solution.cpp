#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <cctype>

using namespace std;

// Check if a character is a symbol (not digit, not period)
bool isSymbol(char c) {
    return c != '.' && !isdigit(c);
}

// Get all 8 adjacent positions
vector<pair<int, int>> getAdjacentPositions(int row, int col) {
    return {
        {row-1, col-1}, {row-1, col}, {row-1, col+1},
        {row, col-1},                 {row, col+1},
        {row+1, col-1}, {row+1, col}, {row+1, col+1}
    };
}

int part1(const vector<string>& grid) {
    int sum = 0;
    int rows = grid.size();

    for (int r = 0; r < rows; r++) {
        int cols = grid[r].size();

        for (int c = 0; c < cols; c++) {
            if (isdigit(grid[r][c])) {
                // Found start of a number
                int numStart = c;
                int number = 0;

                // Parse the full number
                while (c < cols && isdigit(grid[r][c])) {
                    number = number * 10 + (grid[r][c] - '0');
                    c++;
                }
                int numEnd = c - 1;

                // Check if any position in this number is adjacent to a symbol
                bool isPartNumber = false;
                for (int col = numStart; col <= numEnd && !isPartNumber; col++) {
                    auto adjacentPositions = getAdjacentPositions(r, col);
                    for (auto [ar, ac] : adjacentPositions) {
                        if (ar >= 0 && ar < rows && ac >= 0 && ac < (int)grid[ar].size()) {
                            if (isSymbol(grid[ar][ac])) {
                                isPartNumber = true;
                                break;
                            }
                        }
                    }
                }

                if (isPartNumber) {
                    sum += number;
                }

                // Move back one position since the outer loop will increment
                c--;
            }
        }
    }

    return sum;
}

int part2(const vector<string>& grid) {
    int rows = grid.size();

    // Map from gear position (row,col) to the set of adjacent numbers
    // Store each number with a unique identifier (row, start_col, value)
    map<pair<int,int>, vector<pair<int,int>>> gearToNumbers;

    for (int r = 0; r < rows; r++) {
        int cols = grid[r].size();

        for (int c = 0; c < cols; c++) {
            if (isdigit(grid[r][c])) {
                // Found start of a number
                int numStart = c;
                int number = 0;

                // Parse the full number
                while (c < cols && isdigit(grid[r][c])) {
                    number = number * 10 + (grid[r][c] - '0');
                    c++;
                }
                int numEnd = c - 1;

                // Find all adjacent '*' symbols for this number
                set<pair<int,int>> adjacentGears;
                for (int col = numStart; col <= numEnd; col++) {
                    auto adjacentPositions = getAdjacentPositions(r, col);
                    for (auto [ar, ac] : adjacentPositions) {
                        if (ar >= 0 && ar < rows && ac >= 0 && ac < (int)grid[ar].size()) {
                            if (grid[ar][ac] == '*') {
                                adjacentGears.insert({ar, ac});
                            }
                        }
                    }
                }

                // Add this number to all adjacent gears
                for (auto gear : adjacentGears) {
                    gearToNumbers[gear].push_back({r, number});
                }

                c--;
            }
        }
    }

    // Calculate gear ratios
    int sum = 0;
    for (const auto& [gear, numbers] : gearToNumbers) {
        if (numbers.size() == 2) {
            sum += numbers[0].second * numbers[1].second;
        }
    }

    return sum;
}

int main() {
    // Read input file
    ifstream inputFile("../input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input file" << endl;
        return 1;
    }

    vector<string> grid;
    string line;
    while (getline(inputFile, line)) {
        grid.push_back(line);
    }
    inputFile.close();

    cout << "Part 1: " << part1(grid) << endl;
    cout << "Part 2: " << part2(grid) << endl;

    return 0;
}
