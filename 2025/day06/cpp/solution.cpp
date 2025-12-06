#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <sstream>

using namespace std;

struct Problem {
    vector<long long> numbers;
    char op;
};

// Read input file into lines
vector<string> readInput(const string& filename) {
    ifstream file(filename);
    vector<string> lines;
    string line;

    while (getline(file, line)) {
        lines.push_back(line);
    }

    return lines;
}

// Parse problems for Part 1
vector<Problem> parseProblems(const vector<string>& lines) {
    if (lines.empty()) {
        return {};
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    int opRowIdx = lines.size() - 1;
    while (opRowIdx >= 0) {
        string trimmed = lines[opRowIdx];
        // Remove leading/trailing whitespace
        trimmed.erase(0, trimmed.find_first_not_of(" \t\n\r"));
        trimmed.erase(trimmed.find_last_not_of(" \t\n\r") + 1);

        if (trimmed.empty()) {
            opRowIdx--;
            continue;
        }

        bool allOps = true;
        for (char c : lines[opRowIdx]) {
            if (c != '+' && c != '*' && c != ' ') {
                allOps = false;
                break;
            }
        }

        if (allOps) {
            break;
        }
        opRowIdx--;
    }

    if (opRowIdx < 0) {
        return {};
    }

    string opRow = lines[opRowIdx];
    vector<string> numberRows(lines.begin(), lines.begin() + opRowIdx);

    // Find max width
    size_t maxWidth = 0;
    for (const auto& line : lines) {
        maxWidth = max(maxWidth, line.length());
    }

    // Pad all rows to the same width
    vector<string> paddedNumberRows;
    for (const auto& row : numberRows) {
        string padded = row;
        while (padded.length() < maxWidth) {
            padded += ' ';
        }
        paddedNumberRows.push_back(padded);
    }

    string paddedOpRow = opRow;
    while (paddedOpRow.length() < maxWidth) {
        paddedOpRow += ' ';
    }

    // Find problem boundaries
    vector<Problem> problems;
    size_t col = 0;

    while (col < maxWidth) {
        // Skip separator columns (all spaces)
        while (col < maxWidth) {
            bool allSpaces = true;
            for (const auto& row : paddedNumberRows) {
                if (row[col] != ' ') {
                    allSpaces = false;
                    break;
                }
            }
            if (!allSpaces || paddedOpRow[col] != ' ') {
                break;
            }
            col++;
        }

        if (col >= maxWidth) {
            break;
        }

        // Find the end of this problem
        size_t startCol = col;
        while (col < maxWidth) {
            // Check if this is a separator column
            bool isSeparator = true;
            for (const auto& row : paddedNumberRows) {
                if (row[col] != ' ') {
                    isSeparator = false;
                    break;
                }
            }
            if (isSeparator && paddedOpRow[col] == ' ') {
                break;
            }
            col++;
        }

        size_t endCol = col;

        // Extract numbers and operator for this problem
        vector<long long> numbers;
        for (const auto& row : paddedNumberRows) {
            string numStr = row.substr(startCol, endCol - startCol);
            // Trim whitespace
            numStr.erase(0, numStr.find_first_not_of(" \t"));
            numStr.erase(numStr.find_last_not_of(" \t") + 1);

            if (!numStr.empty()) {
                numbers.push_back(stoll(numStr));
            }
        }

        string opStr = paddedOpRow.substr(startCol, endCol - startCol);
        opStr.erase(0, opStr.find_first_not_of(" \t"));
        opStr.erase(opStr.find_last_not_of(" \t") + 1);

        if (!opStr.empty() && !numbers.empty()) {
            Problem p;
            p.numbers = numbers;
            p.op = opStr[0];
            problems.push_back(p);
        }
    }

    return problems;
}

// Parse problems for Part 2
vector<Problem> parseProblemsPart2(const vector<string>& lines) {
    if (lines.empty()) {
        return {};
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    int opRowIdx = lines.size() - 1;
    while (opRowIdx >= 0) {
        string trimmed = lines[opRowIdx];
        // Remove leading/trailing whitespace
        trimmed.erase(0, trimmed.find_first_not_of(" \t\n\r"));
        trimmed.erase(trimmed.find_last_not_of(" \t\n\r") + 1);

        if (trimmed.empty()) {
            opRowIdx--;
            continue;
        }

        bool allOps = true;
        for (char c : lines[opRowIdx]) {
            if (c != '+' && c != '*' && c != ' ') {
                allOps = false;
                break;
            }
        }

        if (allOps) {
            break;
        }
        opRowIdx--;
    }

    if (opRowIdx < 0) {
        return {};
    }

    string opRow = lines[opRowIdx];
    vector<string> numberRows(lines.begin(), lines.begin() + opRowIdx);

    // Find max width
    size_t maxWidth = 0;
    for (const auto& line : lines) {
        maxWidth = max(maxWidth, line.length());
    }

    // Pad all rows to the same width
    vector<string> paddedNumberRows;
    for (const auto& row : numberRows) {
        string padded = row;
        while (padded.length() < maxWidth) {
            padded += ' ';
        }
        paddedNumberRows.push_back(padded);
    }

    string paddedOpRow = opRow;
    while (paddedOpRow.length() < maxWidth) {
        paddedOpRow += ' ';
    }

    // Find problem boundaries
    vector<Problem> problems;
    size_t col = 0;

    while (col < maxWidth) {
        // Skip separator columns (all spaces)
        while (col < maxWidth) {
            bool allSpaces = true;
            for (const auto& row : paddedNumberRows) {
                if (row[col] != ' ') {
                    allSpaces = false;
                    break;
                }
            }
            if (!allSpaces || paddedOpRow[col] != ' ') {
                break;
            }
            col++;
        }

        if (col >= maxWidth) {
            break;
        }

        // Find the end of this problem
        size_t startCol = col;
        while (col < maxWidth) {
            // Check if this is a separator column
            bool isSeparator = true;
            for (const auto& row : paddedNumberRows) {
                if (row[col] != ' ') {
                    isSeparator = false;
                    break;
                }
            }
            if (isSeparator && paddedOpRow[col] == ' ') {
                break;
            }
            col++;
        }

        size_t endCol = col;

        // For Part 2: Read columns right-to-left, each column forms a number
        vector<long long> numbers;
        for (int c = endCol - 1; c >= (int)startCol; c--) {
            string digits;
            for (const auto& row : paddedNumberRows) {
                char ch = row[c];
                if (isdigit(ch)) {
                    digits += ch;
                }
            }
            if (!digits.empty()) {
                numbers.push_back(stoll(digits));
            }
        }

        string opStr = paddedOpRow.substr(startCol, endCol - startCol);
        opStr.erase(0, opStr.find_first_not_of(" \t"));
        opStr.erase(opStr.find_last_not_of(" \t") + 1);

        if (!opStr.empty() && !numbers.empty()) {
            Problem p;
            p.numbers = numbers;
            p.op = opStr[0];
            problems.push_back(p);
        }
    }

    return problems;
}

// Solve a single problem
long long solveProblem(const Problem& problem) {
    if (problem.op == '+') {
        long long sum = 0;
        for (long long n : problem.numbers) {
            sum += n;
        }
        return sum;
    } else if (problem.op == '*') {
        long long product = 1;
        for (long long n : problem.numbers) {
            product *= n;
        }
        return product;
    }
    return 0;
}

long long part1(const vector<string>& lines) {
    auto problems = parseProblems(lines);
    long long total = 0;
    for (const auto& problem : problems) {
        total += solveProblem(problem);
    }
    return total;
}

long long part2(const vector<string>& lines) {
    auto problems = parseProblemsPart2(lines);
    long long total = 0;
    for (const auto& problem : problems) {
        total += solveProblem(problem);
    }
    return total;
}

int main() {
    vector<string> lines = readInput("../input.txt");

    cout << "Part 1: " << part1(lines) << endl;
    cout << "Part 2: " << part2(lines) << endl;

    return 0;
}
