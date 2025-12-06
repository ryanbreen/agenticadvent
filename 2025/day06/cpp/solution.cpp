#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <sstream>

struct Problem {
    std::vector<long long> numbers;
    char op;
};

// Read input file into lines
std::vector<std::string> readInput(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open file " << filename << std::endl;
        return {};
    }

    std::vector<std::string> lines;
    std::string line;

    while (std::getline(file, line)) {
        lines.push_back(line);
    }

    return lines;
}

// Parse problems for Part 1
std::vector<Problem> parseProblems(const std::vector<std::string>& lines) {
    if (lines.empty()) {
        return {};
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    int opRowIdx = lines.size() - 1;
    while (opRowIdx >= 0) {
        std::string trimmed = lines[opRowIdx];
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

    std::string opRow = lines[opRowIdx];
    std::vector<std::string> numberRows(lines.begin(), lines.begin() + opRowIdx);

    // Find max width
    size_t maxWidth = 0;
    for (const auto& line : lines) {
        maxWidth = std::max(maxWidth, line.length());
    }

    // Pad all rows to the same width
    std::vector<std::string> paddedNumberRows;
    for (const auto& row : numberRows) {
        std::string padded = row;
        while (padded.length() < maxWidth) {
            padded += ' ';
        }
        paddedNumberRows.push_back(padded);
    }

    std::string paddedOpRow = opRow;
    while (paddedOpRow.length() < maxWidth) {
        paddedOpRow += ' ';
    }

    // Find problem boundaries
    std::vector<Problem> problems;
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
        std::vector<long long> numbers;
        for (const auto& row : paddedNumberRows) {
            std::string numStr = row.substr(startCol, endCol - startCol);
            // Trim whitespace
            numStr.erase(0, numStr.find_first_not_of(" \t"));
            numStr.erase(numStr.find_last_not_of(" \t") + 1);

            if (!numStr.empty()) {
                numbers.push_back(std::stoll(numStr));
            }
        }

        std::string opStr = paddedOpRow.substr(startCol, endCol - startCol);
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
std::vector<Problem> parseProblemsPart2(const std::vector<std::string>& lines) {
    if (lines.empty()) {
        return {};
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    int opRowIdx = lines.size() - 1;
    while (opRowIdx >= 0) {
        std::string trimmed = lines[opRowIdx];
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

    std::string opRow = lines[opRowIdx];
    std::vector<std::string> numberRows(lines.begin(), lines.begin() + opRowIdx);

    // Find max width
    size_t maxWidth = 0;
    for (const auto& line : lines) {
        maxWidth = std::max(maxWidth, line.length());
    }

    // Pad all rows to the same width
    std::vector<std::string> paddedNumberRows;
    for (const auto& row : numberRows) {
        std::string padded = row;
        while (padded.length() < maxWidth) {
            padded += ' ';
        }
        paddedNumberRows.push_back(padded);
    }

    std::string paddedOpRow = opRow;
    while (paddedOpRow.length() < maxWidth) {
        paddedOpRow += ' ';
    }

    // Find problem boundaries
    std::vector<Problem> problems;
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
        std::vector<long long> numbers;
        for (int c = endCol - 1; c >= (int)startCol; c--) {
            std::string digits;
            for (const auto& row : paddedNumberRows) {
                char ch = row[c];
                if (std::isdigit(ch)) {
                    digits += ch;
                }
            }
            if (!digits.empty()) {
                numbers.push_back(std::stoll(digits));
            }
        }

        std::string opStr = paddedOpRow.substr(startCol, endCol - startCol);
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

long long part1(const std::vector<std::string>& lines) {
    auto problems = parseProblems(lines);
    long long total = 0;
    for (const auto& problem : problems) {
        total += solveProblem(problem);
    }
    return total;
}

long long part2(const std::vector<std::string>& lines) {
    auto problems = parseProblemsPart2(lines);
    long long total = 0;
    for (const auto& problem : problems) {
        total += solveProblem(problem);
    }
    return total;
}

int main() {
    std::vector<std::string> lines = readInput("../input.txt");

    std::cout << "Part 1: " << part1(lines) << std::endl;
    std::cout << "Part 2: " << part2(lines) << std::endl;

    return 0;
}
