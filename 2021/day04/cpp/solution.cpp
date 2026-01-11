#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <array>

using Board = std::array<std::array<int, 5>, 5>;
using Marked = std::array<std::array<bool, 5>, 5>;

struct ParsedInput {
    std::vector<int> numbers;
    std::vector<Board> boards;
};

ParsedInput parseInput(const std::string& filename) {
    ParsedInput result;
    std::ifstream file(filename);
    std::string line;

    // Read the first line of numbers
    std::getline(file, line);
    std::stringstream ss(line);
    std::string num;
    while (std::getline(ss, num, ',')) {
        result.numbers.push_back(std::stoi(num));
    }

    // Read boards
    while (std::getline(file, line)) {
        if (line.empty()) continue;

        Board board;
        // First row already in 'line'
        std::stringstream rowss(line);
        for (int col = 0; col < 5; ++col) {
            rowss >> board[0][col];
        }

        // Read remaining 4 rows
        for (int row = 1; row < 5; ++row) {
            std::getline(file, line);
            std::stringstream rowss2(line);
            for (int col = 0; col < 5; ++col) {
                rowss2 >> board[row][col];
            }
        }

        result.boards.push_back(board);
    }

    return result;
}

bool checkWinner(const Marked& marked) {
    // Check rows
    for (int row = 0; row < 5; ++row) {
        bool rowWin = true;
        for (int col = 0; col < 5; ++col) {
            if (!marked[row][col]) {
                rowWin = false;
                break;
            }
        }
        if (rowWin) return true;
    }

    // Check columns
    for (int col = 0; col < 5; ++col) {
        bool colWin = true;
        for (int row = 0; row < 5; ++row) {
            if (!marked[row][col]) {
                colWin = false;
                break;
            }
        }
        if (colWin) return true;
    }

    return false;
}

int calculateScore(const Board& board, const Marked& marked, int lastNumber) {
    int unmarkedSum = 0;
    for (int row = 0; row < 5; ++row) {
        for (int col = 0; col < 5; ++col) {
            if (!marked[row][col]) {
                unmarkedSum += board[row][col];
            }
        }
    }
    return unmarkedSum * lastNumber;
}

void markNumber(const Board& board, Marked& marked, int number) {
    for (int row = 0; row < 5; ++row) {
        for (int col = 0; col < 5; ++col) {
            if (board[row][col] == number) {
                marked[row][col] = true;
            }
        }
    }
}

int part1(const std::vector<int>& numbers, const std::vector<Board>& boards) {
    std::vector<Marked> marked(boards.size());
    for (auto& m : marked) {
        for (auto& row : m) {
            row.fill(false);
        }
    }

    for (int number : numbers) {
        for (size_t i = 0; i < boards.size(); ++i) {
            markNumber(boards[i], marked[i], number);
            if (checkWinner(marked[i])) {
                return calculateScore(boards[i], marked[i], number);
            }
        }
    }

    return -1;
}

int part2(const std::vector<int>& numbers, const std::vector<Board>& boards) {
    std::vector<Marked> marked(boards.size());
    for (auto& m : marked) {
        for (auto& row : m) {
            row.fill(false);
        }
    }

    std::vector<bool> won(boards.size(), false);
    int lastScore = -1;

    for (int number : numbers) {
        for (size_t i = 0; i < boards.size(); ++i) {
            if (won[i]) continue;

            markNumber(boards[i], marked[i], number);
            if (checkWinner(marked[i])) {
                won[i] = true;
                lastScore = calculateScore(boards[i], marked[i], number);
            }
        }
    }

    return lastScore;
}

int main() {
    ParsedInput input = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(input.numbers, input.boards) << std::endl;
    std::cout << "Part 2: " << part2(input.numbers, input.boards) << std::endl;

    return 0;
}
