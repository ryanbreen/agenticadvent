#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <utility>

std::vector<std::pair<char, char>> parseInput(const std::string& filename) {
    std::vector<std::pair<char, char>> rounds;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (!line.empty()) {
            rounds.emplace_back(line[0], line[2]);
        }
    }

    return rounds;
}

int part1(const std::vector<std::pair<char, char>>& rounds) {
    // X=Rock, Y=Paper, Z=Scissors
    // Shape scores: Rock=1, Paper=2, Scissors=3
    // Outcome: 0=loss, 3=draw, 6=win
    // A=Rock, B=Paper, C=Scissors

    int total = 0;
    for (const auto& [opp, me] : rounds) {
        // Shape score
        int shape = me - 'X' + 1;  // X=1, Y=2, Z=3

        // Outcome score
        int outcome = 0;
        int oppIdx = opp - 'A';  // A=0, B=1, C=2
        int meIdx = me - 'X';    // X=0, Y=1, Z=2

        if (oppIdx == meIdx) {
            outcome = 3;  // Draw
        } else if ((meIdx - oppIdx + 3) % 3 == 1) {
            outcome = 6;  // Win: Rock beats Scissors, Paper beats Rock, Scissors beats Paper
        } else {
            outcome = 0;  // Loss
        }

        total += shape + outcome;
    }

    return total;
}

int part2(const std::vector<std::pair<char, char>>& rounds) {
    // X=lose, Y=draw, Z=win
    // Figure out what shape to play

    int total = 0;
    for (const auto& [opp, result] : rounds) {
        int oppIdx = opp - 'A';  // A=0 (Rock), B=1 (Paper), C=2 (Scissors)

        int shape, outcome;
        if (result == 'X') {
            // Need to lose
            outcome = 0;
            shape = (oppIdx + 2) % 3 + 1;  // Play the losing shape
        } else if (result == 'Y') {
            // Need to draw
            outcome = 3;
            shape = oppIdx + 1;  // Play the same shape
        } else {
            // Need to win
            outcome = 6;
            shape = (oppIdx + 1) % 3 + 1;  // Play the winning shape
        }

        total += shape + outcome;
    }

    return total;
}

int main() {
    std::string inputFile = "../input.txt";
    auto rounds = parseInput(inputFile);

    std::cout << "Part 1: " << part1(rounds) << std::endl;
    std::cout << "Part 2: " << part2(rounds) << std::endl;

    return 0;
}
