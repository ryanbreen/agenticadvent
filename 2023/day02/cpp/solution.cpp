#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>

struct CubeSet {
    int red = 0;
    int green = 0;
    int blue = 0;
};

struct Game {
    int id;
    std::vector<CubeSet> sets;
};

Game parseLine(const std::string& line) {
    Game game;

    // Find the colon that separates game ID from cube data
    size_t colonPos = line.find(':');
    if (colonPos == std::string::npos) {
        return game;
    }

    // Parse game ID
    std::string gameIdPart = line.substr(5, colonPos - 5); // Skip "Game "
    game.id = std::stoi(gameIdPart);

    // Parse sets (separated by semicolons)
    std::string setsData = line.substr(colonPos + 1);
    std::istringstream setsStream(setsData);
    std::string setStr;

    while (std::getline(setsStream, setStr, ';')) {
        CubeSet cubeSet;

        // Parse individual colors in this set (separated by commas)
        std::istringstream colorStream(setStr);
        std::string colorStr;

        while (std::getline(colorStream, colorStr, ',')) {
            // Trim leading spaces
            size_t start = colorStr.find_first_not_of(" ");
            if (start != std::string::npos) {
                colorStr = colorStr.substr(start);
            }

            // Parse count and color
            std::istringstream colorPart(colorStr);
            int count;
            std::string color;
            colorPart >> count >> color;

            if (color == "red") {
                cubeSet.red = count;
            } else if (color == "green") {
                cubeSet.green = count;
            } else if (color == "blue") {
                cubeSet.blue = count;
            }
        }

        game.sets.push_back(cubeSet);
    }

    return game;
}

bool isGamePossible(const Game& game, int maxRed, int maxGreen, int maxBlue) {
    for (const auto& set : game.sets) {
        if (set.red > maxRed || set.green > maxGreen || set.blue > maxBlue) {
            return false;
        }
    }
    return true;
}

int getMinimumPower(const Game& game) {
    int minRed = 0;
    int minGreen = 0;
    int minBlue = 0;

    for (const auto& set : game.sets) {
        minRed = std::max(minRed, set.red);
        minGreen = std::max(minGreen, set.green);
        minBlue = std::max(minBlue, set.blue);
    }

    return minRed * minGreen * minBlue;
}

int part1(const std::vector<Game>& games) {
    const int MAX_RED = 12;
    const int MAX_GREEN = 13;
    const int MAX_BLUE = 14;

    int sum = 0;
    for (const auto& game : games) {
        if (isGamePossible(game, MAX_RED, MAX_GREEN, MAX_BLUE)) {
            sum += game.id;
        }
    }
    return sum;
}

int part2(const std::vector<Game>& games) {
    int sum = 0;
    for (const auto& game : games) {
        sum += getMinimumPower(game);
    }
    return sum;
}

int main() {
    std::ifstream inputFile("../input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }

    std::vector<Game> games;
    std::string line;

    while (std::getline(inputFile, line)) {
        if (!line.empty()) {
            games.push_back(parseLine(line));
        }
    }

    inputFile.close();

    std::cout << "Part 1: " << part1(games) << std::endl;
    std::cout << "Part 2: " << part2(games) << std::endl;

    return 0;
}
