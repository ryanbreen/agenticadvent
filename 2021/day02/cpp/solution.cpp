#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

struct Command {
    std::string direction;
    int value;
};

std::vector<Command> parseInput() {
    std::vector<Command> commands;
    std::ifstream file("../input.txt");
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        std::istringstream iss(line);
        Command cmd;
        iss >> cmd.direction >> cmd.value;
        commands.push_back(cmd);
    }

    return commands;
}

long long part1(const std::vector<Command>& commands) {
    long long horizontal = 0;
    long long depth = 0;

    for (const auto& cmd : commands) {
        if (cmd.direction == "forward") {
            horizontal += cmd.value;
        } else if (cmd.direction == "down") {
            depth += cmd.value;
        } else if (cmd.direction == "up") {
            depth -= cmd.value;
        }
    }

    return horizontal * depth;
}

long long part2(const std::vector<Command>& commands) {
    long long horizontal = 0;
    long long depth = 0;
    long long aim = 0;

    for (const auto& cmd : commands) {
        if (cmd.direction == "forward") {
            horizontal += cmd.value;
            depth += aim * cmd.value;
        } else if (cmd.direction == "down") {
            aim += cmd.value;
        } else if (cmd.direction == "up") {
            aim -= cmd.value;
        }
    }

    return horizontal * depth;
}

int main() {
    std::vector<Command> commands = parseInput();

    std::cout << "Part 1: " << part1(commands) << std::endl;
    std::cout << "Part 2: " << part2(commands) << std::endl;

    return 0;
}
