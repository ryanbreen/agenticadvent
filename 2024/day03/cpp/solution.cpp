#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <vector>
#include <algorithm>

std::string readInput(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open file " << filename << std::endl;
        exit(1);
    }

    std::string content;
    std::string line;
    while (std::getline(file, line)) {
        content += line;
        if (!file.eof()) {
            content += '\n';
        }
    }
    return content;
}

int part1(const std::string& data) {
    // Find all valid mul(X,Y) instructions and sum their products
    std::regex pattern(R"(mul\((\d{1,3}),(\d{1,3})\))");
    std::sregex_iterator iter(data.begin(), data.end(), pattern);
    std::sregex_iterator end;

    int total = 0;
    for (; iter != end; ++iter) {
        int x = std::stoi((*iter)[1].str());
        int y = std::stoi((*iter)[2].str());
        total += x * y;
    }

    return total;
}

int part2(const std::string& data) {
    // Like part1, but do() enables and don't() disables mul instructions
    std::regex mul_pattern(R"(mul\((\d{1,3}),(\d{1,3})\))");
    std::regex do_pattern(R"(do\(\))");
    std::regex dont_pattern(R"(don't\(\))");

    struct Event {
        size_t pos;
        std::string type;
        int x;
        int y;

        bool operator<(const Event& other) const {
            return pos < other.pos;
        }
    };

    std::vector<Event> events;

    // Find all mul instructions
    std::sregex_iterator mul_iter(data.begin(), data.end(), mul_pattern);
    std::sregex_iterator end;
    for (; mul_iter != end; ++mul_iter) {
        Event e;
        e.pos = mul_iter->position();
        e.type = "mul";
        e.x = std::stoi((*mul_iter)[1].str());
        e.y = std::stoi((*mul_iter)[2].str());
        events.push_back(e);
    }

    // Find all do() instructions
    std::sregex_iterator do_iter(data.begin(), data.end(), do_pattern);
    for (; do_iter != end; ++do_iter) {
        Event e;
        e.pos = do_iter->position();
        e.type = "do";
        e.x = 0;
        e.y = 0;
        events.push_back(e);
    }

    // Find all don't() instructions
    std::sregex_iterator dont_iter(data.begin(), data.end(), dont_pattern);
    for (; dont_iter != end; ++dont_iter) {
        Event e;
        e.pos = dont_iter->position();
        e.type = "dont";
        e.x = 0;
        e.y = 0;
        events.push_back(e);
    }

    // Sort by position
    std::sort(events.begin(), events.end());

    int total = 0;
    bool enabled = true;

    for (const auto& event : events) {
        if (event.type == "do") {
            enabled = true;
        } else if (event.type == "dont") {
            enabled = false;
        } else if (event.type == "mul" && enabled) {
            total += event.x * event.y;
        }
    }

    return total;
}

int main() {
    std::string data = readInput("../input.txt");

    std::cout << "Part 1: " << part1(data) << std::endl;
    std::cout << "Part 2: " << part2(data) << std::endl;

    return 0;
}
