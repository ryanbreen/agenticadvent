#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <climits>

// Hash function for pair<int,int>
struct PairHash {
    size_t operator()(const std::pair<int, int>& p) const {
        return std::hash<long long>()(static_cast<long long>(p.first) << 32 | static_cast<unsigned int>(p.second));
    }
};

using ElfSet = std::unordered_set<std::pair<int, int>, PairHash>;

ElfSet parse_input(const std::string& text) {
    ElfSet elves;
    std::istringstream stream(text);
    std::string line;
    int row = 0;
    while (std::getline(stream, line)) {
        for (int col = 0; col < static_cast<int>(line.size()); ++col) {
            if (line[col] == '#') {
                elves.insert({row, col});
            }
        }
        ++row;
    }
    return elves;
}

// Direction checks and moves
// N: check NW, N, NE; move N
// S: check SW, S, SE; move S
// W: check NW, W, SW; move W
// E: check NE, E, SE; move E
struct Direction {
    std::vector<std::pair<int, int>> checks;
    std::pair<int, int> move;
};

std::vector<Direction> get_directions() {
    return {
        // N
        {{{-1, -1}, {-1, 0}, {-1, 1}}, {-1, 0}},
        // S
        {{{1, -1}, {1, 0}, {1, 1}}, {1, 0}},
        // W
        {{{-1, -1}, {0, -1}, {1, -1}}, {0, -1}},
        // E
        {{{-1, 1}, {0, 1}, {1, 1}}, {0, 1}}
    };
}

// All 8 neighbors
const std::vector<std::pair<int, int>> ALL_NEIGHBORS = {
    {-1, -1}, {-1, 0}, {-1, 1},
    {0, -1},           {0, 1},
    {1, -1},  {1, 0},  {1, 1}
};

std::pair<ElfSet, bool> simulate_round(const ElfSet& elves, int start_dir) {
    static std::vector<Direction> directions = get_directions();

    // Phase 1: Each elf proposes a move
    std::unordered_map<std::pair<int, int>, std::pair<int, int>, PairHash> proposals;
    std::unordered_map<std::pair<int, int>, int, PairHash> proposal_counts;

    for (const auto& elf : elves) {
        int r = elf.first;
        int c = elf.second;

        // Check if any neighbors
        bool has_neighbor = false;
        for (const auto& [dr, dc] : ALL_NEIGHBORS) {
            if (elves.count({r + dr, c + dc})) {
                has_neighbor = true;
                break;
            }
        }

        if (!has_neighbor) {
            continue;
        }

        // Try each direction
        for (int i = 0; i < 4; ++i) {
            int dir_idx = (start_dir + i) % 4;
            const Direction& dir = directions[dir_idx];

            bool can_move = true;
            for (const auto& [dr, dc] : dir.checks) {
                if (elves.count({r + dr, c + dc})) {
                    can_move = false;
                    break;
                }
            }

            if (can_move) {
                std::pair<int, int> new_pos = {r + dir.move.first, c + dir.move.second};
                proposals[elf] = new_pos;
                proposal_counts[new_pos]++;
                break;
            }
        }
    }

    // Phase 2: Execute moves (only if unique proposal)
    ElfSet new_elves;
    bool moved = false;

    for (const auto& elf : elves) {
        auto it = proposals.find(elf);
        if (it != proposals.end()) {
            const auto& new_pos = it->second;
            if (proposal_counts[new_pos] == 1) {
                new_elves.insert(new_pos);
                moved = true;
            } else {
                new_elves.insert(elf);
            }
        } else {
            new_elves.insert(elf);
        }
    }

    return {new_elves, moved};
}

int bounding_rect_empty(const ElfSet& elves) {
    int min_r = INT_MAX, max_r = INT_MIN;
    int min_c = INT_MAX, max_c = INT_MIN;

    for (const auto& [r, c] : elves) {
        min_r = std::min(min_r, r);
        max_r = std::max(max_r, r);
        min_c = std::min(min_c, c);
        max_c = std::max(max_c, c);
    }

    int area = (max_r - min_r + 1) * (max_c - min_c + 1);
    return area - static_cast<int>(elves.size());
}

int part1(const std::string& text) {
    ElfSet elves = parse_input(text);
    int start_dir = 0;

    for (int round = 0; round < 10; ++round) {
        auto [new_elves, moved] = simulate_round(elves, start_dir);
        elves = std::move(new_elves);
        start_dir = (start_dir + 1) % 4;
    }

    return bounding_rect_empty(elves);
}

int part2(const std::string& text) {
    ElfSet elves = parse_input(text);
    int start_dir = 0;
    int round_num = 0;

    while (true) {
        ++round_num;
        auto [new_elves, moved] = simulate_round(elves, start_dir);
        if (!moved) {
            return round_num;
        }
        elves = std::move(new_elves);
        start_dir = (start_dir + 1) % 4;
    }
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();

    std::cout << "Part 1: " << part1(text) << std::endl;
    std::cout << "Part 2: " << part2(text) << std::endl;

    return 0;
}
