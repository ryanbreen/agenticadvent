#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <set>
#include <queue>
#include <algorithm>
#include <tuple>

struct Brick {
    int x1, y1, z1, x2, y2, z2;
};

std::vector<Brick> parse_input(const std::string& filename) {
    std::vector<Brick> bricks;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        // Parse "x1,y1,z1~x2,y2,z2"
        size_t tilde = line.find('~');
        std::string left = line.substr(0, tilde);
        std::string right = line.substr(tilde + 1);

        Brick b;
        char comma;
        std::istringstream lss(left);
        lss >> b.x1 >> comma >> b.y1 >> comma >> b.z1;
        std::istringstream rss(right);
        rss >> b.x2 >> comma >> b.y2 >> comma >> b.z2;

        // Ensure z1 <= z2 for consistent processing
        if (b.z1 > b.z2) {
            std::swap(b.x1, b.x2);
            std::swap(b.y1, b.y2);
            std::swap(b.z1, b.z2);
        }

        bricks.push_back(b);
    }

    return bricks;
}

struct SettleResult {
    std::vector<Brick> settled;
    std::vector<std::set<int>> supports;    // supports[i] = bricks that i supports (above)
    std::vector<std::set<int>> supporters;  // supporters[i] = bricks that support i (below)
};

SettleResult settle_bricks(std::vector<Brick>& bricks) {
    int n = bricks.size();

    // Create indices and sort by minimum z
    std::vector<int> order(n);
    for (int i = 0; i < n; i++) order[i] = i;
    std::sort(order.begin(), order.end(), [&](int a, int b) {
        return std::min(bricks[a].z1, bricks[a].z2) < std::min(bricks[b].z1, bricks[b].z2);
    });

    // Track occupied cells: (x, y, z) -> brick index
    std::map<std::tuple<int, int, int>, int> occupied;

    std::vector<Brick> settled(n);
    std::vector<std::set<int>> supports(n);
    std::vector<std::set<int>> supporters(n);

    for (int orig_idx : order) {
        Brick& brick = bricks[orig_idx];
        int x1 = brick.x1, y1 = brick.y1, z1 = brick.z1;
        int x2 = brick.x2, y2 = brick.y2, z2 = brick.z2;

        // Find the lowest z where this brick can rest
        int drop = z1 - 1;  // Maximum drop (to z=1)

        // Get xy footprint and find max obstacle
        for (int x = std::min(x1, x2); x <= std::max(x1, x2); x++) {
            for (int y = std::min(y1, y2); y <= std::max(y1, y2); y++) {
                // Check each z level below the brick
                for (int z = z1 - 1; z >= 1; z--) {
                    auto key = std::make_tuple(x, y, z);
                    if (occupied.find(key) != occupied.end()) {
                        drop = std::min(drop, z1 - z - 1);
                        break;
                    }
                }
            }
        }

        // Drop the brick
        int new_z1 = z1 - drop;
        int new_z2 = z2 - drop;
        Brick new_brick = {x1, y1, new_z1, x2, y2, new_z2};
        settled[orig_idx] = new_brick;

        // Mark cells as occupied and find supporters
        for (int x = std::min(x1, x2); x <= std::max(x1, x2); x++) {
            for (int y = std::min(y1, y2); y <= std::max(y1, y2); y++) {
                // Check if there's a brick directly below
                auto below_key = std::make_tuple(x, y, new_z1 - 1);
                auto it = occupied.find(below_key);
                if (it != occupied.end()) {
                    int supporter_idx = it->second;
                    supporters[orig_idx].insert(supporter_idx);
                    supports[supporter_idx].insert(orig_idx);
                }

                // Mark all cells of this brick as occupied
                for (int z = new_z1; z <= new_z2; z++) {
                    occupied[std::make_tuple(x, y, z)] = orig_idx;
                }
            }
        }
    }

    return {settled, supports, supporters};
}

int part1(std::vector<Brick>& bricks) {
    auto result = settle_bricks(bricks);
    auto& supports = result.supports;
    auto& supporters = result.supporters;

    int safe_count = 0;
    int n = bricks.size();

    for (int i = 0; i < n; i++) {
        // Brick i can be safely removed if every brick it supports
        // has at least one other supporter
        bool can_remove = true;
        for (int supported : supports[i]) {
            if (supporters[supported].size() == 1) {
                can_remove = false;
                break;
            }
        }
        if (can_remove) {
            safe_count++;
        }
    }

    return safe_count;
}

int part2(std::vector<Brick>& bricks) {
    auto result = settle_bricks(bricks);
    auto& supports = result.supports;
    auto& supporters = result.supporters;

    int total_falls = 0;
    int n = bricks.size();

    for (int i = 0; i < n; i++) {
        // Simulate removing brick i and count chain reaction
        // BFS to find all bricks that would fall
        std::set<int> falling;
        falling.insert(i);
        std::queue<int> queue;
        queue.push(i);

        while (!queue.empty()) {
            int brick = queue.front();
            queue.pop();

            // Check all bricks that this brick supports
            for (int supported : supports[brick]) {
                if (falling.count(supported)) continue;

                // This brick falls if all its supporters have fallen
                bool all_fallen = true;
                for (int s : supporters[supported]) {
                    if (!falling.count(s)) {
                        all_fallen = false;
                        break;
                    }
                }

                if (all_fallen) {
                    falling.insert(supported);
                    queue.push(supported);
                }
            }
        }

        // Don't count the initial brick we removed
        total_falls += falling.size() - 1;
    }

    return total_falls;
}

int main() {
    std::string input_path = "../input.txt";
    auto bricks = parse_input(input_path);

    std::cout << "Part 1: " << part1(bricks) << std::endl;
    std::cout << "Part 2: " << part2(bricks) << std::endl;

    return 0;
}
