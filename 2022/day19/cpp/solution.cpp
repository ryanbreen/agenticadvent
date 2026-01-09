#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <regex>
#include <unordered_map>
#include <algorithm>

struct Blueprint {
    int id;
    int oreOre;
    int clayOre;
    int obsOre;
    int obsClay;
    int geoOre;
    int geoObs;
};

std::vector<Blueprint> parseInput(const std::string& filename) {
    std::vector<Blueprint> blueprints;
    std::ifstream file(filename);
    std::string line;

    std::regex pattern(R"(Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.)");

    while (std::getline(file, line)) {
        std::smatch match;
        if (std::regex_search(line, match, pattern)) {
            Blueprint bp;
            bp.id = std::stoi(match[1]);
            bp.oreOre = std::stoi(match[2]);
            bp.clayOre = std::stoi(match[3]);
            bp.obsOre = std::stoi(match[4]);
            bp.obsClay = std::stoi(match[5]);
            bp.geoOre = std::stoi(match[6]);
            bp.geoObs = std::stoi(match[7]);
            blueprints.push_back(bp);
        }
    }
    return blueprints;
}

class GeodeSolver {
private:
    const Blueprint& bp;
    int timeLimit;
    int maxOre;
    int maxClay;
    int maxObs;
    int best;
    std::unordered_map<uint64_t, int> seen;

    uint64_t makeKey(int time, int ore, int clay, int obs, int oreR, int clayR, int obsR, int geoR) {
        // Pack state into 64-bit key
        return ((uint64_t)time << 56) |
               ((uint64_t)(ore & 0xFF) << 48) |
               ((uint64_t)(clay & 0xFF) << 40) |
               ((uint64_t)(obs & 0xFF) << 32) |
               ((uint64_t)(oreR & 0xFF) << 24) |
               ((uint64_t)(clayR & 0xFF) << 16) |
               ((uint64_t)(obsR & 0xFF) << 8) |
               ((uint64_t)(geoR & 0xFF));
    }

    void dfs(int time, int ore, int clay, int obs, int geodes,
             int oreR, int clayR, int obsR, int geoR) {
        // Pruning: upper bound on possible geodes
        int remaining = timeLimit - time;
        int upperBound = geodes + geoR * remaining + (remaining * (remaining - 1)) / 2;
        if (upperBound <= best) return;

        if (time == timeLimit) {
            best = std::max(best, geodes);
            return;
        }

        // Cap resources
        int cappedOre = std::min(ore, remaining * maxOre);
        int cappedClay = std::min(clay, remaining * maxClay);
        int cappedObs = std::min(obs, remaining * maxObs);

        // State deduplication
        uint64_t key = makeKey(time, cappedOre, cappedClay, cappedObs, oreR, clayR, obsR, geoR);
        auto it = seen.find(key);
        if (it != seen.end() && it->second >= geodes) return;
        seen[key] = geodes;

        // Collect resources
        int newOre = cappedOre + oreR;
        int newClay = cappedClay + clayR;
        int newObs = cappedObs + obsR;
        int newGeodes = geodes + geoR;

        // Try building geode robot (always do if possible)
        if (cappedOre >= bp.geoOre && cappedObs >= bp.geoObs) {
            dfs(time + 1, newOre - bp.geoOre, newClay, newObs - bp.geoObs, newGeodes,
                oreR, clayR, obsR, geoR + 1);
            return; // If we can build geode, always do
        }

        // Try building obsidian robot
        if (cappedOre >= bp.obsOre && cappedClay >= bp.obsClay && obsR < maxObs) {
            dfs(time + 1, newOre - bp.obsOre, newClay - bp.obsClay, newObs, newGeodes,
                oreR, clayR, obsR + 1, geoR);
        }

        // Try building clay robot
        if (cappedOre >= bp.clayOre && clayR < maxClay) {
            dfs(time + 1, newOre - bp.clayOre, newClay, newObs, newGeodes,
                oreR, clayR + 1, obsR, geoR);
        }

        // Try building ore robot
        if (cappedOre >= bp.oreOre && oreR < maxOre) {
            dfs(time + 1, newOre - bp.oreOre, newClay, newObs, newGeodes,
                oreR + 1, clayR, obsR, geoR);
        }

        // Do nothing (wait)
        dfs(time + 1, newOre, newClay, newObs, newGeodes,
            oreR, clayR, obsR, geoR);
    }

public:
    GeodeSolver(const Blueprint& blueprint, int limit)
        : bp(blueprint), timeLimit(limit), best(0) {
        maxOre = std::max({bp.oreOre, bp.clayOre, bp.obsOre, bp.geoOre});
        maxClay = bp.obsClay;
        maxObs = bp.geoObs;
    }

    int solve() {
        seen.clear();
        best = 0;
        dfs(0, 0, 0, 0, 0, 1, 0, 0, 0);
        return best;
    }
};

int part1(const std::vector<Blueprint>& blueprints) {
    int total = 0;
    for (const auto& bp : blueprints) {
        GeodeSolver solver(bp, 24);
        int geodes = solver.solve();
        total += bp.id * geodes;
    }
    return total;
}

int part2(const std::vector<Blueprint>& blueprints) {
    int result = 1;
    int count = std::min((int)blueprints.size(), 3);
    for (int i = 0; i < count; i++) {
        GeodeSolver solver(blueprints[i], 32);
        int geodes = solver.solve();
        result *= geodes;
    }
    return result;
}

int main() {
    auto blueprints = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(blueprints) << std::endl;
    std::cout << "Part 2: " << part2(blueprints) << std::endl;

    return 0;
}
