#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <queue>
#include <unordered_map>
#include <cstdlib>

struct Point {
    int r, c;

    bool operator==(const Point& other) const {
        return r == other.r && c == other.c;
    }
};

struct PointHash {
    size_t operator()(const Point& p) const {
        return std::hash<int>()(p.r) ^ (std::hash<int>()(p.c) << 16);
    }
};

using Grid = std::vector<std::string>;
using DistMap = std::unordered_map<Point, int, PointHash>;

struct ParseResult {
    Grid grid;
    Point start;
    Point end;
};

ParseResult parse_grid(const std::string& input) {
    ParseResult result;
    std::istringstream iss(input);
    std::string line;
    int r = 0;

    while (std::getline(iss, line)) {
        if (line.empty()) continue;
        result.grid.push_back(line);
        for (int c = 0; c < static_cast<int>(line.size()); ++c) {
            if (line[c] == 'S') {
                result.start = {r, c};
            } else if (line[c] == 'E') {
                result.end = {r, c};
            }
        }
        ++r;
    }

    return result;
}

DistMap trace_path(const Grid& grid, const Point& start, const Point& end) {
    int rows = static_cast<int>(grid.size());
    int cols = static_cast<int>(grid[0].size());

    DistMap dist;
    dist[start] = 0;

    std::queue<Point> queue;
    queue.push(start);

    const int dr[] = {-1, 1, 0, 0};
    const int dc[] = {0, 0, -1, 1};

    while (!queue.empty()) {
        Point curr = queue.front();
        queue.pop();

        if (curr == end) break;

        for (int i = 0; i < 4; ++i) {
            int nr = curr.r + dr[i];
            int nc = curr.c + dc[i];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
                Point next = {nr, nc};
                if (dist.find(next) == dist.end()) {
                    dist[next] = dist[curr] + 1;
                    queue.push(next);
                }
            }
        }
    }

    return dist;
}

long long count_cheats(const DistMap& dist, int max_cheat_time, int min_savings) {
    long long count = 0;

    std::vector<std::pair<Point, int>> track_positions(dist.begin(), dist.end());

    for (const auto& [p1, d1] : track_positions) {
        for (const auto& [p2, d2] : track_positions) {
            int cheat_cost = std::abs(p2.r - p1.r) + std::abs(p2.c - p1.c);
            if (cheat_cost <= max_cheat_time) {
                int savings = d2 - d1 - cheat_cost;
                if (savings >= min_savings) {
                    ++count;
                }
            }
        }
    }

    return count;
}

long long part1(const Grid& grid, const Point& start, const Point& end) {
    DistMap dist = trace_path(grid, start, end);
    return count_cheats(dist, 2, 100);
}

long long part2(const Grid& grid, const Point& start, const Point& end) {
    DistMap dist = trace_path(grid, start, end);
    return count_cheats(dist, 20, 100);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Cannot open input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string input = buffer.str();

    auto [grid, start, end] = parse_grid(input);

    std::cout << "Part 1: " << part1(grid, start, end) << std::endl;
    std::cout << "Part 2: " << part2(grid, start, end) << std::endl;

    return 0;
}
