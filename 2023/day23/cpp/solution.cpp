/**
 * Day 23: A Long Walk - Longest path through hiking trails
 *
 * Algorithm:
 * 1. Parse the grid of paths (.), forest (#), and slopes (^,v,<,>)
 * 2. Find junction points (start, end, and cells with 3+ walkable neighbors)
 * 3. Build a compressed graph between junctions with edge weights
 * 4. Part 1: Respect slope directions when building edges
 * 5. Part 2: Ignore slopes (treat as paths)
 * 6. DFS with backtracking to find longest path
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <limits>

using namespace std;

// Hash function for pair<int, int>
struct PairHash {
    size_t operator()(const pair<int, int>& p) const {
        return hash<long long>()(((long long)p.first << 32) | (unsigned int)p.second);
    }
};

using Point = pair<int, int>;
using JunctionSet = unordered_set<Point, PairHash>;
using Graph = unordered_map<Point, unordered_map<Point, int, PairHash>, PairHash>;

// Directions: up, down, left, right
const int dr[] = {-1, 1, 0, 0};
const int dc[] = {0, 0, -1, 1};

// Slope to direction mapping
unordered_map<char, pair<int, int>> slopeDir = {
    {'^', {-1, 0}},
    {'v', {1, 0}},
    {'<', {0, -1}},
    {'>', {0, 1}}
};

vector<string> parseInput(const string& filename) {
    ifstream file(filename);
    vector<string> grid;
    string line;
    while (getline(file, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }
    return grid;
}

JunctionSet findJunctions(const vector<string>& grid) {
    int rows = grid.size();
    int cols = grid[0].size();
    JunctionSet junctions;

    // Find start and end points
    Point start = {0, (int)grid[0].find('.')};
    Point end = {rows - 1, (int)grid[rows - 1].find('.')};
    junctions.insert(start);
    junctions.insert(end);

    // Find intersections (cells with 3+ walkable neighbors)
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '#') continue;

            int neighbors = 0;
            for (int d = 0; d < 4; d++) {
                int nr = r + dr[d];
                int nc = c + dc[d];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
                    neighbors++;
                }
            }
            if (neighbors >= 3) {
                junctions.insert({r, c});
            }
        }
    }

    return junctions;
}

Graph buildGraph(const vector<string>& grid, const JunctionSet& junctions, bool respectSlopes) {
    int rows = grid.size();
    int cols = grid[0].size();
    Graph graph;

    for (const Point& startJunction : junctions) {
        // BFS/DFS from each junction to find reachable junctions
        vector<pair<Point, int>> stack;
        unordered_set<Point, PairHash> visited;

        stack.push_back({startJunction, 0});
        visited.insert(startJunction);

        while (!stack.empty()) {
            auto [pos, dist] = stack.back();
            stack.pop_back();
            auto [r, c] = pos;

            if (dist > 0 && junctions.count(pos)) {
                // Found another junction
                graph[startJunction][pos] = dist;
                continue;
            }

            // Explore neighbors
            for (int d = 0; d < 4; d++) {
                int nr = r + dr[d];
                int nc = c + dc[d];

                if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
                if (grid[nr][nc] == '#') continue;
                if (visited.count({nr, nc})) continue;

                // Check slope constraints for Part 1
                if (respectSlopes) {
                    char cell = grid[r][c];
                    if (slopeDir.count(cell)) {
                        auto [reqDr, reqDc] = slopeDir[cell];
                        if (dr[d] != reqDr || dc[d] != reqDc) {
                            continue;
                        }
                    }
                }

                visited.insert({nr, nc});
                stack.push_back({{nr, nc}, dist + 1});
            }
        }
    }

    return graph;
}

// DFS with backtracking to find longest path
int longestPathDFS(const Graph& graph, const Point& start, const Point& end) {
    // Convert graph to index-based for efficiency
    vector<Point> nodes;
    unordered_map<Point, int, PairHash> nodeIndex;

    for (const auto& [node, _] : graph) {
        if (!nodeIndex.count(node)) {
            nodeIndex[node] = nodes.size();
            nodes.push_back(node);
        }
        for (const auto& [neighbor, __] : graph.at(node)) {
            if (!nodeIndex.count(neighbor)) {
                nodeIndex[neighbor] = nodes.size();
                nodes.push_back(neighbor);
            }
        }
    }

    int n = nodes.size();
    int startIdx = nodeIndex[start];
    int endIdx = nodeIndex[end];

    // Build adjacency list with indices
    vector<vector<pair<int, int>>> adj(n);
    for (const auto& [from, edges] : graph) {
        int fromIdx = nodeIndex[from];
        for (const auto& [to, dist] : edges) {
            int toIdx = nodeIndex[to];
            adj[fromIdx].push_back({toIdx, dist});
        }
    }

    // Use bitmask for visited (works up to 64 nodes, which is sufficient for this problem)
    int maxDist = INT_MIN;
    uint64_t visited = 0;

    // Iterative DFS with stack (stores: node index, distance, visited mask, neighbor index)
    // Actually, recursive with bitmask is cleaner and works well here

    function<int(int, uint64_t)> dfs = [&](int node, uint64_t visited) -> int {
        if (node == endIdx) {
            return 0;
        }

        visited |= (1ULL << node);
        int maxDist = INT_MIN;

        for (const auto& [neighbor, dist] : adj[node]) {
            if (!(visited & (1ULL << neighbor))) {
                int result = dfs(neighbor, visited);
                if (result != INT_MIN) {
                    maxDist = max(maxDist, dist + result);
                }
            }
        }

        return maxDist;
    };

    return dfs(startIdx, 0);
}

int solve(const vector<string>& grid, bool respectSlopes) {
    int rows = grid.size();
    Point start = {0, (int)grid[0].find('.')};
    Point end = {rows - 1, (int)grid[rows - 1].find('.')};

    JunctionSet junctions = findJunctions(grid);
    Graph graph = buildGraph(grid, junctions, respectSlopes);

    return longestPathDFS(graph, start, end);
}

int part1(const vector<string>& grid) {
    return solve(grid, true);
}

int part2(const vector<string>& grid) {
    return solve(grid, false);
}

int main() {
    string inputPath = "../input.txt";
    vector<string> grid = parseInput(inputPath);

    cout << "Part 1: " << part1(grid) << endl;
    cout << "Part 2: " << part2(grid) << endl;

    return 0;
}
