#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <set>
#include <vector>
#include <utility>
#include <cstdlib>

using namespace std;

struct Point {
    int x, y;

    bool operator<(const Point& other) const {
        if (x != other.x) return x < other.x;
        return y < other.y;
    }
};

int sign(int x) {
    if (x == 0) return 0;
    return x > 0 ? 1 : -1;
}

Point moveTail(const Point& head, const Point& tail) {
    int dx = head.x - tail.x;
    int dy = head.y - tail.y;

    // If adjacent or overlapping, don't move
    if (abs(dx) <= 1 && abs(dy) <= 1) {
        return tail;
    }

    // Move toward head
    return {tail.x + sign(dx), tail.y + sign(dy)};
}

int simulateRope(const vector<pair<char, int>>& moves, int ropeLength) {
    vector<Point> knots(ropeLength, {0, 0});
    set<Point> visited;
    visited.insert(knots.back());

    for (const auto& move : moves) {
        char direction = move.first;
        int count = move.second;

        int dx = 0, dy = 0;
        switch (direction) {
            case 'U': dy = 1; break;
            case 'D': dy = -1; break;
            case 'L': dx = -1; break;
            case 'R': dx = 1; break;
        }

        for (int step = 0; step < count; step++) {
            // Move head
            knots[0].x += dx;
            knots[0].y += dy;

            // Move each subsequent knot
            for (int i = 1; i < ropeLength; i++) {
                knots[i] = moveTail(knots[i-1], knots[i]);
            }

            visited.insert(knots.back());
        }
    }

    return visited.size();
}

int part1(const vector<pair<char, int>>& moves) {
    return simulateRope(moves, 2);
}

int part2(const vector<pair<char, int>>& moves) {
    return simulateRope(moves, 10);
}

int main() {
    ifstream file("../input.txt");
    if (!file.is_open()) {
        cerr << "Could not open input file: ../input.txt" << endl;
        return 1;
    }

    vector<pair<char, int>> moves;
    string line;
    while (getline(file, line)) {
        if (line.empty()) continue;
        char direction = line[0];
        int count = stoi(line.substr(2));
        moves.emplace_back(direction, count);
    }

    cout << "Part 1: " << part1(moves) << endl;
    cout << "Part 2: " << part2(moves) << endl;

    return 0;
}
