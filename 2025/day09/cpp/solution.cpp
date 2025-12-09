#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <map>

using namespace std;

struct Point {
    int x, y;
};

vector<Point> parse_input(const string& filename) {
    vector<Point> points;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        if (line.empty()) continue;
        size_t comma = line.find(',');
        int x = stoi(line.substr(0, comma));
        int y = stoi(line.substr(comma + 1));
        points.push_back({x, y});
    }

    return points;
}

long long part1(const vector<Point>& points) {
    long long max_area = 0;
    int n = points.size();

    // Check all pairs of points as opposite corners
    for (int i = 0; i < n; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;

        for (int j = i + 1; j < n; j++) {
            int x2 = points[j].x;
            int y2 = points[j].y;

            // Rectangle area = width * height (inclusive of both corners)
            long long width = abs(x2 - x1) + 1;
            long long height = abs(y2 - y1) + 1;
            long long area = width * height;

            max_area = max(max_area, area);
        }
    }

    return max_area;
}

struct HorizontalEdge {
    int y, x_min, x_max;
};

struct VerticalEdge {
    int x, y_min, y_max;
};

bool is_inside_polygon(double x, double y,
                       const map<int, vector<pair<int, int>>>& vert_by_x) {
    // Ray casting algorithm - cast ray to the right
    double crossings = 0;

    for (const auto& [vx, edges] : vert_by_x) {
        if (vx <= x) continue;

        for (const auto& [y_min, y_max] : edges) {
            if (y_min < y && y < y_max) {
                // Strict inequality - ray crosses edge
                crossings += 1;
            } else if (y == y_min || y == y_max) {
                // On corner - count as 0.5 crossing
                crossings += 0.5;
            }
        }
    }

    return fmod(crossings, 2.0) == 1.0;
}

bool rectangle_valid(int x1, int y1, int x2, int y2,
                     const map<int, vector<pair<int, int>>>& vert_by_x,
                     const map<int, vector<pair<int, int>>>& horiz_by_y) {
    int min_x = min(x1, x2);
    int max_x = max(x1, x2);
    int min_y = min(y1, y2);
    int max_y = max(y1, y2);

    // Check if any vertical edge crosses through the rectangle interior
    for (const auto& [vx, edges] : vert_by_x) {
        if (min_x < vx && vx < max_x) {  // Vertical edge x is inside rectangle's x range
            for (const auto& [y_min, y_max] : edges) {
                // Check if this edge segment overlaps with rectangle's y range
                if (!(y_max <= min_y || y_min >= max_y)) {
                    return false;
                }
            }
        }
    }

    // Check if any horizontal edge crosses through the rectangle interior
    for (const auto& [hy, edges] : horiz_by_y) {
        if (min_y < hy && hy < max_y) {  // Horizontal edge y is inside rectangle's y range
            for (const auto& [x_min, x_max] : edges) {
                // Check if this edge segment overlaps with rectangle's x range
                if (!(x_max <= min_x || x_min >= max_x)) {
                    return false;
                }
            }
        }
    }

    // Finally, check that we're inside the polygon (not outside)
    // Check center point
    double center_x = (min_x + max_x) / 2.0;
    double center_y = (min_y + max_y) / 2.0;
    return is_inside_polygon(center_x, center_y, vert_by_x);
}

long long part2(const vector<Point>& points) {
    int n = points.size();
    vector<HorizontalEdge> horizontal_edges;
    vector<VerticalEdge> vertical_edges;

    // Build edges connecting consecutive points
    for (int i = 0; i < n; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;
        int x2 = points[(i + 1) % n].x;
        int y2 = points[(i + 1) % n].y;

        if (y1 == y2) {  // Horizontal edge
            horizontal_edges.push_back({y1, min(x1, x2), max(x1, x2)});
        } else {  // Vertical edge
            vertical_edges.push_back({x1, min(y1, y2), max(y1, y2)});
        }
    }

    // Sort vertical edges by x coordinate
    sort(vertical_edges.begin(), vertical_edges.end(),
         [](const VerticalEdge& a, const VerticalEdge& b) {
             return a.x < b.x;
         });

    // Build maps for efficient lookup
    map<int, vector<pair<int, int>>> vert_by_x;
    for (const auto& edge : vertical_edges) {
        vert_by_x[edge.x].push_back({edge.y_min, edge.y_max});
    }

    map<int, vector<pair<int, int>>> horiz_by_y;
    for (const auto& edge : horizontal_edges) {
        horiz_by_y[edge.y].push_back({edge.x_min, edge.x_max});
    }

    // Find largest valid rectangle with red corners
    long long max_area = 0;

    for (int i = 0; i < n; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;

        for (int j = i + 1; j < n; j++) {
            int x2 = points[j].x;
            int y2 = points[j].y;

            if (rectangle_valid(x1, y1, x2, y2, vert_by_x, horiz_by_y)) {
                long long width = abs(x2 - x1) + 1;
                long long height = abs(y2 - y1) + 1;
                long long area = width * height;
                max_area = max(max_area, area);
            }
        }
    }

    return max_area;
}

int main() {
    vector<Point> points = parse_input("../input.txt");

    cout << "Part 1: " << part1(points) << endl;
    cout << "Part 2: " << part2(points) << endl;

    return 0;
}
