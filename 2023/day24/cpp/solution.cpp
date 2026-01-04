#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cmath>
#include <array>

using ld = long double;

struct Hailstone {
    long long px, py, pz;
    long long vx, vy, vz;
};

std::vector<Hailstone> parse_input(const std::string& filename) {
    std::vector<Hailstone> hailstones;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        Hailstone h;
        for (char& c : line) {
            if (c == ',' || c == '@') c = ' ';
        }
        std::istringstream iss(line);
        iss >> h.px >> h.py >> h.pz >> h.vx >> h.vy >> h.vz;
        hailstones.push_back(h);
    }
    return hailstones;
}

// Part 1: Count 2D intersections in test area
int part1(const std::vector<Hailstone>& hailstones) {
    const ld MIN_COORD = 200000000000000.0L;
    const ld MAX_COORD = 400000000000000.0L;
    int count = 0;

    for (size_t i = 0; i < hailstones.size(); i++) {
        for (size_t j = i + 1; j < hailstones.size(); j++) {
            const Hailstone& h1 = hailstones[i];
            const Hailstone& h2 = hailstones[j];

            ld det = (ld)h1.vx * (-h2.vy) - (-h2.vx) * (ld)h1.vy;
            if (std::abs(det) < 1e-10L) continue;

            ld dx = h2.px - h1.px;
            ld dy = h2.py - h1.py;

            ld t1 = (dx * (-h2.vy) - (-h2.vx) * dy) / det;
            ld t2 = ((ld)h1.vx * dy - dx * (ld)h1.vy) / det;

            if (t1 < 0 || t2 < 0) continue;

            ld x = h1.px + h1.vx * t1;
            ld y = h1.py + h1.vy * t1;

            if (x >= MIN_COORD && x <= MAX_COORD && y >= MIN_COORD && y <= MAX_COORD) {
                count++;
            }
        }
    }
    return count;
}

// Solve 4x4 system using Gaussian elimination with long double
std::array<ld, 4> solve_system_ld(std::array<std::array<ld, 4>, 4> matrix,
                                   std::array<ld, 4> rhs) {
    std::array<std::array<ld, 5>, 4> aug;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            aug[i][j] = matrix[i][j];
        }
        aug[i][4] = rhs[i];
    }

    // Forward elimination with partial pivoting
    for (int col = 0; col < 4; col++) {
        int max_row = col;
        for (int row = col + 1; row < 4; row++) {
            if (std::abs(aug[row][col]) > std::abs(aug[max_row][col])) {
                max_row = row;
            }
        }
        std::swap(aug[col], aug[max_row]);

        if (std::abs(aug[col][col]) < 1e-20L) continue;

        for (int row = col + 1; row < 4; row++) {
            ld factor = aug[row][col] / aug[col][col];
            for (int j = col; j < 5; j++) {
                aug[row][j] = aug[row][j] - factor * aug[col][j];
            }
        }
    }

    // Back substitution
    std::array<ld, 4> solution;
    for (int i = 3; i >= 0; i--) {
        solution[i] = aug[i][4];
        for (int j = i + 1; j < 4; j++) {
            solution[i] = solution[i] - aug[i][j] * solution[j];
        }
        solution[i] = solution[i] / aug[i][i];
    }

    return solution;
}

// Part 2: Find rock that hits all hailstones
long long part2(const std::vector<Hailstone>& hailstones) {
    // Build XY system
    std::array<std::array<ld, 4>, 4> matrix_xy;
    std::array<ld, 4> rhs_xy;

    for (int i = 0; i < 4; i++) {
        const Hailstone& h1 = hailstones[i];
        const Hailstone& h2 = hailstones[i + 1];

        ld a = (ld)h1.vy - h2.vy;
        ld b = (ld)h2.vx - h1.vx;
        ld c = (ld)h2.py - h1.py;
        ld d = (ld)h1.px - h2.px;
        ld e = (ld)h1.px * h1.vy - (ld)h1.py * h1.vx
             - ((ld)h2.px * h2.vy - (ld)h2.py * h2.vx);

        matrix_xy[i][0] = a;
        matrix_xy[i][1] = b;
        matrix_xy[i][2] = c;
        matrix_xy[i][3] = d;
        rhs_xy[i] = e;
    }

    auto sol_xy = solve_system_ld(matrix_xy, rhs_xy);
    ld rx = sol_xy[0];
    ld ry = sol_xy[1];

    // Build XZ system
    std::array<std::array<ld, 4>, 4> matrix_xz;
    std::array<ld, 4> rhs_xz;

    for (int i = 0; i < 4; i++) {
        const Hailstone& h1 = hailstones[i];
        const Hailstone& h2 = hailstones[i + 1];

        ld a = (ld)h1.vz - h2.vz;
        ld b = (ld)h2.vx - h1.vx;
        ld c = (ld)h2.pz - h1.pz;
        ld d = (ld)h1.px - h2.px;
        ld e = (ld)h1.px * h1.vz - (ld)h1.pz * h1.vx
             - ((ld)h2.px * h2.vz - (ld)h2.pz * h2.vx);

        matrix_xz[i][0] = a;
        matrix_xz[i][1] = b;
        matrix_xz[i][2] = c;
        matrix_xz[i][3] = d;
        rhs_xz[i] = e;
    }

    auto sol_xz = solve_system_ld(matrix_xz, rhs_xz);
    ld rz = sol_xz[1];

    return (long long)std::round(rx) + (long long)std::round(ry) + (long long)std::round(rz);
}

int main() {
    auto hailstones = parse_input("../input.txt");

    std::cout << "Part 1: " << part1(hailstones) << std::endl;
    std::cout << "Part 2: " << part2(hailstones) << std::endl;

    return 0;
}
