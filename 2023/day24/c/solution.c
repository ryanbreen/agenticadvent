#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_HAILSTONES 400

typedef long long i64;
typedef long double ld;

typedef struct {
    i64 px, py, pz;
    i64 vx, vy, vz;
} Hailstone;

// Parse hailstones from file
static int parse_input(const char *filename, Hailstone *hailstones) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("fopen");
        exit(1);
    }

    int count = 0;
    char line[256];
    while (fgets(line, sizeof(line), f)) {
        Hailstone *h = &hailstones[count];
        if (sscanf(line, "%lld, %lld, %lld @ %lld, %lld, %lld",
                   &h->px, &h->py, &h->pz, &h->vx, &h->vy, &h->vz) == 6) {
            count++;
        }
    }
    fclose(f);
    return count;
}

// Part 1: Find 2D intersections in future within bounds
static int part1(Hailstone *h, int n) {
    const double MIN_COORD = 200000000000000.0;
    const double MAX_COORD = 400000000000000.0;
    int count = 0;

    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            double vx1 = h[i].vx, vy1 = h[i].vy;
            double vx2 = h[j].vx, vy2 = h[j].vy;
            double px1 = h[i].px, py1 = h[i].py;
            double px2 = h[j].px, py2 = h[j].py;

            double det = vx1 * (-vy2) - (-vx2) * vy1;
            if (det == 0) continue;

            double dx = px2 - px1;
            double dy = py2 - py1;

            double t1 = (dx * (-vy2) - (-vx2) * dy) / det;
            double t2 = (vx1 * dy - dx * vy1) / det;

            if (t1 < 0 || t2 < 0) continue;

            double x = px1 + vx1 * t1;
            double y = py1 + vy1 * t1;

            if (x >= MIN_COORD && x <= MAX_COORD &&
                y >= MIN_COORD && y <= MAX_COORD) {
                count++;
            }
        }
    }

    return count;
}

// Solve 4x4 linear system using Gaussian elimination with long double
static void solve_system_4x4_ld(ld matrix[4][5], ld solution[4]) {
    // Forward elimination with partial pivoting
    for (int col = 0; col < 4; col++) {
        // Find pivot
        int max_row = col;
        for (int row = col + 1; row < 4; row++) {
            if (fabsl(matrix[row][col]) > fabsl(matrix[max_row][col])) {
                max_row = row;
            }
        }

        // Swap rows
        for (int k = 0; k < 5; k++) {
            ld tmp = matrix[col][k];
            matrix[col][k] = matrix[max_row][k];
            matrix[max_row][k] = tmp;
        }

        if (matrix[col][col] == 0) continue;

        // Eliminate column
        for (int row = col + 1; row < 4; row++) {
            if (matrix[row][col] != 0) {
                ld factor = matrix[row][col] / matrix[col][col];
                for (int k = col; k < 5; k++) {
                    matrix[row][k] -= factor * matrix[col][k];
                }
            }
        }
    }

    // Back substitution
    for (int i = 3; i >= 0; i--) {
        solution[i] = matrix[i][4];
        for (int j = i + 1; j < 4; j++) {
            solution[i] -= matrix[i][j] * solution[j];
        }
        solution[i] /= matrix[i][i];
    }
}

// Part 2: Find rock position that hits all hailstones
static i64 part2(Hailstone *h, int n __attribute__((unused))) {
    // Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
    // For hailstones i and j:
    // (vyi - vyj)*rx + (vxj - vxi)*ry + (pyj - pyi)*rvx + (pxi - pxj)*rvy
    //   = pxi*vyi - pyi*vxi - (pxj*vyj - pyj*vxj)

    ld matrix_xy[4][5];
    for (int i = 0; i < 4; i++) {
        ld px1 = h[i].px, py1 = h[i].py;
        ld vx1 = h[i].vx, vy1 = h[i].vy;
        ld px2 = h[i+1].px, py2 = h[i+1].py;
        ld vx2 = h[i+1].vx, vy2 = h[i+1].vy;

        matrix_xy[i][0] = vy1 - vy2;
        matrix_xy[i][1] = vx2 - vx1;
        matrix_xy[i][2] = py2 - py1;
        matrix_xy[i][3] = px1 - px2;
        matrix_xy[i][4] = px1*vy1 - py1*vx1 - (px2*vy2 - py2*vx2);
    }

    ld sol_xy[4];
    solve_system_4x4_ld(matrix_xy, sol_xy);
    i64 rx = (i64)roundl(sol_xy[0]);
    i64 ry = (i64)roundl(sol_xy[1]);

    // Build system for XZ plane to get rz
    ld matrix_xz[4][5];
    for (int i = 0; i < 4; i++) {
        ld px1 = h[i].px, pz1 = h[i].pz;
        ld vx1 = h[i].vx, vz1 = h[i].vz;
        ld px2 = h[i+1].px, pz2 = h[i+1].pz;
        ld vx2 = h[i+1].vx, vz2 = h[i+1].vz;

        matrix_xz[i][0] = vz1 - vz2;
        matrix_xz[i][1] = vx2 - vx1;
        matrix_xz[i][2] = pz2 - pz1;
        matrix_xz[i][3] = px1 - px2;
        matrix_xz[i][4] = px1*vz1 - pz1*vx1 - (px2*vz2 - pz2*vx2);
    }

    ld sol_xz[4];
    solve_system_4x4_ld(matrix_xz, sol_xz);
    i64 rz = (i64)roundl(sol_xz[1]);  // Second element is rz

    return rx + ry + rz;
}

int main(int argc, char **argv) {
    const char *filename = argc > 1 ? argv[1] : "../input.txt";

    Hailstone hailstones[MAX_HAILSTONES];
    int n = parse_input(filename, hailstones);

    printf("Part 1: %d\n", part1(hailstones, n));
    printf("Part 2: %lld\n", part2(hailstones, n));

    return 0;
}
