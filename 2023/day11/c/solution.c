#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINES 200
#define MAX_LINE_LEN 200
#define MAX_GALAXIES 500

typedef struct {
    int row;
    int col;
} Galaxy;

static char grid[MAX_LINES][MAX_LINE_LEN];
static int num_rows = 0;
static int num_cols = 0;

static Galaxy galaxies[MAX_GALAXIES];
static int num_galaxies = 0;

static bool empty_rows[MAX_LINES];
static bool empty_cols[MAX_LINE_LEN];

static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), f)) {
        size_t len = strlen(line);
        while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        strcpy(grid[num_rows], line);
        if (num_cols == 0) {
            num_cols = (int)len;
        }
        num_rows++;
    }
    fclose(f);
}

static void find_galaxies(void) {
    for (int r = 0; r < num_rows; r++) {
        for (int c = 0; c < num_cols; c++) {
            if (grid[r][c] == '#') {
                galaxies[num_galaxies].row = r;
                galaxies[num_galaxies].col = c;
                num_galaxies++;
            }
        }
    }
}

static void find_empty_rows_and_cols(void) {
    /* Initialize all rows and columns as empty */
    for (int r = 0; r < num_rows; r++) {
        empty_rows[r] = true;
    }
    for (int c = 0; c < num_cols; c++) {
        empty_cols[c] = true;
    }

    /* Mark rows and columns that contain galaxies as not empty */
    for (int i = 0; i < num_galaxies; i++) {
        empty_rows[galaxies[i].row] = false;
        empty_cols[galaxies[i].col] = false;
    }
}

static long long calculate_distances(long long expansion_factor) {
    long long total = 0;

    for (int i = 0; i < num_galaxies; i++) {
        for (int j = i + 1; j < num_galaxies; j++) {
            int r1 = galaxies[i].row;
            int c1 = galaxies[i].col;
            int r2 = galaxies[j].row;
            int c2 = galaxies[j].col;

            /* Calculate row distance with expansion */
            int min_r = (r1 < r2) ? r1 : r2;
            int max_r = (r1 > r2) ? r1 : r2;
            long long row_dist = max_r - min_r;
            for (int r = min_r; r < max_r; r++) {
                if (empty_rows[r]) {
                    row_dist += expansion_factor - 1;
                }
            }

            /* Calculate column distance with expansion */
            int min_c = (c1 < c2) ? c1 : c2;
            int max_c = (c1 > c2) ? c1 : c2;
            long long col_dist = max_c - min_c;
            for (int c = min_c; c < max_c; c++) {
                if (empty_cols[c]) {
                    col_dist += expansion_factor - 1;
                }
            }

            total += row_dist + col_dist;
        }
    }

    return total;
}

static long long part1(void) {
    return calculate_distances(2);
}

static long long part2(void) {
    return calculate_distances(1000000);
}

int main(int argc, char *argv[]) {
    const char *filename = (argc > 1) ? argv[1] : "../input.txt";

    parse_input(filename);
    find_galaxies();
    find_empty_rows_and_cols();

    printf("Part 1: %lld\n", part1());
    printf("Part 2: %lld\n", part2());

    return 0;
}
