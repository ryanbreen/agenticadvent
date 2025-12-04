#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 200

// Direction offsets for 8 adjacent positions (including diagonals)
int dx[] = {-1, -1, -1, 0, 0, 1, 1, 1};
int dy[] = {-1, 0, 1, -1, 1, -1, 0, 1};

int rows = 0;
int cols = 0;
char grid[MAX_SIZE][MAX_SIZE];
char working_grid[MAX_SIZE][MAX_SIZE];

void read_input() {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        exit(1);
    }

    char line[MAX_SIZE];
    rows = 0;
    while (fgets(line, sizeof(line), fp)) {
        // Remove newline
        int len = strlen(line);
        if (len > 0 && line[len-1] == '\n') {
            line[len-1] = '\0';
            len--;
        }
        if (len == 0) continue;

        cols = len;
        strcpy(grid[rows], line);
        rows++;
    }

    fclose(fp);
}

int count_neighbors(int row, int col, char g[MAX_SIZE][MAX_SIZE]) {
    int count = 0;
    for (int i = 0; i < 8; i++) {
        int nr = row + dx[i];
        int nc = col + dy[i];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && g[nr][nc] == '@') {
            count++;
        }
    }
    return count;
}

int part1() {
    int accessible = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '@') {
                int neighbors = count_neighbors(r, c, grid);
                if (neighbors < 4) {
                    accessible++;
                }
            }
        }
    }
    return accessible;
}

int part2() {
    // Copy grid to working grid
    for (int r = 0; r < rows; r++) {
        strcpy(working_grid[r], grid[r]);
    }

    int total_removed = 0;

    while (1) {
        // Find all accessible rolls
        int accessible_positions[MAX_SIZE * MAX_SIZE][2];
        int accessible_count = 0;

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (working_grid[r][c] == '@') {
                    int neighbors = count_neighbors(r, c, working_grid);
                    if (neighbors < 4) {
                        accessible_positions[accessible_count][0] = r;
                        accessible_positions[accessible_count][1] = c;
                        accessible_count++;
                    }
                }
            }
        }

        // If no accessible rolls found, stop
        if (accessible_count == 0) {
            break;
        }

        // Remove all accessible rolls
        for (int i = 0; i < accessible_count; i++) {
            int r = accessible_positions[i][0];
            int c = accessible_positions[i][1];
            working_grid[r][c] = '.';
        }

        total_removed += accessible_count;
    }

    return total_removed;
}

int main() {
    read_input();

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
