#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_ROWS 256
#define MAX_COLS 256
#define MAX_PATH 10000
#define MAX_LINE 8192

typedef struct {
    int is_number;
    int value;    // number of steps or direction: 'L' or 'R'
} Instruction;

char grid[MAX_ROWS][MAX_COLS];
int height = 0;
int width = 0;
Instruction instructions[MAX_PATH];
int num_instructions = 0;

// Directions: 0=right, 1=down, 2=left, 3=up
int DR[] = {0, 1, 0, -1};
int DC[] = {1, 0, -1, 0};

void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[MAX_LINE];

    // Read grid lines
    while (fgets(line, sizeof(line), f)) {
        // Check for empty line (separator between grid and path)
        if (line[0] == '\n' || line[0] == '\r') {
            break;
        }

        int len = strlen(line);
        // Remove newline
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }

        // Copy line to grid, tracking width
        strcpy(grid[height], line);
        if (len > width) {
            width = len;
        }
        height++;
    }

    // Pad all rows to the same width
    for (int r = 0; r < height; r++) {
        int len = strlen(grid[r]);
        while (len < width) {
            grid[r][len++] = ' ';
        }
        grid[r][len] = '\0';
    }

    // Read path line
    if (fgets(line, sizeof(line), f)) {
        int len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }

        // Parse path into instructions
        int i = 0;
        while (i < len) {
            if (isdigit(line[i])) {
                int num = 0;
                while (i < len && isdigit(line[i])) {
                    num = num * 10 + (line[i] - '0');
                    i++;
                }
                instructions[num_instructions].is_number = 1;
                instructions[num_instructions].value = num;
                num_instructions++;
            } else {
                instructions[num_instructions].is_number = 0;
                instructions[num_instructions].value = line[i];
                num_instructions++;
                i++;
            }
        }
    }

    fclose(f);
}

int part1(void) {
    // Find starting position (leftmost open tile on top row)
    int row = 0, col = 0;
    while (grid[0][col] != '.') col++;
    int facing = 0;

    for (int i = 0; i < num_instructions; i++) {
        if (instructions[i].is_number) {
            int steps = instructions[i].value;
            for (int s = 0; s < steps; s++) {
                int dr = DR[facing], dc = DC[facing];
                int nr = row + dr, nc = col + dc;

                // Wrap around if needed
                if (facing == 0) { // Right
                    if (nc >= width || grid[nr][nc] == ' ') {
                        nc = 0;
                        while (grid[nr][nc] == ' ') nc++;
                    }
                } else if (facing == 2) { // Left
                    if (nc < 0 || grid[nr][nc] == ' ') {
                        nc = width - 1;
                        while (grid[nr][nc] == ' ') nc--;
                    }
                } else if (facing == 1) { // Down
                    if (nr >= height || grid[nr][nc] == ' ') {
                        nr = 0;
                        while (grid[nr][nc] == ' ') nr++;
                    }
                } else if (facing == 3) { // Up
                    if (nr < 0 || grid[nr][nc] == ' ') {
                        nr = height - 1;
                        while (grid[nr][nc] == ' ') nr--;
                    }
                }

                // Check if we hit a wall
                if (grid[nr][nc] == '#') {
                    break;
                }

                row = nr;
                col = nc;
            }
        } else {
            // Turn
            if (instructions[i].value == 'R') {
                facing = (facing + 1) % 4;
            } else {
                facing = (facing + 3) % 4;  // -1 mod 4
            }
        }
    }

    return 1000 * (row + 1) + 4 * (col + 1) + facing;
}

// Get cube face and local coordinates for the actual input layout:
//    12
//    3
//   45
//   6
void get_cube_face(int row, int col, int face_size, int *face, int *lr, int *lc) {
    int face_row = row / face_size;
    int face_col = col / face_size;
    *lr = row % face_size;
    *lc = col % face_size;

    if (face_row == 0 && face_col == 1) *face = 1;
    else if (face_row == 0 && face_col == 2) *face = 2;
    else if (face_row == 1 && face_col == 1) *face = 3;
    else if (face_row == 2 && face_col == 0) *face = 4;
    else if (face_row == 2 && face_col == 1) *face = 5;
    else if (face_row == 3 && face_col == 0) *face = 6;
    else *face = -1;
}

// Wrap on cube edges for the specific layout
void wrap_cube(int row, int col, int facing, int face_size, int *nr, int *nc, int *nf) {
    int S = face_size;
    int face, lr, lc;
    get_cube_face(row, col, S, &face, &lr, &lc);

    // Default: no change
    *nr = row;
    *nc = col;
    *nf = facing;

    if (face == 1) {
        if (facing == 3) { // Up: goes to face 6, from left, facing right
            *nr = 3*S + lc;
            *nc = 0;
            *nf = 0;
        } else if (facing == 2) { // Left: goes to face 4, from left, facing right (inverted)
            *nr = 3*S - 1 - lr;
            *nc = 0;
            *nf = 0;
        }
    } else if (face == 2) {
        if (facing == 0) { // Right: goes to face 5, from right, facing left (inverted)
            *nr = 3*S - 1 - lr;
            *nc = 2*S - 1;
            *nf = 2;
        } else if (facing == 1) { // Down: goes to face 3, from right, facing left
            *nr = S + lc;
            *nc = 2*S - 1;
            *nf = 2;
        } else if (facing == 3) { // Up: goes to face 6, from bottom, facing up
            *nr = 4*S - 1;
            *nc = lc;
            *nf = 3;
        }
    } else if (face == 3) {
        if (facing == 0) { // Right: goes to face 2, from bottom, facing up
            *nr = S - 1;
            *nc = 2*S + lr;
            *nf = 3;
        } else if (facing == 2) { // Left: goes to face 4, from top, facing down
            *nr = 2*S;
            *nc = lr;
            *nf = 1;
        }
    } else if (face == 4) {
        if (facing == 3) { // Up: goes to face 3, from left, facing right
            *nr = S + lc;
            *nc = S;
            *nf = 0;
        } else if (facing == 2) { // Left: goes to face 1, from left, facing right (inverted)
            *nr = S - 1 - lr;
            *nc = S;
            *nf = 0;
        }
    } else if (face == 5) {
        if (facing == 0) { // Right: goes to face 2, from right, facing left (inverted)
            *nr = S - 1 - lr;
            *nc = 3*S - 1;
            *nf = 2;
        } else if (facing == 1) { // Down: goes to face 6, from right, facing left
            *nr = 3*S + lc;
            *nc = S - 1;
            *nf = 2;
        }
    } else if (face == 6) {
        if (facing == 0) { // Right: goes to face 5, from bottom, facing up
            *nr = 3*S - 1;
            *nc = S + lr;
            *nf = 3;
        } else if (facing == 1) { // Down: goes to face 2, from top, facing down
            *nr = 0;
            *nc = 2*S + lc;
            *nf = 1;
        } else if (facing == 2) { // Left: goes to face 1, from top, facing down
            *nr = 0;
            *nc = S + lr;
            *nf = 1;
        }
    }
}

int part2(void) {
    // Determine face size
    int face_size = (height > 50) ? 50 : 4;

    // Find starting position
    int row = 0, col = 0;
    while (grid[0][col] != '.') col++;
    int facing = 0;

    for (int i = 0; i < num_instructions; i++) {
        if (instructions[i].is_number) {
            int steps = instructions[i].value;
            for (int s = 0; s < steps; s++) {
                int dr = DR[facing], dc = DC[facing];
                int nr = row + dr, nc = col + dc;
                int nf = facing;

                // Check if we need to wrap
                int need_wrap = 0;
                if (nr < 0 || nr >= height || nc < 0 || nc >= width) {
                    need_wrap = 1;
                } else if (grid[nr][nc] == ' ') {
                    need_wrap = 1;
                }

                if (need_wrap) {
                    wrap_cube(row, col, facing, face_size, &nr, &nc, &nf);
                }

                // Check if we hit a wall
                if (grid[nr][nc] == '#') {
                    break;
                }

                row = nr;
                col = nc;
                facing = nf;
            }
        } else {
            // Turn
            if (instructions[i].value == 'R') {
                facing = (facing + 1) % 4;
            } else {
                facing = (facing + 3) % 4;
            }
        }
    }

    return 1000 * (row + 1) + 4 * (col + 1) + facing;
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
