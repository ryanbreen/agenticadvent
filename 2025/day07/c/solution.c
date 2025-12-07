#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ROWS 256
#define MAX_COLS 256

typedef struct {
    int col;
    unsigned long long count;
} Timeline;

typedef struct {
    Timeline timelines[MAX_COLS];
    int size;
} TimelineMap;

char grid[MAX_ROWS][MAX_COLS + 1];
int rows = 0;
int cols = 0;

void read_input(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        exit(1);
    }

    rows = 0;
    while (fgets(grid[rows], sizeof(grid[rows]), file) && rows < MAX_ROWS) {
        // Remove newline
        size_t len = strlen(grid[rows]);
        if (len > 0 && grid[rows][len - 1] == '\n') {
            grid[rows][len - 1] = '\0';
            len--;
        }
        if (len > 0) {
            if (cols == 0) {
                cols = len;
            }
            rows++;
        }
    }

    fclose(file);
}

int find_start_col() {
    for (int col = 0; col < cols; col++) {
        if (grid[0][col] == 'S') {
            return col;
        }
    }
    return -1;
}

int part1() {
    int start_col = find_start_col();
    if (start_col == -1) {
        return 0;
    }

    // Track active beam columns at each row using a set-like array
    bool active_beams[MAX_COLS] = {false};
    bool new_beams[MAX_COLS] = {false};
    active_beams[start_col] = true;
    int split_count = 0;

    // Process row by row starting from row 1 (below S)
    for (int row = 1; row < rows; row++) {
        memset(new_beams, false, sizeof(new_beams));

        for (int col = 0; col < cols; col++) {
            if (active_beams[col]) {
                char cell = grid[row][col];

                if (cell == '^') {
                    // Beam hits splitter - count it and emit left/right
                    split_count++;
                    if (col - 1 >= 0) {
                        new_beams[col - 1] = true;
                    }
                    if (col + 1 < cols) {
                        new_beams[col + 1] = true;
                    }
                } else if (cell == '.' || cell == 'S') {
                    // Beam continues straight down
                    new_beams[col] = true;
                } else {
                    // Other characters - beam continues
                    new_beams[col] = true;
                }
            }
        }

        memcpy(active_beams, new_beams, sizeof(active_beams));

        // Check if no more beams
        bool has_beams = false;
        for (int i = 0; i < cols; i++) {
            if (active_beams[i]) {
                has_beams = true;
                break;
            }
        }
        if (!has_beams) {
            break;
        }
    }

    return split_count;
}

unsigned long long part2() {
    int start_col = find_start_col();
    if (start_col == -1) {
        return 0;
    }

    // Track number of timelines at each column position
    TimelineMap timelines = {.size = 0};
    TimelineMap new_timelines = {.size = 0};

    timelines.timelines[0].col = start_col;
    timelines.timelines[0].count = 1;
    timelines.size = 1;

    // Process row by row starting from row 1 (below S)
    for (int row = 1; row < rows; row++) {
        new_timelines.size = 0;

        for (int i = 0; i < timelines.size; i++) {
            int col = timelines.timelines[i].col;
            unsigned long long count = timelines.timelines[i].count;

            if (col >= 0 && col < cols) {
                char cell = grid[row][col];

                if (cell == '^') {
                    // Each timeline splits into 2 (left and right)
                    if (col - 1 >= 0) {
                        // Check if this column already exists in new_timelines
                        bool found = false;
                        for (int j = 0; j < new_timelines.size; j++) {
                            if (new_timelines.timelines[j].col == col - 1) {
                                new_timelines.timelines[j].count += count;
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            new_timelines.timelines[new_timelines.size].col = col - 1;
                            new_timelines.timelines[new_timelines.size].count = count;
                            new_timelines.size++;
                        }
                    }
                    if (col + 1 < cols) {
                        // Check if this column already exists in new_timelines
                        bool found = false;
                        for (int j = 0; j < new_timelines.size; j++) {
                            if (new_timelines.timelines[j].col == col + 1) {
                                new_timelines.timelines[j].count += count;
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            new_timelines.timelines[new_timelines.size].col = col + 1;
                            new_timelines.timelines[new_timelines.size].count = count;
                            new_timelines.size++;
                        }
                    }
                } else if (cell == '.' || cell == 'S') {
                    // Timelines continue straight down
                    bool found = false;
                    for (int j = 0; j < new_timelines.size; j++) {
                        if (new_timelines.timelines[j].col == col) {
                            new_timelines.timelines[j].count += count;
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        new_timelines.timelines[new_timelines.size].col = col;
                        new_timelines.timelines[new_timelines.size].count = count;
                        new_timelines.size++;
                    }
                } else {
                    // Other characters - timelines continue
                    bool found = false;
                    for (int j = 0; j < new_timelines.size; j++) {
                        if (new_timelines.timelines[j].col == col) {
                            new_timelines.timelines[j].count += count;
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        new_timelines.timelines[new_timelines.size].col = col;
                        new_timelines.timelines[new_timelines.size].count = count;
                        new_timelines.size++;
                    }
                }
            }
        }

        // Copy new_timelines to timelines
        memcpy(&timelines, &new_timelines, sizeof(TimelineMap));

        if (timelines.size == 0) {
            break;
        }
    }

    // Sum all timeline counts
    unsigned long long total = 0;
    for (int i = 0; i < timelines.size; i++) {
        total += timelines.timelines[i].count;
    }

    return total;
}

int main() {
    read_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %llu\n", part2());

    return 0;
}
