#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define WIDTH 7
#define MAX_HEIGHT 100000
#define PROFILE_DEPTH 30

// Rock shapes as (dx, dy) offsets from bottom-left
// Each rock ends with (-1, -1) sentinel
static const int ROCKS[5][6][2] = {
    {{0, 0}, {1, 0}, {2, 0}, {3, 0}, {-1, -1}},           // Horizontal line
    {{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}, {-1, -1}},   // Plus
    {{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}, {-1, -1}},   // L shape
    {{0, 0}, {0, 1}, {0, 2}, {0, 3}, {-1, -1}},           // Vertical line
    {{0, 0}, {1, 0}, {0, 1}, {1, 1}, {-1, -1}}            // Square
};

static const int ROCK_SIZES[5] = {4, 5, 5, 4, 4};

// Grid: each row is a bitmask of 7 columns
static uint8_t grid[MAX_HEIGHT];
static int height = 0;

static char *jets = NULL;
static int jet_len = 0;
static int jet_idx = 0;

static inline int is_occupied(int x, int y) {
    if (y < 0 || x < 0 || x >= WIDTH) return 1;
    if (y >= MAX_HEIGHT) return 0;
    return (grid[y] >> x) & 1;
}

static inline void set_occupied(int x, int y) {
    if (y >= 0 && y < MAX_HEIGHT) {
        grid[y] |= (1 << x);
    }
}

static int can_place(int rock_type, int x, int y) {
    for (int i = 0; i < ROCK_SIZES[rock_type]; i++) {
        int nx = x + ROCKS[rock_type][i][0];
        int ny = y + ROCKS[rock_type][i][1];
        if (is_occupied(nx, ny)) return 0;
    }
    return 1;
}

static void place_rock(int rock_type, int x, int y) {
    for (int i = 0; i < ROCK_SIZES[rock_type]; i++) {
        int nx = x + ROCKS[rock_type][i][0];
        int ny = y + ROCKS[rock_type][i][1];
        set_occupied(nx, ny);
        if (ny + 1 > height) height = ny + 1;
    }
}

// State for cycle detection: (rock_type, jet_idx, profile)
typedef struct {
    int rock_type;
    int jet_idx;
    int profile[WIDTH];  // Depth to first block in each column
    long rock_num;
    int height;
} State;

#define MAX_STATES 100000
static State states[MAX_STATES];
static int state_count = 0;

static void get_profile(int *profile) {
    for (int col = 0; col < WIDTH; col++) {
        profile[col] = PROFILE_DEPTH;  // Default: no block found within depth
        for (int row = 0; row < PROFILE_DEPTH && height - 1 - row >= 0; row++) {
            if ((grid[height - 1 - row] >> col) & 1) {
                profile[col] = row;
                break;
            }
        }
    }
}

static int profiles_equal(int *p1, int *p2) {
    for (int i = 0; i < WIDTH; i++) {
        if (p1[i] != p2[i]) return 0;
    }
    return 1;
}

static long simulate(long num_rocks) {
    memset(grid, 0, sizeof(grid));
    height = 0;
    jet_idx = 0;
    state_count = 0;

    long *heights = NULL;
    if (num_rocks > 10000) {
        heights = malloc(sizeof(long) * 20000);  // Store heights for cycle detection
    }

    for (long rock_num = 0; rock_num < num_rocks; rock_num++) {
        int rock_type = rock_num % 5;

        // Starting position
        int x = 2;
        int y = height + 3;

        while (1) {
            // Jet push
            int dx = (jets[jet_idx] == '>') ? 1 : -1;
            jet_idx = (jet_idx + 1) % jet_len;

            if (can_place(rock_type, x + dx, y)) {
                x += dx;
            }

            // Fall down
            if (can_place(rock_type, x, y - 1)) {
                y--;
            } else {
                // Rock stops
                place_rock(rock_type, x, y);
                break;
            }
        }

        if (heights) {
            if (rock_num < 20000) {
                heights[rock_num] = height;
            }
        }

        // Cycle detection for Part 2
        if (num_rocks > 10000 && rock_num > 100) {
            int profile[WIDTH];
            get_profile(profile);

            // Look for matching state
            for (int i = 0; i < state_count; i++) {
                if (states[i].rock_type == rock_type &&
                    states[i].jet_idx == jet_idx &&
                    profiles_equal(states[i].profile, profile)) {
                    // Found cycle!
                    long cycle_start = states[i].rock_num;
                    long cycle_len = rock_num - cycle_start;
                    long cycle_height = height - states[i].height;

                    long remaining = num_rocks - rock_num - 1;
                    long full_cycles = remaining / cycle_len;
                    long leftover = remaining % cycle_len;

                    long final_height = height + full_cycles * cycle_height;
                    if (leftover > 0) {
                        final_height += heights[cycle_start + leftover] - heights[cycle_start];
                    }

                    free(heights);
                    return final_height;
                }
            }

            // Store this state
            if (state_count < MAX_STATES) {
                states[state_count].rock_type = rock_type;
                states[state_count].jet_idx = jet_idx;
                memcpy(states[state_count].profile, profile, sizeof(profile));
                states[state_count].rock_num = rock_num;
                states[state_count].height = height;
                state_count++;
            }
        }
    }

    if (heights) free(heights);
    return height;
}

int main(void) {
    // Read input
    char path[256];
    const char *dir = __FILE__;
    int len = strlen(dir);
    while (len > 0 && dir[len-1] != '/') len--;
    strncpy(path, dir, len);
    strcpy(path + len, "../input.txt");

    FILE *f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "Cannot open %s\n", path);
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    jets = malloc(size + 1);
    fread(jets, 1, size, f);
    jets[size] = '\0';
    fclose(f);

    // Strip newline
    jet_len = strlen(jets);
    while (jet_len > 0 && (jets[jet_len-1] == '\n' || jets[jet_len-1] == '\r')) {
        jets[--jet_len] = '\0';
    }

    // Part 1: 2022 rocks
    long part1 = simulate(2022);
    printf("Part 1: %ld\n", part1);

    // Part 2: 1 trillion rocks
    long part2 = simulate(1000000000000L);
    printf("Part 2: %ld\n", part2);

    free(jets);
    return 0;
}
