/**
 * Day 20: Race Condition
 *
 * Algorithm:
 * 1. Parse the grid and find Start (S) and End (E) positions
 * 2. Trace the single path from S to E using BFS, recording distance at each cell
 * 3. For each pair of track positions, check if "cheating" through walls saves time
 *    - A cheat allows passing through walls for up to max_cheat_time steps
 *    - Savings = dist[end] - dist[start] - manhattan_distance
 *    - Part 1: max cheat time = 2
 *    - Part 2: max cheat time = 20
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Grid dimensions for AoC 2024 Day 20 input (141x141 grid) */
#define MAX_GRID_SIZE 150

/* Maximum track cells (path length in typical input is ~9400) */
#define MAX_TRACK_CELLS 10000

/* Minimum time savings required to count a cheat as useful */
#define MIN_SAVINGS_THRESHOLD 100

typedef struct {
    int row, col;
} Point;

/**
 * Encapsulates all puzzle state to avoid global variables.
 * This makes the code more testable and the data flow explicit.
 */
typedef struct {
    char grid[MAX_GRID_SIZE][MAX_GRID_SIZE + 2];
    int rows;
    int cols;
    Point start;
    Point end;
    Point track[MAX_TRACK_CELLS];
    int dist[MAX_GRID_SIZE][MAX_GRID_SIZE];
    int track_count;
} RaceState;

/**
 * Parses the input file into the race state.
 * Identifies grid dimensions, start position (S), and end position (E).
 */
static void parse_grid(RaceState *state) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Cannot open input.txt");
        exit(1);
    }

    state->rows = 0;
    state->cols = 0;

    while (fgets(state->grid[state->rows], sizeof(state->grid[state->rows]), f)) {
        int len = strlen(state->grid[state->rows]);
        while (len > 0 && (state->grid[state->rows][len - 1] == '\n' ||
                           state->grid[state->rows][len - 1] == '\r')) {
            state->grid[state->rows][--len] = '\0';
        }
        if (len == 0) break;
        if (state->cols == 0) state->cols = len;

        for (int c = 0; c < state->cols; c++) {
            char ch = state->grid[state->rows][c];
            if (ch == 'S') {
                state->start.row = state->rows;
                state->start.col = c;
            } else if (ch == 'E') {
                state->end.row = state->rows;
                state->end.col = c;
            }
        }
        state->rows++;
    }
    fclose(f);
}

/**
 * Traces the single path from start to end using BFS.
 * Records the distance from start for each cell on the track.
 * Also builds the ordered list of track positions.
 */
static void trace_path(RaceState *state) {
    memset(state->dist, -1, sizeof(state->dist));
    state->track_count = 0;

    Point queue[MAX_TRACK_CELLS];
    int head = 0, tail = 0;

    queue[tail++] = state->start;
    state->dist[state->start.row][state->start.col] = 0;

    /* Direction vectors: up, down, left, right */
    static const int dr[] = {-1, 1, 0, 0};
    static const int dc[] = {0, 0, -1, 1};

    while (head < tail) {
        Point cur = queue[head++];
        state->track[state->track_count++] = cur;

        if (cur.row == state->end.row && cur.col == state->end.col) break;

        for (int i = 0; i < 4; i++) {
            int nr = cur.row + dr[i];
            int nc = cur.col + dc[i];

            if (nr >= 0 && nr < state->rows && nc >= 0 && nc < state->cols &&
                state->grid[nr][nc] != '#' && state->dist[nr][nc] == -1) {
                state->dist[nr][nc] = state->dist[cur.row][cur.col] + 1;
                queue[tail++] = (Point){nr, nc};
            }
        }
    }
}

/**
 * Counts how many cheats save at least min_savings picoseconds.
 *
 * A cheat consists of:
 * - Starting at any track position
 * - Moving through walls for up to max_cheat_time steps (Manhattan distance)
 * - Ending at any other track position
 *
 * The savings is calculated as:
 *   savings = dist[destination] - dist[origin] - cheat_cost
 *
 * If savings >= min_savings, the cheat is counted as useful.
 */
static long count_cheats(const RaceState *state, int max_cheat_time, int min_savings) {
    long count = 0;

    for (int i = 0; i < state->track_count; i++) {
        int r1 = state->track[i].row;
        int c1 = state->track[i].col;
        int d1 = state->dist[r1][c1];

        for (int j = 0; j < state->track_count; j++) {
            int r2 = state->track[j].row;
            int c2 = state->track[j].col;

            int cheat_cost = abs(r2 - r1) + abs(c2 - c1);
            if (cheat_cost <= max_cheat_time) {
                int d2 = state->dist[r2][c2];
                int savings = d2 - d1 - cheat_cost;
                if (savings >= min_savings) {
                    count++;
                }
            }
        }
    }

    return count;
}

int main(void) {
    RaceState state = {0};

    parse_grid(&state);
    trace_path(&state);

    printf("Part 1: %ld\n", count_cheats(&state, 2, MIN_SAVINGS_THRESHOLD));
    printf("Part 2: %ld\n", count_cheats(&state, 20, MIN_SAVINGS_THRESHOLD));

    return 0;
}
