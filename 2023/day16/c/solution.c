#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ROWS 256
#define MAX_COLS 256
#define QUEUE_SIZE (MAX_ROWS * MAX_COLS * 4)

// Directions: 0=right, 1=down, 2=left, 3=up
static const int dr[] = {0, 1, 0, -1};
static const int dc[] = {1, 0, -1, 0};

typedef struct {
    int r, c, d;
} State;

static char grid[MAX_ROWS][MAX_COLS + 1];
static int rows = 0;
static int cols = 0;

// BFS queue
static State queue[QUEUE_SIZE];
static int queue_head, queue_tail;

// Visited array: visited[r][c][d]
static bool visited[MAX_ROWS][MAX_COLS][4];

static void enqueue(int r, int c, int d) {
    queue[queue_tail].r = r;
    queue[queue_tail].c = c;
    queue[queue_tail].d = d;
    queue_tail++;
}

static State dequeue(void) {
    return queue[queue_head++];
}

static bool queue_empty(void) {
    return queue_head >= queue_tail;
}

static int count_energized(int start_row, int start_col, int start_dir) {
    // Reset visited and queue
    memset(visited, 0, sizeof(visited));
    queue_head = 0;
    queue_tail = 0;

    enqueue(start_row, start_col, start_dir);

    while (!queue_empty()) {
        State s = dequeue();
        int r = s.r, c = s.c, d = s.d;

        // Bounds check
        if (r < 0 || r >= rows || c < 0 || c >= cols) {
            continue;
        }

        // Already visited?
        if (visited[r][c][d]) {
            continue;
        }
        visited[r][c][d] = true;

        char cell = grid[r][c];

        if (cell == '.') {
            // Continue same direction
            enqueue(r + dr[d], c + dc[d], d);
        } else if (cell == '/') {
            // Reflect: 0->3, 1->2, 2->1, 3->0
            int nd = 3 - d;
            if (d == 1) nd = 2;
            else if (d == 2) nd = 1;
            // Actually simpler: new_dir = [3, 2, 1, 0][d]
            nd = (d == 0) ? 3 : (d == 1) ? 2 : (d == 2) ? 1 : 0;
            enqueue(r + dr[nd], c + dc[nd], nd);
        } else if (cell == '\\') {
            // Reflect: 0->1, 1->0, 2->3, 3->2
            int nd = (d == 0) ? 1 : (d == 1) ? 0 : (d == 2) ? 3 : 2;
            enqueue(r + dr[nd], c + dc[nd], nd);
        } else if (cell == '|') {
            if (d == 0 || d == 2) {
                // Split to up and down
                enqueue(r + dr[1], c + dc[1], 1);  // down
                enqueue(r + dr[3], c + dc[3], 3);  // up
            } else {
                // Pass through
                enqueue(r + dr[d], c + dc[d], d);
            }
        } else if (cell == '-') {
            if (d == 1 || d == 3) {
                // Split to left and right
                enqueue(r + dr[0], c + dc[0], 0);  // right
                enqueue(r + dr[2], c + dc[2], 2);  // left
            } else {
                // Pass through
                enqueue(r + dr[d], c + dc[d], d);
            }
        }
    }

    // Count unique (r, c) positions that were visited
    int count = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (visited[r][c][0] || visited[r][c][1] || visited[r][c][2] || visited[r][c][3]) {
                count++;
            }
        }
    }

    return count;
}

static int part1(void) {
    return count_energized(0, 0, 0);  // Start at (0,0) heading right
}

static int part2(void) {
    int max_energized = 0;
    int energized;

    // Top row, heading down
    for (int c = 0; c < cols; c++) {
        energized = count_energized(0, c, 1);
        if (energized > max_energized) max_energized = energized;
    }

    // Bottom row, heading up
    for (int c = 0; c < cols; c++) {
        energized = count_energized(rows - 1, c, 3);
        if (energized > max_energized) max_energized = energized;
    }

    // Left column, heading right
    for (int r = 0; r < rows; r++) {
        energized = count_energized(r, 0, 0);
        if (energized > max_energized) max_energized = energized;
    }

    // Right column, heading left
    for (int r = 0; r < rows; r++) {
        energized = count_energized(r, cols - 1, 2);
        if (energized > max_energized) max_energized = energized;
    }

    return max_energized;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Could not open input.txt");
        return 1;
    }

    // Read the grid
    while (fgets(grid[rows], sizeof(grid[rows]), fp)) {
        // Remove trailing newline
        int len = strlen(grid[rows]);
        if (len > 0 && grid[rows][len - 1] == '\n') {
            grid[rows][len - 1] = '\0';
            len--;
        }
        if (len > 0) {
            if (rows == 0) cols = len;
            rows++;
        }
    }
    fclose(fp);

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
