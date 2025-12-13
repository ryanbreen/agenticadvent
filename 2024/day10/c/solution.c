#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ROWS 100
#define MAX_COLS 100
#define MAX_QUEUE 10000

// Directions: up, down, left, right
static const int DIRS[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

// Point structure
typedef struct {
    int r;
    int c;
} Point;

// Grid structure to encapsulate grid data
typedef struct {
    int data[MAX_ROWS][MAX_COLS];
    int rows;
    int cols;
} Grid;

// Queue structure for BFS
typedef struct {
    Point data[MAX_QUEUE];
    int start;
    int end;
} Queue;

void queue_init(Queue *q) {
    q->start = 0;
    q->end = 0;
}

void enqueue(Queue *q, int r, int c) {
    if (q->end >= MAX_QUEUE) {
        fprintf(stderr, "Queue overflow\n");
        exit(1);
    }
    q->data[q->end].r = r;
    q->data[q->end].c = c;
    q->end++;
}

Point dequeue(Queue *q) {
    return q->data[q->start++];
}

bool queue_is_empty(const Queue *q) {
    return q->start >= q->end;
}

// Set structure for tracking visited positions and found nines
typedef struct {
    bool visited[MAX_ROWS][MAX_COLS];
} VisitedSet;

void reset_visited(VisitedSet *set) {
    memset(set->visited, 0, sizeof(set->visited));
}

void add_visited(VisitedSet *set, int r, int c) {
    set->visited[r][c] = true;
}

bool is_visited(VisitedSet *set, int r, int c) {
    return set->visited[r][c];
}

// Read input file
void read_input(const char *filename, Grid *grid) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        exit(1);
    }

    char line[MAX_COLS + 2];
    grid->rows = 0;
    while (fgets(line, sizeof(line), file)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;

        if (strlen(line) == 0) continue;

        grid->cols = strlen(line);
        for (int c = 0; c < grid->cols; c++) {
            grid->data[grid->rows][c] = line[c] - '0';
        }
        grid->rows++;
    }

    fclose(file);
}

// Find all trailheads (positions with height 0)
int find_trailheads(const Grid *grid, Point trailheads[]) {
    int count = 0;
    for (int r = 0; r < grid->rows; r++) {
        for (int c = 0; c < grid->cols; c++) {
            if (grid->data[r][c] == 0) {
                trailheads[count].r = r;
                trailheads[count].c = c;
                count++;
            }
        }
    }
    return count;
}

// Part 1: BFS to count unique 9s reachable from a trailhead
int count_reachable_nines(const Grid *grid, int start_r, int start_c) {
    VisitedSet visited;
    VisitedSet nines;
    Queue queue;

    reset_visited(&visited);
    reset_visited(&nines);
    queue_init(&queue);

    add_visited(&visited, start_r, start_c);
    enqueue(&queue, start_r, start_c);

    int nines_count = 0;

    while (!queue_is_empty(&queue)) {
        Point p = dequeue(&queue);
        int r = p.r;
        int c = p.c;
        int current_height = grid->data[r][c];

        if (current_height == 9) {
            if (!is_visited(&nines, r, c)) {
                add_visited(&nines, r, c);
                nines_count++;
            }
            continue;
        }

        // Try all four directions
        for (int i = 0; i < 4; i++) {
            int nr = r + DIRS[i][0];
            int nc = c + DIRS[i][1];

            if (nr >= 0 && nr < grid->rows && nc >= 0 && nc < grid->cols) {
                if (!is_visited(&visited, nr, nc)) {
                    if (grid->data[nr][nc] == current_height + 1) {
                        add_visited(&visited, nr, nc);
                        enqueue(&queue, nr, nc);
                    }
                }
            }
        }
    }

    return nines_count;
}

// Part 2: DFS to count distinct trails
int dfs(const Grid *grid, int r, int c) {
    int current_height = grid->data[r][c];

    if (current_height == 9) {
        return 1;
    }

    int total = 0;
    for (int i = 0; i < 4; i++) {
        int nr = r + DIRS[i][0];
        int nc = c + DIRS[i][1];

        if (nr >= 0 && nr < grid->rows && nc >= 0 && nc < grid->cols) {
            if (grid->data[nr][nc] == current_height + 1) {
                total += dfs(grid, nr, nc);
            }
        }
    }
    return total;
}

int count_distinct_trails(const Grid *grid, int start_r, int start_c) {
    return dfs(grid, start_r, start_c);
}

int part1(const Grid *grid) {
    Point trailheads[MAX_ROWS * MAX_COLS];
    int num_trailheads = find_trailheads(grid, trailheads);

    int total_score = 0;
    for (int i = 0; i < num_trailheads; i++) {
        total_score += count_reachable_nines(grid, trailheads[i].r, trailheads[i].c);
    }
    return total_score;
}

int part2(const Grid *grid) {
    Point trailheads[MAX_ROWS * MAX_COLS];
    int num_trailheads = find_trailheads(grid, trailheads);

    int total_rating = 0;
    for (int i = 0; i < num_trailheads; i++) {
        total_rating += count_distinct_trails(grid, trailheads[i].r, trailheads[i].c);
    }
    return total_rating;
}

int main(void) {
    Grid grid;
    read_input("../input.txt", &grid);

    printf("Part 1: %d\n", part1(&grid));
    printf("Part 2: %d\n", part2(&grid));

    return 0;
}
