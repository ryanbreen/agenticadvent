#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_MOVES 2048
#define GRID_SIZE 1000
#define OFFSET 500

typedef struct {
    char direction;
    int count;
} Move;

typedef struct {
    int x, y;
} Point;

static int sign(int x) {
    if (x == 0) return 0;
    return x > 0 ? 1 : -1;
}

static void move_tail(Point *head, Point *tail) {
    int dx = head->x - tail->x;
    int dy = head->y - tail->y;

    // If adjacent or overlapping, don't move
    if (abs(dx) <= 1 && abs(dy) <= 1) {
        return;
    }

    // Move toward head
    tail->x += sign(dx);
    tail->y += sign(dy);
}

static int simulate_rope(Move *moves, int num_moves, int rope_length) {
    // Use a 2D grid to track visited positions
    // Grid is centered at OFFSET,OFFSET
    char *visited = calloc(GRID_SIZE * GRID_SIZE, sizeof(char));
    if (!visited) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }

    Point *knots = malloc(rope_length * sizeof(Point));
    if (!knots) {
        fprintf(stderr, "Memory allocation failed\n");
        free(visited);
        exit(1);
    }

    // Initialize all knots at origin
    for (int i = 0; i < rope_length; i++) {
        knots[i].x = 0;
        knots[i].y = 0;
    }

    // Mark starting position
    visited[(0 + OFFSET) * GRID_SIZE + (0 + OFFSET)] = 1;
    int count = 1;

    // Direction deltas
    int dx, dy;

    for (int m = 0; m < num_moves; m++) {
        switch (moves[m].direction) {
            case 'U': dx = 0; dy = 1; break;
            case 'D': dx = 0; dy = -1; break;
            case 'L': dx = -1; dy = 0; break;
            case 'R': dx = 1; dy = 0; break;
            default: continue;
        }

        for (int step = 0; step < moves[m].count; step++) {
            // Move head
            knots[0].x += dx;
            knots[0].y += dy;

            // Move each subsequent knot
            for (int i = 1; i < rope_length; i++) {
                move_tail(&knots[i-1], &knots[i]);
            }

            // Mark tail position as visited
            int tail_idx = (knots[rope_length-1].y + OFFSET) * GRID_SIZE +
                           (knots[rope_length-1].x + OFFSET);
            if (!visited[tail_idx]) {
                visited[tail_idx] = 1;
                count++;
            }
        }
    }

    free(visited);
    free(knots);
    return count;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        fprintf(stderr, "Could not open input file\n");
        return 1;
    }

    Move moves[MAX_MOVES];
    int num_moves = 0;

    char line[32];
    while (fgets(line, sizeof(line), fp) && num_moves < MAX_MOVES) {
        if (sscanf(line, "%c %d", &moves[num_moves].direction, &moves[num_moves].count) == 2) {
            num_moves++;
        }
    }
    fclose(fp);

    int part1 = simulate_rope(moves, num_moves, 2);
    int part2 = simulate_rope(moves, num_moves, 10);

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    return 0;
}
