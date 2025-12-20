#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 150
#define MAX_TRACK 10000

typedef struct {
    int r, c;
} Point;

char grid[MAX_SIZE][MAX_SIZE + 2];
int rows = 0, cols = 0;
Point start, end;

Point track[MAX_TRACK];
int dist[MAX_SIZE][MAX_SIZE];
int track_count = 0;

void parse_grid(void) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Cannot open input.txt");
        exit(1);
    }

    while (fgets(grid[rows], sizeof(grid[rows]), f)) {
        int len = strlen(grid[rows]);
        while (len > 0 && (grid[rows][len-1] == '\n' || grid[rows][len-1] == '\r')) {
            grid[rows][--len] = '\0';
        }
        if (len == 0) break;
        if (cols == 0) cols = len;

        for (int c = 0; c < cols; c++) {
            if (grid[rows][c] == 'S') {
                start.r = rows;
                start.c = c;
            } else if (grid[rows][c] == 'E') {
                end.r = rows;
                end.c = c;
            }
        }
        rows++;
    }
    fclose(f);
}

void trace_path(void) {
    memset(dist, -1, sizeof(dist));

    Point queue[MAX_TRACK];
    int head = 0, tail = 0;

    queue[tail++] = start;
    dist[start.r][start.c] = 0;

    int dr[] = {-1, 1, 0, 0};
    int dc[] = {0, 0, -1, 1};

    while (head < tail) {
        Point cur = queue[head++];
        track[track_count++] = cur;

        if (cur.r == end.r && cur.c == end.c) break;

        for (int i = 0; i < 4; i++) {
            int nr = cur.r + dr[i];
            int nc = cur.c + dc[i];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols &&
                grid[nr][nc] != '#' && dist[nr][nc] == -1) {
                dist[nr][nc] = dist[cur.r][cur.c] + 1;
                queue[tail++] = (Point){nr, nc};
            }
        }
    }
}

long count_cheats(int max_cheat_time, int min_savings) {
    long count = 0;

    for (int i = 0; i < track_count; i++) {
        int r1 = track[i].r;
        int c1 = track[i].c;
        int d1 = dist[r1][c1];

        for (int j = 0; j < track_count; j++) {
            int r2 = track[j].r;
            int c2 = track[j].c;

            int cheat_cost = abs(r2 - r1) + abs(c2 - c1);
            if (cheat_cost <= max_cheat_time) {
                int d2 = dist[r2][c2];
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
    parse_grid();
    trace_path();

    printf("Part 1: %ld\n", count_cheats(2, 100));
    printf("Part 2: %ld\n", count_cheats(20, 100));

    return 0;
}
