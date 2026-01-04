/*
 * Day 23: A Long Walk
 *
 * Find the longest path through a hiking trail maze.
 * Part 1: Respect slope directions (^,v,<,>)
 * Part 2: Ignore slopes (treat as regular paths)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_GRID_SIZE 256
#define MAX_JUNCTIONS 64
#define MAX_LINE_LEN 256
#define MAX_EDGES_PER_NODE 10

// Grid storage
static char grid[MAX_GRID_SIZE][MAX_GRID_SIZE];
static int rows, cols;

// Junction handling
static int junctions[MAX_JUNCTIONS][2];  // (row, col)
static int num_junctions;
static int junction_index[MAX_GRID_SIZE][MAX_GRID_SIZE];  // -1 if not junction

// Edge structure for adjacency list
typedef struct {
    uint8_t to;      // Target junction index
    uint16_t weight;  // Edge weight (distance)
} Edge;

// Adjacency list representation
static Edge adj_list[MAX_JUNCTIONS][MAX_EDGES_PER_NODE];
static uint8_t adj_count[MAX_JUNCTIONS];  // Number of edges for each node

// Direction vectors
static const int dr[] = {-1, 1, 0, 0};
static const int dc[] = {0, 0, -1, 1};

// Slope characters and their required directions
static inline int get_slope_dir(char c) {
    switch (c) {
        case '^': return 0;  // up
        case 'v': return 1;  // down
        case '<': return 2;  // left
        case '>': return 3;  // right
        default: return -1;
    }
}

void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Error opening input file");
        exit(1);
    }

    rows = 0;
    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), f)) {
        int len = (int)strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        if (rows == 0) {
            cols = len;
        }
        strncpy(grid[rows], line, cols);
        grid[rows][cols] = '\0';
        rows++;
    }
    fclose(f);
}

void find_junctions(void) {
    num_junctions = 0;
    memset(junction_index, -1, sizeof(junction_index));

    // Find start (first . in row 0)
    for (int c = 0; c < cols; c++) {
        if (grid[0][c] == '.') {
            junctions[num_junctions][0] = 0;
            junctions[num_junctions][1] = c;
            junction_index[0][c] = num_junctions++;
            break;
        }
    }

    // Find end (first . in last row)
    for (int c = 0; c < cols; c++) {
        if (grid[rows-1][c] == '.') {
            junctions[num_junctions][0] = rows - 1;
            junctions[num_junctions][1] = c;
            junction_index[rows-1][c] = num_junctions++;
            break;
        }
    }

    // Find intersections (cells with 3+ walkable neighbors)
    for (int r = 1; r < rows - 1; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '#') continue;

            int neighbors = 0;
            for (int d = 0; d < 4; d++) {
                int nr = r + dr[d];
                int nc = c + dc[d];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
                    neighbors++;
                }
            }

            if (neighbors >= 3) {
                junctions[num_junctions][0] = r;
                junctions[num_junctions][1] = c;
                junction_index[r][c] = num_junctions++;
            }
        }
    }
}

void build_graph(bool respect_slopes) {
    // Clear adjacency list
    memset(adj_count, 0, sizeof(adj_count));

    // Stack for BFS - use compact structure
    typedef struct {
        uint8_t r, c;
        uint16_t dist;
    } StackEntry;

    StackEntry *stack = malloc(rows * cols * sizeof(StackEntry));
    if (!stack) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }

    uint8_t visited[MAX_GRID_SIZE][MAX_GRID_SIZE];

    for (int ji = 0; ji < num_junctions; ji++) {
        int start_r = junctions[ji][0];
        int start_c = junctions[ji][1];

        // BFS/DFS from this junction to find reachable junctions
        memset(visited, 0, sizeof(visited));
        int sp = 0;  // stack pointer

        stack[sp].r = start_r;
        stack[sp].c = start_c;
        stack[sp].dist = 0;
        sp++;
        visited[start_r][start_c] = 1;

        while (sp > 0) {
            sp--;
            int r = stack[sp].r;
            int c = stack[sp].c;
            int dist = stack[sp].dist;

            // If we've reached another junction (not the start), record edge
            if (dist > 0 && junction_index[r][c] >= 0) {
                int target_ji = junction_index[r][c];

                // Add edge to adjacency list
                int idx = adj_count[ji];
                adj_list[ji][idx].to = target_ji;
                adj_list[ji][idx].weight = dist;
                adj_count[ji]++;

                continue;  // Don't explore past junctions
            }

            // Explore neighbors
            for (int d = 0; d < 4; d++) {
                int nr = r + dr[d];
                int nc = c + dc[d];

                if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
                if (grid[nr][nc] == '#') continue;
                if (visited[nr][nc]) continue;

                // Check slope constraints for Part 1
                if (respect_slopes) {
                    char cell = grid[r][c];
                    int slope_dir = get_slope_dir(cell);
                    if (slope_dir >= 0 && slope_dir != d) {
                        continue;  // Must follow slope direction
                    }
                }

                visited[nr][nc] = 1;
                stack[sp].r = nr;
                stack[sp].c = nc;
                stack[sp].dist = dist + 1;
                sp++;
            }
        }
    }

    free(stack);
}

// DFS with backtracking using bitmask for visited junctions
static int dfs(int node, int end, uint64_t visited_mask) {
    if (node == end) {
        return 0;
    }

    visited_mask |= (1ULL << node);
    int max_dist = -1;  // -1 indicates no path found

    // Iterate through adjacency list instead of all junctions
    const Edge *edges = adj_list[node];
    int count = adj_count[node];

    for (int i = 0; i < count; i++) {
        int neighbor = edges[i].to;
        int weight = edges[i].weight;

        if (visited_mask & (1ULL << neighbor)) continue;

        int result = dfs(neighbor, end, visited_mask);
        if (result >= 0) {
            int total = weight + result;
            if (total > max_dist) {
                max_dist = total;
            }
        }
    }

    return max_dist;
}

int longest_path(void) {
    // Start is junction 0, end is junction 1 (based on how we add them)
    return dfs(0, 1, 0ULL);
}

int solve(bool respect_slopes) {
    find_junctions();
    build_graph(respect_slopes);
    return longest_path();
}

int main(void) {
    parse_input("../input.txt");

    int part1 = solve(true);   // Respect slopes
    int part2 = solve(false);  // Ignore slopes

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    return 0;
}
