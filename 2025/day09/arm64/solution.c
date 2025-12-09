#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_POINTS 2048
#define MAX_EDGES 2048

typedef struct {
    int x, y;
} Point;

typedef struct {
    int y, x_min, x_max;
} HEdge;

typedef struct {
    int x, y_min, y_max;
} VEdge;

typedef struct {
    VEdge *edges;
    int count;
    int capacity;
} VEdgeList;

typedef struct {
    HEdge *edges;
    int count;
    int capacity;
} HEdgeList;

// Hash table for vertical edges indexed by x
#define HASH_SIZE 4096
typedef struct {
    VEdgeList buckets[HASH_SIZE];
} VEdgeHash;

// Hash table for horizontal edges indexed by y
typedef struct {
    HEdgeList buckets[HASH_SIZE];
} HEdgeHash;

Point points[MAX_POINTS];
int point_count = 0;

VEdge vertical_edges[MAX_EDGES];
int vert_count = 0;

HEdge horizontal_edges[MAX_EDGES];
int horiz_count = 0;

static inline int min(int a, int b) { return a < b ? a : b; }
static inline int max(int a, int b) { return a > b ? a : b; }
static inline int abs_val(int x) { return x < 0 ? -x : x; }

void read_input(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Failed to open input file");
        exit(1);
    }

    point_count = 0;
    while (fscanf(fp, "%d,%d\n", &points[point_count].x, &points[point_count].y) == 2) {
        point_count++;
        if (point_count >= MAX_POINTS) break;
    }
    fclose(fp);
}

long long part1() {
    long long max_area = 0;

    for (int i = 0; i < point_count; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;

        for (int j = i + 1; j < point_count; j++) {
            int x2 = points[j].x;
            int y2 = points[j].y;

            long long width = abs_val(x2 - x1) + 1;
            long long height = abs_val(y2 - y1) + 1;
            long long area = width * height;

            if (area > max_area) {
                max_area = area;
            }
        }
    }

    return max_area;
}

void build_edges() {
    vert_count = 0;
    horiz_count = 0;

    for (int i = 0; i < point_count; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;
        int x2 = points[(i + 1) % point_count].x;
        int y2 = points[(i + 1) % point_count].y;

        if (y1 == y2) {  // Horizontal edge
            horizontal_edges[horiz_count].y = y1;
            horizontal_edges[horiz_count].x_min = min(x1, x2);
            horizontal_edges[horiz_count].x_max = max(x1, x2);
            horiz_count++;
        } else {  // Vertical edge
            vertical_edges[vert_count].x = x1;
            vertical_edges[vert_count].y_min = min(y1, y2);
            vertical_edges[vert_count].y_max = max(y1, y2);
            vert_count++;
        }
    }
}

void init_vedge_hash(VEdgeHash *hash) {
    for (int i = 0; i < HASH_SIZE; i++) {
        hash->buckets[i].edges = NULL;
        hash->buckets[i].count = 0;
        hash->buckets[i].capacity = 0;
    }
}

void init_hedge_hash(HEdgeHash *hash) {
    for (int i = 0; i < HASH_SIZE; i++) {
        hash->buckets[i].edges = NULL;
        hash->buckets[i].count = 0;
        hash->buckets[i].capacity = 0;
    }
}

void add_vedge(VEdgeHash *hash, int x, int y_min, int y_max) {
    unsigned int bucket = (unsigned int)x % HASH_SIZE;
    VEdgeList *list = &hash->buckets[bucket];

    if (list->count >= list->capacity) {
        list->capacity = list->capacity == 0 ? 8 : list->capacity * 2;
        list->edges = realloc(list->edges, list->capacity * sizeof(VEdge));
    }

    list->edges[list->count].x = x;
    list->edges[list->count].y_min = y_min;
    list->edges[list->count].y_max = y_max;
    list->count++;
}

void add_hedge(HEdgeHash *hash, int y, int x_min, int x_max) {
    unsigned int bucket = (unsigned int)y % HASH_SIZE;
    HEdgeList *list = &hash->buckets[bucket];

    if (list->count >= list->capacity) {
        list->capacity = list->capacity == 0 ? 8 : list->capacity * 2;
        list->edges = realloc(list->edges, list->capacity * sizeof(HEdge));
    }

    list->edges[list->count].y = y;
    list->edges[list->count].x_min = x_min;
    list->edges[list->count].x_max = x_max;
    list->count++;
}

void free_vedge_hash(VEdgeHash *hash) {
    for (int i = 0; i < HASH_SIZE; i++) {
        free(hash->buckets[i].edges);
    }
}

void free_hedge_hash(HEdgeHash *hash) {
    for (int i = 0; i < HASH_SIZE; i++) {
        free(hash->buckets[i].edges);
    }
}

int is_inside_polygon(VEdgeHash *vert_hash, double x, double y) {
    int crossings_times_2 = 0;  // Track crossings * 2 to handle 0.5 increments

    // Ray casting to the right
    for (int i = 0; i < vert_count; i++) {
        int vx = vertical_edges[i].x;
        if (vx <= x) continue;

        int y_min = vertical_edges[i].y_min;
        int y_max = vertical_edges[i].y_max;

        if (y_min < y && y < y_max) {
            crossings_times_2 += 2;  // Full crossing
        } else if (y == y_min || y == y_max) {
            crossings_times_2 += 1;  // Half crossing (0.5 * 2 = 1)
        }
    }

    // Python checks: crossings % 2 == 1
    // With floats: 1.0 % 2 == 1.0 (True), 3.0 % 2 == 1.0 (True)
    // We have crossings_times_2, so divide by 2 and check if odd
    return (crossings_times_2 / 2) % 2 == 1;
}

int rectangle_valid(VEdgeHash *vert_hash, HEdgeHash *horiz_hash,
                    int x1, int y1, int x2, int y2) {
    int min_x = min(x1, x2);
    int max_x = max(x1, x2);
    int min_y = min(y1, y2);
    int max_y = max(y1, y2);

    // Check vertical edges
    for (int i = 0; i < vert_count; i++) {
        int vx = vertical_edges[i].x;
        if (min_x < vx && vx < max_x) {
            int y_min = vertical_edges[i].y_min;
            int y_max = vertical_edges[i].y_max;

            // Check if this edge overlaps with rectangle's y range
            if (!(y_max <= min_y || y_min >= max_y)) {
                return 0;
            }
        }
    }

    // Check horizontal edges
    for (int i = 0; i < horiz_count; i++) {
        int hy = horizontal_edges[i].y;
        if (min_y < hy && hy < max_y) {
            int x_min = horizontal_edges[i].x_min;
            int x_max = horizontal_edges[i].x_max;

            // Check if this edge overlaps with rectangle's x range
            if (!(x_max <= min_x || x_min >= max_x)) {
                return 0;
            }
        }
    }

    // Check center point is inside polygon
    double center_x = (min_x + max_x) / 2.0;
    double center_y = (min_y + max_y) / 2.0;

    return is_inside_polygon(vert_hash, center_x, center_y);
}

long long part2() {
    build_edges();

    VEdgeHash vert_hash;
    HEdgeHash horiz_hash;
    init_vedge_hash(&vert_hash);
    init_hedge_hash(&horiz_hash);

    // Build hash tables
    for (int i = 0; i < vert_count; i++) {
        add_vedge(&vert_hash, vertical_edges[i].x,
                  vertical_edges[i].y_min, vertical_edges[i].y_max);
    }

    for (int i = 0; i < horiz_count; i++) {
        add_hedge(&horiz_hash, horizontal_edges[i].y,
                  horizontal_edges[i].x_min, horizontal_edges[i].x_max);
    }

    long long max_area = 0;

    for (int i = 0; i < point_count; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;

        for (int j = i + 1; j < point_count; j++) {
            int x2 = points[j].x;
            int y2 = points[j].y;

            if (rectangle_valid(&vert_hash, &horiz_hash, x1, y1, x2, y2)) {
                long long width = abs_val(x2 - x1) + 1;
                long long height = abs_val(y2 - y1) + 1;
                long long area = width * height;

                if (area > max_area) {
                    max_area = area;
                }
            }
        }
    }

    free_vedge_hash(&vert_hash);
    free_hedge_hash(&horiz_hash);

    return max_area;
}

int main() {
    read_input("../input.txt");

    printf("Part 1: %lld\n", part1());
    printf("Part 2: %lld\n", part2());

    return 0;
}
