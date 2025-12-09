#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#define MAX_POINTS 10000
#define MAX_EDGES 10000

typedef struct {
    int x, y;
} Point;

typedef struct {
    int y, x_min, x_max;
} HorizontalEdge;

typedef struct {
    int x, y_min, y_max;
} VerticalEdge;

Point points[MAX_POINTS];
int num_points = 0;

HorizontalEdge horizontal_edges[MAX_EDGES];
int num_horiz_edges = 0;

VerticalEdge vertical_edges[MAX_EDGES];
int num_vert_edges = 0;

void read_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("fopen");
        exit(1);
    }

    char line[256];
    while (fgets(line, sizeof(line), f)) {
        int x, y;
        if (sscanf(line, "%d,%d", &x, &y) == 2) {
            points[num_points].x = x;
            points[num_points].y = y;
            num_points++;
        }
    }

    fclose(f);
}

long long part1() {
    long long max_area = 0;

    // Check all pairs of points as opposite corners
    for (int i = 0; i < num_points; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;

        for (int j = i + 1; j < num_points; j++) {
            int x2 = points[j].x;
            int y2 = points[j].y;

            // Rectangle area = width * height (inclusive of both corners)
            long long width = abs(x2 - x1) + 1;
            long long height = abs(y2 - y1) + 1;
            long long area = width * height;

            if (area > max_area) {
                max_area = area;
            }
        }
    }

    return max_area;
}

void build_edges() {
    num_horiz_edges = 0;
    num_vert_edges = 0;

    for (int i = 0; i < num_points; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;
        int x2 = points[(i + 1) % num_points].x;
        int y2 = points[(i + 1) % num_points].y;

        if (y1 == y2) {
            // Horizontal edge
            horizontal_edges[num_horiz_edges].y = y1;
            horizontal_edges[num_horiz_edges].x_min = (x1 < x2) ? x1 : x2;
            horizontal_edges[num_horiz_edges].x_max = (x1 > x2) ? x1 : x2;
            num_horiz_edges++;
        } else {
            // Vertical edge
            vertical_edges[num_vert_edges].x = x1;
            vertical_edges[num_vert_edges].y_min = (y1 < y2) ? y1 : y2;
            vertical_edges[num_vert_edges].y_max = (y1 > y2) ? y1 : y2;
            num_vert_edges++;
        }
    }
}

int compare_vertical_edges(const void *a, const void *b) {
    VerticalEdge *ea = (VerticalEdge *)a;
    VerticalEdge *eb = (VerticalEdge *)b;
    return ea->x - eb->x;
}

bool is_inside_polygon(double x, double y) {
    double crossings = 0.0;

    // Cast ray to the right
    for (int i = 0; i < num_vert_edges; i++) {
        int vx = vertical_edges[i].x;
        int y_min = vertical_edges[i].y_min;
        int y_max = vertical_edges[i].y_max;

        if (vx <= x) {
            continue;
        }

        if (y_min < y && y < y_max) {
            // Strict inequality - ray crosses edge
            crossings += 1.0;
        } else if (y == y_min || y == y_max) {
            // On corner - count as 0.5 crossing
            crossings += 0.5;
        }
    }

    // Check if odd number of crossings (inside)
    // Python's "crossings % 2 == 1" only returns True for odd integers (1.0, 3.0, etc.)
    double remainder = fmod(crossings, 2.0);
    return fabs(remainder - 1.0) < 0.01;
}

bool rectangle_valid(int x1, int y1, int x2, int y2) {
    int min_x = (x1 < x2) ? x1 : x2;
    int max_x = (x1 > x2) ? x1 : x2;
    int min_y = (y1 < y2) ? y1 : y2;
    int max_y = (y1 > y2) ? y1 : y2;

    // Check if any vertical edge crosses through the rectangle interior
    for (int i = 0; i < num_vert_edges; i++) {
        int vx = vertical_edges[i].x;
        int vy_min = vertical_edges[i].y_min;
        int vy_max = vertical_edges[i].y_max;

        if (min_x < vx && vx < max_x) {
            // Vertical edge x is inside rectangle's x range
            // Check if this edge segment overlaps with rectangle's y range
            if (!(vy_max <= min_y || vy_min >= max_y)) {
                return false;
            }
        }
    }

    // Check if any horizontal edge crosses through the rectangle interior
    for (int i = 0; i < num_horiz_edges; i++) {
        int hy = horizontal_edges[i].y;
        int hx_min = horizontal_edges[i].x_min;
        int hx_max = horizontal_edges[i].x_max;

        if (min_y < hy && hy < max_y) {
            // Horizontal edge y is inside rectangle's y range
            // Check if this edge segment overlaps with rectangle's x range
            if (!(hx_max <= min_x || hx_min >= max_x)) {
                return false;
            }
        }
    }

    // Finally, check that we're inside the polygon (not outside)
    // Check center point
    double center_x = (min_x + max_x) / 2.0;
    double center_y = (min_y + max_y) / 2.0;
    return is_inside_polygon(center_x, center_y);
}

long long part2() {
    build_edges();
    qsort(vertical_edges, num_vert_edges, sizeof(VerticalEdge), compare_vertical_edges);

    long long max_area = 0;

    // Find largest valid rectangle with red corners
    for (int i = 0; i < num_points; i++) {
        int x1 = points[i].x;
        int y1 = points[i].y;

        for (int j = i + 1; j < num_points; j++) {
            int x2 = points[j].x;
            int y2 = points[j].y;

            if (rectangle_valid(x1, y1, x2, y2)) {
                long long width = abs(x2 - x1) + 1;
                long long height = abs(y2 - y1) + 1;
                long long area = width * height;

                if (area > max_area) {
                    max_area = area;
                }
            }
        }
    }

    return max_area;
}

int main(int argc, char *argv[]) {
    const char *input_file = (argc > 1) ? argv[1] : "../input.txt";
    read_input(input_file);

    printf("Part 1: %lld\n", part1());
    printf("Part 2: %lld\n", part2());

    return 0;
}
