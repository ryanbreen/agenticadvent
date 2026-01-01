/*
 * Day 18: Lavaduct Lagoon
 * Calculate polygon area using Shoelace formula and Pick's theorem.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INSTRUCTIONS 1000

typedef struct {
    char direction;
    int distance;
    char color[7];  /* 6 hex digits + null terminator */
} Instruction;

static long long calculate_area(long long *vertices_r, long long *vertices_c,
                                int n, long long perimeter) {
    /*
     * Shoelace formula for polygon area.
     * Pick's theorem: A = i + b/2 - 1
     * Total = i + b = A + b/2 + 1
     */
    long long area = 0;
    for (int i = 0; i < n; i++) {
        int j = (i + 1) % n;
        area += vertices_r[i] * vertices_c[j];
        area -= vertices_r[j] * vertices_c[i];
    }
    if (area < 0) area = -area;
    area /= 2;

    return area + perimeter / 2 + 1;
}

static long long part1(Instruction *instructions, int count) {
    long long vertices_r[MAX_INSTRUCTIONS + 1];
    long long vertices_c[MAX_INSTRUCTIONS + 1];

    int n = 0;
    long long r = 0, c = 0;
    long long perimeter = 0;

    vertices_r[n] = r;
    vertices_c[n] = c;
    n++;

    for (int i = 0; i < count; i++) {
        int dr = 0, dc = 0;
        switch (instructions[i].direction) {
            case 'R': dr = 0; dc = 1; break;
            case 'D': dr = 1; dc = 0; break;
            case 'L': dr = 0; dc = -1; break;
            case 'U': dr = -1; dc = 0; break;
        }

        r += (long long)dr * instructions[i].distance;
        c += (long long)dc * instructions[i].distance;
        vertices_r[n] = r;
        vertices_c[n] = c;
        n++;
        perimeter += instructions[i].distance;
    }

    return calculate_area(vertices_r, vertices_c, n, perimeter);
}

static long long part2(Instruction *instructions, int count) {
    long long vertices_r[MAX_INSTRUCTIONS + 1];
    long long vertices_c[MAX_INSTRUCTIONS + 1];

    int n = 0;
    long long r = 0, c = 0;
    long long perimeter = 0;

    vertices_r[n] = r;
    vertices_c[n] = c;
    n++;

    /* Direction mapping: 0=R, 1=D, 2=L, 3=U */
    int dir_r[] = {0, 1, 0, -1};
    int dir_c[] = {1, 0, -1, 0};

    for (int i = 0; i < count; i++) {
        /* First 5 hex digits = distance, last digit = direction */
        char hex_dist[6];
        strncpy(hex_dist, instructions[i].color, 5);
        hex_dist[5] = '\0';

        long long distance = strtol(hex_dist, NULL, 16);
        int direction = instructions[i].color[5] - '0';

        int dr = dir_r[direction];
        int dc = dir_c[direction];

        r += dr * distance;
        c += dc * distance;
        vertices_r[n] = r;
        vertices_c[n] = c;
        n++;
        perimeter += distance;
    }

    return calculate_area(vertices_r, vertices_c, n, perimeter);
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    Instruction instructions[MAX_INSTRUCTIONS];
    int count = 0;

    char line[64];
    while (fgets(line, sizeof(line), fp) && count < MAX_INSTRUCTIONS) {
        char dir;
        int dist;
        char color_part[16];

        if (sscanf(line, "%c %d (#%[^)])", &dir, &dist, color_part) == 3) {
            instructions[count].direction = dir;
            instructions[count].distance = dist;
            strncpy(instructions[count].color, color_part, 6);
            instructions[count].color[6] = '\0';
            count++;
        }
    }
    fclose(fp);

    printf("Part 1: %lld\n", part1(instructions, count));
    printf("Part 2: %lld\n", part2(instructions, count));

    return 0;
}
