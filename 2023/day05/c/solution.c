#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>

#define MAX_SEEDS 32
#define MAX_RANGES 64
#define MAX_MAPS 8
#define MAX_RESULT_RANGES 4096

typedef struct {
    int64_t dst_start;
    int64_t src_start;
    int64_t length;
} MapRange;

typedef struct {
    MapRange ranges[MAX_RANGES];
    int count;
} Map;

typedef struct {
    int64_t start;
    int64_t end;
} Range;

// Global data
int64_t seeds[MAX_SEEDS];
int seed_count = 0;
Map maps[MAX_MAPS];
int map_count = 0;

void parse_input(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[1024];
    int current_map = -1;

    while (fgets(line, sizeof(line), fp)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;

        if (strlen(line) == 0) {
            continue;
        }

        // Parse seeds line
        if (strncmp(line, "seeds:", 6) == 0) {
            char *ptr = line + 7;
            char *token;
            while ((token = strtok(ptr, " ")) != NULL) {
                seeds[seed_count++] = strtoll(token, NULL, 10);
                ptr = NULL;
            }
            continue;
        }

        // Check for map header
        if (strstr(line, "map:") != NULL) {
            current_map++;
            maps[current_map].count = 0;
            map_count = current_map + 1;
            continue;
        }

        // Parse map range
        if (current_map >= 0 && line[0] >= '0' && line[0] <= '9') {
            int64_t dst, src, len;
            if (sscanf(line, "%lld %lld %lld", &dst, &src, &len) == 3) {
                Map *m = &maps[current_map];
                m->ranges[m->count].dst_start = dst;
                m->ranges[m->count].src_start = src;
                m->ranges[m->count].length = len;
                m->count++;
            }
        }
    }

    fclose(fp);
}

int64_t apply_map(int64_t value, Map *map) {
    for (int i = 0; i < map->count; i++) {
        MapRange *r = &map->ranges[i];
        if (value >= r->src_start && value < r->src_start + r->length) {
            return r->dst_start + (value - r->src_start);
        }
    }
    return value;
}

int64_t seed_to_location(int64_t seed) {
    int64_t value = seed;
    for (int i = 0; i < map_count; i++) {
        value = apply_map(value, &maps[i]);
    }
    return value;
}

int64_t part1(void) {
    int64_t min_location = INT64_MAX;
    for (int i = 0; i < seed_count; i++) {
        int64_t loc = seed_to_location(seeds[i]);
        if (loc < min_location) {
            min_location = loc;
        }
    }
    return min_location;
}

int apply_map_to_ranges(Range *input, int input_count, Map *map, Range *output) {
    int output_count = 0;

    for (int i = 0; i < input_count; i++) {
        Range remaining[MAX_RESULT_RANGES];
        int remaining_count = 1;
        remaining[0] = input[i];

        for (int j = 0; j < map->count; j++) {
            MapRange *mr = &map->ranges[j];
            int64_t src_end = mr->src_start + mr->length;

            Range new_remaining[MAX_RESULT_RANGES];
            int new_remaining_count = 0;

            for (int k = 0; k < remaining_count; k++) {
                int64_t r_start = remaining[k].start;
                int64_t r_end = remaining[k].end;

                // Part before the map range (unmapped for now)
                if (r_start < mr->src_start) {
                    int64_t end = r_end < mr->src_start ? r_end : mr->src_start;
                    new_remaining[new_remaining_count].start = r_start;
                    new_remaining[new_remaining_count].end = end;
                    new_remaining_count++;
                }

                // Part within the map range (mapped)
                int64_t overlap_start = r_start > mr->src_start ? r_start : mr->src_start;
                int64_t overlap_end = r_end < src_end ? r_end : src_end;
                if (overlap_start < overlap_end) {
                    int64_t offset = mr->dst_start - mr->src_start;
                    output[output_count].start = overlap_start + offset;
                    output[output_count].end = overlap_end + offset;
                    output_count++;
                }

                // Part after the map range (unmapped for now)
                if (r_end > src_end) {
                    int64_t start = r_start > src_end ? r_start : src_end;
                    new_remaining[new_remaining_count].start = start;
                    new_remaining[new_remaining_count].end = r_end;
                    new_remaining_count++;
                }
            }

            remaining_count = new_remaining_count;
            for (int k = 0; k < new_remaining_count; k++) {
                remaining[k] = new_remaining[k];
            }
        }

        // Any remaining parts are unmapped (identity)
        for (int k = 0; k < remaining_count; k++) {
            output[output_count++] = remaining[k];
        }
    }

    return output_count;
}

int64_t part2(void) {
    // Convert seeds to ranges
    Range ranges[MAX_RESULT_RANGES];
    int range_count = 0;

    for (int i = 0; i < seed_count; i += 2) {
        ranges[range_count].start = seeds[i];
        ranges[range_count].end = seeds[i] + seeds[i + 1];
        range_count++;
    }

    // Apply each map to the ranges
    Range temp[MAX_RESULT_RANGES];
    for (int i = 0; i < map_count; i++) {
        int new_count = apply_map_to_ranges(ranges, range_count, &maps[i], temp);
        range_count = new_count;
        for (int j = 0; j < new_count; j++) {
            ranges[j] = temp[j];
        }
    }

    // Find minimum start of any range
    int64_t min_location = INT64_MAX;
    for (int i = 0; i < range_count; i++) {
        if (ranges[i].start < min_location) {
            min_location = ranges[i].start;
        }
    }

    return min_location;
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %lld\n", part1());
    printf("Part 2: %lld\n", part2());

    return 0;
}
