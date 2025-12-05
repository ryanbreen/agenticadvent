#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_RANGES 200
#define MAX_INGREDIENTS 1100
#define MAX_LINE 256

typedef struct {
    long long start;
    long long end;
} Range;

// Comparison function for qsort
int compare_ranges(const void *a, const void *b) {
    Range *r1 = (Range *)a;
    Range *r2 = (Range *)b;
    if (r1->start < r2->start) return -1;
    if (r1->start > r2->start) return 1;
    return 0;
}

int part1(Range *ranges, int range_count, long long *ingredients, int ingredient_count) {
    int fresh_count = 0;

    for (int i = 0; i < ingredient_count; i++) {
        long long id = ingredients[i];
        for (int j = 0; j < range_count; j++) {
            if (id >= ranges[j].start && id <= ranges[j].end) {
                fresh_count++;
                break;  // Found a match, no need to check other ranges
            }
        }
    }

    return fresh_count;
}

long long part2(Range *ranges, int range_count) {
    // Sort ranges by start position
    qsort(ranges, range_count, sizeof(Range), compare_ranges);

    // Merge overlapping ranges
    Range merged[MAX_RANGES];
    int merged_count = 0;

    for (int i = 0; i < range_count; i++) {
        if (merged_count > 0 && ranges[i].start <= merged[merged_count - 1].end + 1) {
            // Overlapping or adjacent - merge with the last range
            if (ranges[i].end > merged[merged_count - 1].end) {
                merged[merged_count - 1].end = ranges[i].end;
            }
        } else {
            // No overlap - add as new range
            merged[merged_count].start = ranges[i].start;
            merged[merged_count].end = ranges[i].end;
            merged_count++;
        }
    }

    // Count total unique IDs covered by merged ranges
    long long total_count = 0;
    for (int i = 0; i < merged_count; i++) {
        total_count += (merged[i].end - merged[i].start + 1);
    }

    return total_count;
}

int main() {
    FILE *file = fopen("../input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    Range ranges[MAX_RANGES];
    long long ingredients[MAX_INGREDIENTS];
    int range_count = 0;
    int ingredient_count = 0;
    bool past_blank = false;
    char line[MAX_LINE];

    // Read the file
    while (fgets(line, sizeof(line), file)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;

        // Check for blank line
        if (strlen(line) == 0) {
            past_blank = true;
            continue;
        }

        if (!past_blank) {
            // Parse range (format: "start-end")
            long long start, end;
            if (sscanf(line, "%lld-%lld", &start, &end) == 2) {
                ranges[range_count].start = start;
                ranges[range_count].end = end;
                range_count++;
            }
        } else {
            // Parse ingredient ID
            long long id;
            if (sscanf(line, "%lld", &id) == 1) {
                ingredients[ingredient_count] = id;
                ingredient_count++;
            }
        }
    }

    fclose(file);

    // Calculate and print results
    printf("Part 1: %d\n", part1(ranges, range_count, ingredients, ingredient_count));
    printf("Part 2: %lld\n", part2(ranges, range_count));

    return 0;
}
