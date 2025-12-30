#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_PATTERNS 100
#define MAX_LINES 50
#define MAX_LINE_LEN 50

typedef struct {
    char lines[MAX_LINES][MAX_LINE_LEN];
    int num_lines;
    int width;
} Pattern;

Pattern patterns[MAX_PATTERNS];
int num_patterns = 0;

void parse_input(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Error opening file");
        exit(1);
    }

    char line[MAX_LINE_LEN];
    int current_pattern = 0;
    patterns[current_pattern].num_lines = 0;

    while (fgets(line, sizeof(line), fp)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;

        if (strlen(line) == 0) {
            // Empty line - start new pattern
            if (patterns[current_pattern].num_lines > 0) {
                current_pattern++;
                patterns[current_pattern].num_lines = 0;
            }
        } else {
            // Add line to current pattern
            strcpy(patterns[current_pattern].lines[patterns[current_pattern].num_lines], line);
            patterns[current_pattern].width = strlen(line);
            patterns[current_pattern].num_lines++;
        }
    }

    // Count the last pattern if it has lines
    if (patterns[current_pattern].num_lines > 0) {
        num_patterns = current_pattern + 1;
    } else {
        num_patterns = current_pattern;
    }

    fclose(fp);
}

int find_vertical_reflection(Pattern *p) {
    if (p->num_lines == 0) return 0;

    int width = p->width;

    // Try each possible vertical line of reflection
    for (int col = 1; col < width; col++) {
        bool is_reflection = true;

        // Check all rows
        for (int row = 0; row < p->num_lines && is_reflection; row++) {
            // Compare left side with right side (mirrored)
            int left_len = col;
            int right_len = width - col;
            int min_len = (left_len < right_len) ? left_len : right_len;

            for (int i = 0; i < min_len; i++) {
                if (p->lines[row][col - 1 - i] != p->lines[row][col + i]) {
                    is_reflection = false;
                    break;
                }
            }
        }

        if (is_reflection) {
            return col;
        }
    }

    return 0;
}

int find_horizontal_reflection(Pattern *p) {
    if (p->num_lines == 0) return 0;

    int height = p->num_lines;

    // Try each possible horizontal line of reflection
    for (int row = 1; row < height; row++) {
        bool is_reflection = true;

        // Compare top with bottom (mirrored)
        int top_len = row;
        int bottom_len = height - row;
        int min_len = (top_len < bottom_len) ? top_len : bottom_len;

        for (int i = 0; i < min_len && is_reflection; i++) {
            if (strcmp(p->lines[row - 1 - i], p->lines[row + i]) != 0) {
                is_reflection = false;
            }
        }

        if (is_reflection) {
            return row;
        }
    }

    return 0;
}

int summarize_pattern(Pattern *p) {
    int v = find_vertical_reflection(p);
    if (v > 0) return v;

    int h = find_horizontal_reflection(p);
    return h * 100;
}

int count_differences(const char *s1, const char *s2, int len) {
    int diff = 0;
    for (int i = 0; i < len; i++) {
        if (s1[i] != s2[i]) {
            diff++;
        }
    }
    return diff;
}

int find_vertical_reflection_with_smudge(Pattern *p) {
    if (p->num_lines == 0) return 0;

    int width = p->width;

    // Try each possible vertical line of reflection
    for (int col = 1; col < width; col++) {
        int total_diff = 0;

        // Check all rows
        for (int row = 0; row < p->num_lines; row++) {
            // Compare left side with right side (mirrored)
            int left_len = col;
            int right_len = width - col;
            int min_len = (left_len < right_len) ? left_len : right_len;

            for (int i = 0; i < min_len; i++) {
                if (p->lines[row][col - 1 - i] != p->lines[row][col + i]) {
                    total_diff++;
                    if (total_diff > 1) break;
                }
            }
            if (total_diff > 1) break;
        }

        if (total_diff == 1) {
            return col;
        }
    }

    return 0;
}

int find_horizontal_reflection_with_smudge(Pattern *p) {
    if (p->num_lines == 0) return 0;

    int height = p->num_lines;

    // Try each possible horizontal line of reflection
    for (int row = 1; row < height; row++) {
        int total_diff = 0;

        // Compare top with bottom (mirrored)
        int top_len = row;
        int bottom_len = height - row;
        int min_len = (top_len < bottom_len) ? top_len : bottom_len;

        for (int i = 0; i < min_len; i++) {
            total_diff += count_differences(p->lines[row - 1 - i], p->lines[row + i], p->width);
            if (total_diff > 1) break;
        }

        if (total_diff == 1) {
            return row;
        }
    }

    return 0;
}

int summarize_pattern_with_smudge(Pattern *p) {
    int v = find_vertical_reflection_with_smudge(p);
    if (v > 0) return v;

    int h = find_horizontal_reflection_with_smudge(p);
    return h * 100;
}

int part1() {
    int sum = 0;
    for (int i = 0; i < num_patterns; i++) {
        sum += summarize_pattern(&patterns[i]);
    }
    return sum;
}

int part2() {
    int sum = 0;
    for (int i = 0; i < num_patterns; i++) {
        sum += summarize_pattern_with_smudge(&patterns[i]);
    }
    return sum;
}

int main() {
    parse_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
