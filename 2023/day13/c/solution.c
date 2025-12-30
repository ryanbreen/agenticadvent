#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PATTERNS 100
#define MAX_LINES 50
#define MAX_LINE_LEN 50

typedef struct {
    char lines[MAX_LINES][MAX_LINE_LEN];
    int num_lines;
    int width;
} Pattern;

typedef struct {
    Pattern items[MAX_PATTERNS];
    int count;
} PatternList;

static int parse_input(const char *filename, PatternList *list) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Error opening file");
        return -1;
    }

    char line[MAX_LINE_LEN];
    int current = 0;
    list->items[current].num_lines = 0;

    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\n")] = '\0';

        if (line[0] == '\0') {
            if (list->items[current].num_lines > 0) {
                current++;
                list->items[current].num_lines = 0;
            }
        } else {
            Pattern *p = &list->items[current];
            strcpy(p->lines[p->num_lines], line);
            p->width = (int)strlen(line);
            p->num_lines++;
        }
    }

    list->count = (list->items[current].num_lines > 0) ? current + 1 : current;
    fclose(fp);
    return 0;
}

static int count_vertical_differences(const Pattern *p, int col) {
    int total_diff = 0;
    int left_len = col;
    int right_len = p->width - col;
    int check_len = (left_len < right_len) ? left_len : right_len;

    for (int row = 0; row < p->num_lines; row++) {
        for (int i = 0; i < check_len; i++) {
            if (p->lines[row][col - 1 - i] != p->lines[row][col + i]) {
                total_diff++;
            }
        }
    }
    return total_diff;
}

static int count_horizontal_differences(const Pattern *p, int row) {
    int total_diff = 0;
    int top_len = row;
    int bottom_len = p->num_lines - row;
    int check_len = (top_len < bottom_len) ? top_len : bottom_len;

    for (int i = 0; i < check_len; i++) {
        const char *top_row = p->lines[row - 1 - i];
        const char *bottom_row = p->lines[row + i];
        for (int c = 0; c < p->width; c++) {
            if (top_row[c] != bottom_row[c]) {
                total_diff++;
            }
        }
    }
    return total_diff;
}

static int find_vertical_reflection(const Pattern *p, int target_diff) {
    for (int col = 1; col < p->width; col++) {
        if (count_vertical_differences(p, col) == target_diff) {
            return col;
        }
    }
    return 0;
}

static int find_horizontal_reflection(const Pattern *p, int target_diff) {
    for (int row = 1; row < p->num_lines; row++) {
        if (count_horizontal_differences(p, row) == target_diff) {
            return row;
        }
    }
    return 0;
}

static int summarize_pattern(const Pattern *p, int target_diff) {
    int v = find_vertical_reflection(p, target_diff);
    if (v > 0) {
        return v;
    }
    return find_horizontal_reflection(p, target_diff) * 100;
}

static int solve(const PatternList *list, int target_diff) {
    int sum = 0;
    for (int i = 0; i < list->count; i++) {
        sum += summarize_pattern(&list->items[i], target_diff);
    }
    return sum;
}

int main(void) {
    PatternList patterns;

    if (parse_input("../input.txt", &patterns) != 0) {
        return 1;
    }

    printf("Part 1: %d\n", solve(&patterns, 0));
    printf("Part 2: %d\n", solve(&patterns, 1));

    return 0;
}
