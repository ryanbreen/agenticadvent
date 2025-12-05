#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_RULES 2000
#define MAX_UPDATES 300
#define MAX_PAGES 100
#define MAX_PAGES_IN_UPDATE 50

typedef struct {
    int before;
    int after;
} Rule;

typedef struct {
    int pages[MAX_PAGES_IN_UPDATE];
    int count;
} Update;

Rule rules[MAX_RULES];
int rule_count = 0;

Update updates[MAX_UPDATES];
int update_count = 0;

// Check if there's a rule that says 'before' must come before 'after'
bool has_rule(int before, int after) {
    for (int i = 0; i < rule_count; i++) {
        if (rules[i].before == before && rules[i].after == after) {
            return true;
        }
    }
    return false;
}

// Check if an update is in valid order
bool is_valid_order(Update *update) {
    for (int i = 0; i < update->count; i++) {
        int page = update->pages[i];

        // For each page that must come after this page
        for (int j = 0; j < rule_count; j++) {
            if (rules[j].before == page) {
                int must_be_after = rules[j].after;

                // Check if must_be_after is in this update
                for (int k = 0; k < update->count; k++) {
                    if (update->pages[k] == must_be_after) {
                        // must_be_after is in the update
                        // Check if it comes before current page (violation)
                        if (k < i) {
                            return false;
                        }
                        break;
                    }
                }
            }
        }
    }
    return true;
}

// Comparison function for sorting
int compare_pages(const void *a, const void *b, void *arg) {
    int page_a = *(int *)a;
    int page_b = *(int *)b;

    // If a must come before b, return -1
    if (has_rule(page_a, page_b)) {
        return -1;
    }
    // If b must come before a, return 1
    if (has_rule(page_b, page_a)) {
        return 1;
    }
    return 0;
}

// qsort doesn't support context in standard C, so we'll implement bubble sort
// with our custom comparison
void fix_order(Update *update) {
    bool swapped;
    do {
        swapped = false;
        for (int i = 0; i < update->count - 1; i++) {
            int a = update->pages[i];
            int b = update->pages[i + 1];

            // If b must come before a, swap them
            if (has_rule(b, a)) {
                update->pages[i] = b;
                update->pages[i + 1] = a;
                swapped = true;
            }
        }
    } while (swapped);
}

int part1() {
    int total = 0;
    for (int i = 0; i < update_count; i++) {
        if (is_valid_order(&updates[i])) {
            int middle_idx = updates[i].count / 2;
            total += updates[i].pages[middle_idx];
        }
    }
    return total;
}

int part2() {
    int total = 0;
    for (int i = 0; i < update_count; i++) {
        if (!is_valid_order(&updates[i])) {
            // Make a copy and fix it
            Update fixed = updates[i];
            fix_order(&fixed);
            int middle_idx = fixed.count / 2;
            total += fixed.pages[middle_idx];
        }
    }
    return total;
}

int main() {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        fprintf(stderr, "Error: Cannot open input.txt\n");
        return 1;
    }

    char line[256];
    bool reading_rules = true;

    while (fgets(line, sizeof(line), f)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;

        if (strlen(line) == 0) {
            reading_rules = false;
            continue;
        }

        if (reading_rules) {
            // Parse rule: X|Y
            int before, after;
            if (sscanf(line, "%d|%d", &before, &after) == 2) {
                rules[rule_count].before = before;
                rules[rule_count].after = after;
                rule_count++;
            }
        } else {
            // Parse update: comma-separated numbers
            Update *update = &updates[update_count];
            update->count = 0;

            char *token = strtok(line, ",");
            while (token != NULL) {
                update->pages[update->count++] = atoi(token);
                token = strtok(NULL, ",");
            }
            update_count++;
        }
    }

    fclose(f);

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
