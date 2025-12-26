#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ITEMS 500
#define COLS 5
#define ROWS 7

typedef struct {
    int heights[COLS];
} Item;

typedef struct {
    Item items[MAX_ITEMS];
    int count;
} ItemList;

void parse_input(const char *filename, ItemList *locks, ItemList *keys) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        exit(1);
    }

    char lines[ROWS][6];  // 5 chars + null terminator
    int line_count = 0;

    locks->count = 0;
    keys->count = 0;

    char buffer[100];
    while (fgets(buffer, sizeof(buffer), fp)) {
        // Remove newline
        buffer[strcspn(buffer, "\n")] = 0;

        // Skip empty lines between schematics
        if (strlen(buffer) == 0) {
            if (line_count == ROWS) {
                // Process the schematic
                Item item;

                // Check if it's a lock (top row is all #) or key (top row is all .)
                if (strcmp(lines[0], "#####") == 0) {
                    // It's a lock - count # from top (excluding top row)
                    for (int col = 0; col < COLS; col++) {
                        int height = 0;
                        for (int row = 1; row < ROWS; row++) {
                            if (lines[row][col] == '#') {
                                height++;
                            } else {
                                break;
                            }
                        }
                        item.heights[col] = height;
                    }
                    locks->items[locks->count++] = item;
                } else {
                    // It's a key - count # from bottom (excluding bottom row)
                    for (int col = 0; col < COLS; col++) {
                        int height = 0;
                        for (int row = ROWS - 2; row >= 0; row--) {
                            if (lines[row][col] == '#') {
                                height++;
                            } else {
                                break;
                            }
                        }
                        item.heights[col] = height;
                    }
                    keys->items[keys->count++] = item;
                }
            }
            line_count = 0;
            continue;
        }

        // Store the line
        if (line_count < ROWS) {
            strcpy(lines[line_count], buffer);
            line_count++;
        }
    }

    // Process the last schematic if file doesn't end with empty line
    if (line_count == ROWS) {
        Item item;

        if (strcmp(lines[0], "#####") == 0) {
            // It's a lock
            for (int col = 0; col < COLS; col++) {
                int height = 0;
                for (int row = 1; row < ROWS; row++) {
                    if (lines[row][col] == '#') {
                        height++;
                    } else {
                        break;
                    }
                }
                item.heights[col] = height;
            }
            locks->items[locks->count++] = item;
        } else {
            // It's a key
            for (int col = 0; col < COLS; col++) {
                int height = 0;
                for (int row = ROWS - 2; row >= 0; row--) {
                    if (lines[row][col] == '#') {
                        height++;
                    } else {
                        break;
                    }
                }
                item.heights[col] = height;
            }
            keys->items[keys->count++] = item;
        }
    }

    fclose(fp);
}

bool fits(const Item *lock, const Item *key) {
    for (int i = 0; i < COLS; i++) {
        if (lock->heights[i] + key->heights[i] > 5) {
            return false;
        }
    }
    return true;
}

int part1(const ItemList *locks, const ItemList *keys) {
    int count = 0;
    for (int i = 0; i < locks->count; i++) {
        for (int j = 0; j < keys->count; j++) {
            if (fits(&locks->items[i], &keys->items[j])) {
                count++;
            }
        }
    }
    return count;
}

int main(void) {
    ItemList locks, keys;

    parse_input("../input.txt", &locks, &keys);

    int answer1 = part1(&locks, &keys);
    printf("Part 1: %d\n", answer1);

    // Day 25 typically only has Part 1
    printf("Part 2: Merry Christmas!\n");

    return 0;
}
