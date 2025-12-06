#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

#define MAX_ROWS 200
#define MAX_COLS 200

char grid[MAX_ROWS][MAX_COLS];
int rows = 0;
int cols = 0;

// Structure to track a number and its position
typedef struct {
    int value;
    int row;
    int start_col;
    int end_col;
} Number;

Number numbers[5000];
int num_count = 0;

// Check if a character is a symbol (not digit, not period)
bool is_symbol(char c) {
    return c != '.' && !isdigit(c) && c != '\0' && c != '\n';
}

// Check if a number at given position is adjacent to any symbol
bool is_adjacent_to_symbol(int row, int start_col, int end_col) {
    // Check all 8 directions around the number
    for (int r = row - 1; r <= row + 1; r++) {
        for (int c = start_col - 1; c <= end_col + 1; c++) {
            // Skip out of bounds
            if (r < 0 || r >= rows || c < 0 || c >= cols) continue;
            // Skip the number itself
            if (r == row && c >= start_col && c <= end_col) continue;

            if (is_symbol(grid[r][c])) {
                return true;
            }
        }
    }
    return false;
}

// Get all numbers adjacent to a position
void get_adjacent_numbers(int row, int col, int *adj_nums, int *count) {
    *count = 0;

    // Check all 8 directions
    for (int r = row - 1; r <= row + 1; r++) {
        for (int c = col - 1; c <= col + 1; c++) {
            // Skip out of bounds
            if (r < 0 || r >= rows || c < 0 || c >= cols) continue;
            // Skip center
            if (r == row && c == col) continue;

            // If this is a digit, find the complete number
            if (isdigit(grid[r][c])) {
                // Find which number this belongs to
                for (int i = 0; i < num_count; i++) {
                    if (numbers[i].row == r &&
                        c >= numbers[i].start_col &&
                        c <= numbers[i].end_col) {
                        // Check if we already have this number index
                        bool found = false;
                        for (int j = 0; j < *count; j++) {
                            if (adj_nums[j] == i) {
                                found = true;
                                break;
                            }
                        }

                        if (!found) {
                            adj_nums[*count] = i;  // Store index
                            (*count)++;
                        }
                        break;
                    }
                }
            }
        }
    }
}

int part1() {
    int sum = 0;

    for (int i = 0; i < num_count; i++) {
        if (is_adjacent_to_symbol(numbers[i].row, numbers[i].start_col, numbers[i].end_col)) {
            sum += numbers[i].value;
        }
    }

    return sum;
}

int part2() {
    int sum = 0;

    // Find all '*' symbols
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '*') {
                int adj_nums[10];
                int count;
                get_adjacent_numbers(r, c, adj_nums, &count);

                // If exactly 2 adjacent numbers, it's a gear
                if (count == 2) {
                    int gear_ratio = numbers[adj_nums[0]].value * numbers[adj_nums[1]].value;
                    sum += gear_ratio;
                }
            }
        }
    }

    return sum;
}

int main(int argc, char *argv[]) {
    const char *filename = (argc > 1) ? argv[1] : "../input.txt";
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Error opening file");
        return 1;
    }

    // Read the grid
    char line[MAX_COLS];
    while (fgets(line, sizeof(line), f) && rows < MAX_ROWS) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;
        strcpy(grid[rows], line);
        if (cols == 0) {
            cols = strlen(line);
        }
        rows++;
    }
    fclose(f);

    // Parse all numbers and their positions
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (isdigit(grid[r][c])) {
                int start = c;
                int value = 0;

                // Parse the complete number
                while (c < cols && isdigit(grid[r][c])) {
                    value = value * 10 + (grid[r][c] - '0');
                    c++;
                }

                // Store the number
                numbers[num_count].value = value;
                numbers[num_count].row = r;
                numbers[num_count].start_col = start;
                numbers[num_count].end_col = c - 1;
                num_count++;

                c--; // Adjust because loop will increment
            }
        }
    }

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
