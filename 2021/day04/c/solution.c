#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define BOARD_SIZE 5
#define MAX_BOARDS 100
#define MAX_NUMBERS 100

typedef struct {
    int cells[BOARD_SIZE][BOARD_SIZE];
    bool marked[BOARD_SIZE][BOARD_SIZE];
} Board;

int numbers[MAX_NUMBERS];
int num_count = 0;
Board boards[MAX_BOARDS];
int board_count = 0;

void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[1024];

    // Parse the draw numbers (first line)
    if (fgets(line, sizeof(line), f)) {
        char *token = strtok(line, ",\n");
        while (token != NULL) {
            numbers[num_count++] = atoi(token);
            token = strtok(NULL, ",\n");
        }
    }

    // Parse boards
    while (fgets(line, sizeof(line), f)) {
        // Skip empty lines
        if (line[0] == '\n' || line[0] == '\0') {
            continue;
        }

        // Start of a new board
        Board *b = &boards[board_count];
        memset(b->marked, false, sizeof(b->marked));

        // First row already in 'line'
        int row = 0;
        do {
            if (line[0] == '\n' || line[0] == '\0') {
                break;
            }

            // Parse 5 numbers from the line
            int col = 0;
            char *ptr = line;
            while (col < BOARD_SIZE && *ptr) {
                while (*ptr == ' ') ptr++;
                if (*ptr == '\n' || *ptr == '\0') break;
                b->cells[row][col] = atoi(ptr);
                col++;
                while (*ptr && *ptr != ' ' && *ptr != '\n') ptr++;
            }

            row++;
            if (row >= BOARD_SIZE) break;
        } while (fgets(line, sizeof(line), f));

        if (row == BOARD_SIZE) {
            board_count++;
        }
    }

    fclose(f);
}

void mark_number(Board *b, int number) {
    for (int row = 0; row < BOARD_SIZE; row++) {
        for (int col = 0; col < BOARD_SIZE; col++) {
            if (b->cells[row][col] == number) {
                b->marked[row][col] = true;
            }
        }
    }
}

bool check_winner(Board *b) {
    // Check rows
    for (int row = 0; row < BOARD_SIZE; row++) {
        bool win = true;
        for (int col = 0; col < BOARD_SIZE; col++) {
            if (!b->marked[row][col]) {
                win = false;
                break;
            }
        }
        if (win) return true;
    }

    // Check columns
    for (int col = 0; col < BOARD_SIZE; col++) {
        bool win = true;
        for (int row = 0; row < BOARD_SIZE; row++) {
            if (!b->marked[row][col]) {
                win = false;
                break;
            }
        }
        if (win) return true;
    }

    return false;
}

int calculate_score(Board *b, int last_number) {
    int unmarked_sum = 0;
    for (int row = 0; row < BOARD_SIZE; row++) {
        for (int col = 0; col < BOARD_SIZE; col++) {
            if (!b->marked[row][col]) {
                unmarked_sum += b->cells[row][col];
            }
        }
    }
    return unmarked_sum * last_number;
}

int part1(void) {
    // Reset marked state for all boards
    for (int i = 0; i < board_count; i++) {
        memset(boards[i].marked, false, sizeof(boards[i].marked));
    }

    for (int n = 0; n < num_count; n++) {
        int number = numbers[n];
        for (int i = 0; i < board_count; i++) {
            mark_number(&boards[i], number);
            if (check_winner(&boards[i])) {
                return calculate_score(&boards[i], number);
            }
        }
    }

    return -1;
}

int part2(void) {
    // Reset marked state for all boards
    for (int i = 0; i < board_count; i++) {
        memset(boards[i].marked, false, sizeof(boards[i].marked));
    }

    bool won[MAX_BOARDS] = {false};
    int last_score = -1;

    for (int n = 0; n < num_count; n++) {
        int number = numbers[n];
        for (int i = 0; i < board_count; i++) {
            if (won[i]) continue;
            mark_number(&boards[i], number);
            if (check_winner(&boards[i])) {
                won[i] = true;
                last_score = calculate_score(&boards[i], number);
            }
        }
    }

    return last_score;
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
