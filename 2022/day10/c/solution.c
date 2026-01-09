#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 32
#define CRT_WIDTH 40
#define CRT_HEIGHT 6

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    int x = 1;
    int cycle = 0;
    int part1_sum = 0;
    int target_cycles[] = {20, 60, 100, 140, 180, 220};
    int target_idx = 0;
    char crt[CRT_HEIGHT][CRT_WIDTH + 1];

    // Initialize CRT
    for (int row = 0; row < CRT_HEIGHT; row++) {
        memset(crt[row], '.', CRT_WIDTH);
        crt[row][CRT_WIDTH] = '\0';
    }

    char line[MAX_LINE];
    while (fgets(line, sizeof(line), fp)) {
        // Remove newline
        line[strcspn(line, "\n")] = '\0';

        int cycles_to_add;
        int value_to_add = 0;

        if (strncmp(line, "noop", 4) == 0) {
            cycles_to_add = 1;
        } else {
            // addx V
            sscanf(line, "addx %d", &value_to_add);
            cycles_to_add = 2;
        }

        for (int i = 0; i < cycles_to_add; i++) {
            // During this cycle
            int pos = cycle % CRT_WIDTH;
            int row = cycle / CRT_WIDTH;

            // Draw pixel if sprite overlaps (sprite is 3 pixels wide centered at x)
            if (row < CRT_HEIGHT && abs(pos - x) <= 1) {
                crt[row][pos] = '#';
            }

            cycle++;

            // Check signal strength at target cycles
            if (target_idx < 6 && cycle == target_cycles[target_idx]) {
                part1_sum += cycle * x;
                target_idx++;
            }
        }

        // After instruction completes, update X
        x += value_to_add;
    }

    fclose(fp);

    printf("Part 1: %d\n", part1_sum);
    printf("Part 2:\n");
    for (int row = 0; row < CRT_HEIGHT; row++) {
        printf("%s\n", crt[row]);
    }

    return 0;
}
