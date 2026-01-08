#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ELVES 1000

int compare_desc(const void *a, const void *b) {
    return *(int *)b - *(int *)a;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input file");
        return 1;
    }

    int elves[MAX_ELVES];
    int num_elves = 0;
    int current_total = 0;
    char line[64];

    while (fgets(line, sizeof(line), fp)) {
        // Remove trailing newline
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }

        if (len == 0) {
            // Blank line - end of current elf's inventory
            if (current_total > 0) {
                elves[num_elves++] = current_total;
                current_total = 0;
            }
        } else {
            // Parse calorie value and add to current total
            current_total += atoi(line);
        }
    }

    // Don't forget the last elf if file doesn't end with blank line
    if (current_total > 0) {
        elves[num_elves++] = current_total;
    }

    fclose(fp);

    // Part 1: Find maximum
    int max_calories = 0;
    for (int i = 0; i < num_elves; i++) {
        if (elves[i] > max_calories) {
            max_calories = elves[i];
        }
    }

    // Part 2: Sum of top 3
    qsort(elves, num_elves, sizeof(int), compare_desc);
    int top_three = 0;
    for (int i = 0; i < 3 && i < num_elves; i++) {
        top_three += elves[i];
    }

    printf("Part 1: %d\n", max_calories);
    printf("Part 2: %d\n", top_three);

    return 0;
}
