#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    char command[16];
    int value;

    /* Part 1 variables */
    long horizontal1 = 0;
    long depth1 = 0;

    /* Part 2 variables */
    long horizontal2 = 0;
    long depth2 = 0;
    long aim = 0;

    while (fscanf(fp, "%s %d", command, &value) == 2) {
        if (strcmp(command, "forward") == 0) {
            horizontal1 += value;
            horizontal2 += value;
            depth2 += aim * value;
        } else if (strcmp(command, "down") == 0) {
            depth1 += value;
            aim += value;
        } else if (strcmp(command, "up") == 0) {
            depth1 -= value;
            aim -= value;
        }
    }

    fclose(fp);

    printf("Part 1: %ld\n", horizontal1 * depth1);
    printf("Part 2: %ld\n", horizontal2 * depth2);

    return 0;
}
