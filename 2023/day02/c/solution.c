#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE 512

// Part 1: Check if game is possible with 12 red, 13 green, 14 blue
int is_game_possible(const char *line) {
    const int MAX_RED = 12;
    const int MAX_GREEN = 13;
    const int MAX_BLUE = 14;

    // Skip past "Game X: "
    const char *ptr = strchr(line, ':');
    if (!ptr) return 0;
    ptr++; // Move past the colon

    // Parse each draw (separated by semicolons)
    char *game_data = strdup(ptr);
    char *saveptr1, *saveptr2;
    char *draw = strtok_r(game_data, ";", &saveptr1);

    while (draw != NULL) {
        // Parse each color count in this draw (separated by commas)
        char *draw_copy = strdup(draw);
        char *cube = strtok_r(draw_copy, ",", &saveptr2);

        while (cube != NULL) {
            // Skip leading whitespace
            while (*cube && isspace(*cube)) cube++;

            // Parse number
            int count = atoi(cube);

            // Find color
            while (*cube && isdigit(*cube)) cube++;
            while (*cube && isspace(*cube)) cube++;

            if (strncmp(cube, "red", 3) == 0) {
                if (count > MAX_RED) {
                    free(draw_copy);
                    free(game_data);
                    return 0;
                }
            } else if (strncmp(cube, "green", 5) == 0) {
                if (count > MAX_GREEN) {
                    free(draw_copy);
                    free(game_data);
                    return 0;
                }
            } else if (strncmp(cube, "blue", 4) == 0) {
                if (count > MAX_BLUE) {
                    free(draw_copy);
                    free(game_data);
                    return 0;
                }
            }

            cube = strtok_r(NULL, ",", &saveptr2);
        }

        free(draw_copy);
        draw = strtok_r(NULL, ";", &saveptr1);
    }

    free(game_data);
    return 1;
}

// Part 2: Find minimum cubes needed and calculate power
int calculate_game_power(const char *line) {
    int min_red = 0, min_green = 0, min_blue = 0;

    // Skip past "Game X: "
    const char *ptr = strchr(line, ':');
    if (!ptr) return 0;
    ptr++;

    // Parse each draw (separated by semicolons)
    char *game_data = strdup(ptr);
    char *saveptr1, *saveptr2;
    char *draw = strtok_r(game_data, ";", &saveptr1);

    while (draw != NULL) {
        // Parse each color count in this draw (separated by commas)
        char *draw_copy = strdup(draw);
        char *cube = strtok_r(draw_copy, ",", &saveptr2);

        while (cube != NULL) {
            // Skip leading whitespace
            while (*cube && isspace(*cube)) cube++;

            // Parse number
            int count = atoi(cube);

            // Find color
            while (*cube && isdigit(*cube)) cube++;
            while (*cube && isspace(*cube)) cube++;

            if (strncmp(cube, "red", 3) == 0) {
                if (count > min_red) min_red = count;
            } else if (strncmp(cube, "green", 5) == 0) {
                if (count > min_green) min_green = count;
            } else if (strncmp(cube, "blue", 4) == 0) {
                if (count > min_blue) min_blue = count;
            }

            cube = strtok_r(NULL, ",", &saveptr2);
        }

        free(draw_copy);
        draw = strtok_r(NULL, ";", &saveptr1);
    }

    free(game_data);
    return min_red * min_green * min_blue;
}

int main() {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        fprintf(stderr, "Error opening input.txt\n");
        return 1;
    }

    char line[MAX_LINE];
    int game_id = 0;
    int part1_sum = 0;
    int part2_sum = 0;

    while (fgets(line, sizeof(line), fp)) {
        game_id++;

        // Remove trailing newline
        line[strcspn(line, "\n")] = 0;

        // Part 1: Check if game is possible
        if (is_game_possible(line)) {
            part1_sum += game_id;
        }

        // Part 2: Calculate power
        part2_sum += calculate_game_power(line);
    }

    fclose(fp);

    printf("Part 1: %d\n", part1_sum);
    printf("Part 2: %d\n", part2_sum);

    return 0;
}
