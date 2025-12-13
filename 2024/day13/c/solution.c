#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct {
    int64_t ax, ay, bx, by, px, py;
} Machine;

// Parse a line to extract two numbers after X and Y
bool parse_button_line(const char *line, int64_t *x, int64_t *y) {
    const char *x_pos = strstr(line, "X+");
    const char *y_pos = strstr(line, "Y+");

    if (!x_pos || !y_pos) return false;

    *x = atoll(x_pos + 2);
    *y = atoll(y_pos + 2);
    return true;
}

// Parse a prize line to extract X= and Y= values
bool parse_prize_line(const char *line, int64_t *x, int64_t *y) {
    const char *x_pos = strstr(line, "X=");
    const char *y_pos = strstr(line, "Y=");

    if (!x_pos || !y_pos) return false;

    *x = atoll(x_pos + 2);
    *y = atoll(y_pos + 2);
    return true;
}

// Solve using Cramer's rule
// Returns cost (3*a + b) or -1 if no valid solution
int64_t solve_machine(int64_t ax, int64_t ay, int64_t bx, int64_t by,
                       int64_t px, int64_t py, int64_t max_presses) {
    // System: a*ax + b*bx = px
    //         a*ay + b*by = py

    int64_t det = ax * by - ay * bx;

    if (det == 0) {
        return -1;  // No unique solution
    }

    // Calculate numerators
    int64_t a_num = px * by - py * bx;
    int64_t b_num = ax * py - ay * px;

    // Check if solutions are integers
    if (a_num % det != 0 || b_num % det != 0) {
        return -1;
    }

    int64_t a = a_num / det;
    int64_t b = b_num / det;

    // Check non-negative
    if (a < 0 || b < 0) {
        return -1;
    }

    // Check max presses constraint (if applicable)
    if (max_presses >= 0 && (a > max_presses || b > max_presses)) {
        return -1;
    }

    return 3 * a + b;
}

int main() {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        fprintf(stderr, "Error opening input.txt\n");
        return 1;
    }

    // Read entire file
    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *input = malloc(fsize + 1);
    fread(input, 1, fsize, fp);
    input[fsize] = '\0';
    fclose(fp);

    // Parse machines
    Machine machines[500];  // Should be enough for input
    int machine_count = 0;

    char *line = strtok(input, "\n");
    while (line) {
        // Skip empty lines
        if (strlen(line) == 0) {
            line = strtok(NULL, "\n");
            continue;
        }

        Machine m;

        // Button A line
        if (strstr(line, "Button A:") == line) {
            if (!parse_button_line(line, &m.ax, &m.ay)) {
                fprintf(stderr, "Error parsing Button A\n");
                free(input);
                return 1;
            }

            // Button B line
            line = strtok(NULL, "\n");
            if (!line || !parse_button_line(line, &m.bx, &m.by)) {
                fprintf(stderr, "Error parsing Button B\n");
                free(input);
                return 1;
            }

            // Prize line
            line = strtok(NULL, "\n");
            if (!line || !parse_prize_line(line, &m.px, &m.py)) {
                fprintf(stderr, "Error parsing Prize\n");
                free(input);
                return 1;
            }

            machines[machine_count++] = m;
        }

        line = strtok(NULL, "\n");
    }

    // Part 1: Max 100 presses
    int64_t part1_total = 0;
    for (int i = 0; i < machine_count; i++) {
        int64_t cost = solve_machine(
            machines[i].ax, machines[i].ay,
            machines[i].bx, machines[i].by,
            machines[i].px, machines[i].py,
            100  // max presses
        );
        if (cost >= 0) {
            part1_total += cost;
        }
    }

    // Part 2: Shift prize by 10^13, no press limit
    int64_t offset = 10000000000000LL;
    int64_t part2_total = 0;
    for (int i = 0; i < machine_count; i++) {
        int64_t cost = solve_machine(
            machines[i].ax, machines[i].ay,
            machines[i].bx, machines[i].by,
            machines[i].px + offset, machines[i].py + offset,
            -1  // no max presses
        );
        if (cost >= 0) {
            part2_total += cost;
        }
    }

    printf("Part 1: %lld\n", part1_total);
    printf("Part 2: %lld\n", part2_total);

    free(input);
    return 0;
}
