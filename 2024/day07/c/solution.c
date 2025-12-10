#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#define MAX_NUMS 20
#define MAX_LINE 256

typedef struct {
    int64_t target;
    int64_t nums[MAX_NUMS];
    int count;
} Equation;

// Concatenate two numbers (12 || 345 = 12345)
static inline int64_t concat(int64_t a, int64_t b) {
    int64_t multiplier = 10;
    int64_t temp = b;
    while (temp >= 10) {
        multiplier *= 10;
        temp /= 10;
    }
    return a * multiplier + b;
}

// Evaluate expression with given operators (0=+, 1=*, 2=||)
static inline int64_t evaluate(int64_t *nums, int count, int64_t ops, int num_operators) {
    int64_t result = nums[0];
    for (int i = 0; i < count - 1; i++) {
        int op = ops % num_operators;
        ops /= num_operators;
        if (op == 0) {
            result += nums[i + 1];
        } else if (op == 1) {
            result *= nums[i + 1];
        } else {
            result = concat(result, nums[i + 1]);
        }
    }
    return result;
}

// Try all combinations of operators
bool can_make_target(Equation *eq, int num_operators) {
    int n_ops = eq->count - 1;
    if (n_ops == 0) {
        return eq->nums[0] == eq->target;
    }

    // Total combinations: num_operators^n_ops
    int64_t total = 1;
    for (int i = 0; i < n_ops; i++) {
        total *= num_operators;
    }

    for (int64_t ops = 0; ops < total; ops++) {
        if (evaluate(eq->nums, eq->count, ops, num_operators) == eq->target) {
            return true;
        }
    }

    return false;
}

// Parse a line into an equation
bool parse_line(const char *line, Equation *eq) {
    char *colon = strchr(line, ':');
    if (!colon) return false;

    char target_str[64];
    int target_len = colon - line;
    strncpy(target_str, line, target_len);
    target_str[target_len] = '\0';
    eq->target = atoll(target_str);

    const char *ptr = colon + 2;
    eq->count = 0;

    while (*ptr && *ptr != '\n' && eq->count < MAX_NUMS) {
        while (*ptr == ' ') ptr++;
        if (!*ptr || *ptr == '\n') break;

        char num_str[32];
        int i = 0;
        while (*ptr && *ptr != ' ' && *ptr != '\n' && i < 31) {
            num_str[i++] = *ptr++;
        }
        num_str[i] = '\0';

        if (i > 0) {
            eq->nums[eq->count++] = atoll(num_str);
        }
    }

    return eq->count > 0;
}

int main() {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return 1;
    }

    Equation equations[1000];
    int num_equations = 0;

    char line[MAX_LINE];
    while (fgets(line, sizeof(line), f)) {
        if (parse_line(line, &equations[num_equations])) {
            num_equations++;
        }
    }
    fclose(f);

    int64_t part1 = 0, part2 = 0;
    for (int i = 0; i < num_equations; i++) {
        if (can_make_target(&equations[i], 2)) {
            part1 += equations[i].target;
        }
        if (can_make_target(&equations[i], 3)) {
            part2 += equations[i].target;
        }
    }

    printf("Part 1: %lld\n", part1);
    printf("Part 2: %lld\n", part2);

    return 0;
}
