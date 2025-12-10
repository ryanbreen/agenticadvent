/*
 * Day 10: Factory - Linear algebra over GF(2) and rational Gaussian elimination
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>
#include <math.h>

#define MAX_BUTTONS 100
#define MAX_LIGHTS 100
#define MAX_LINE 1024

/* Rational number for Part 2 */
typedef struct {
    int64_t num;
    int64_t den;
} Rational;

/* GCD for rational simplification */
int64_t gcd(int64_t a, int64_t b) {
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    while (b != 0) {
        int64_t t = b;
        b = a % b;
        a = t;
    }
    return a;
}

Rational make_rational(int64_t num, int64_t den) {
    if (den == 0) {
        fprintf(stderr, "Division by zero\n");
        exit(1);
    }
    if (den < 0) {
        num = -num;
        den = -den;
    }
    int64_t g = gcd(num, den);
    Rational r = {num / g, den / g};
    return r;
}

Rational add_rational(Rational a, Rational b) {
    return make_rational(a.num * b.den + b.num * a.den, a.den * b.den);
}

Rational sub_rational(Rational a, Rational b) {
    return make_rational(a.num * b.den - b.num * a.den, a.den * b.den);
}

Rational mul_rational(Rational a, Rational b) {
    return make_rational(a.num * b.num, a.den * b.den);
}

Rational div_rational(Rational a, Rational b) {
    return make_rational(a.num * b.den, a.den * b.num);
}

int rational_is_zero(Rational r) {
    return r.num == 0;
}

int rational_is_negative(Rational r) {
    return r.num < 0;
}

int rational_is_integer(Rational r) {
    return r.den == 1;
}

int64_t rational_to_int(Rational r) {
    return r.num / r.den;
}

double rational_to_double(Rational r) {
    return (double)r.num / (double)r.den;
}

/* Parse indicator lights and buttons for Part 1 */
int parse_line_part1(const char *line, uint64_t *target, uint64_t *buttons, int *button_indices[MAX_BUTTONS], int *button_counts) {
    char *s = strdup(line);
    char *p = s;

    /* Find indicator pattern [...] */
    char *start = strchr(p, '[');
    char *end = strchr(p, ']');
    if (!start || !end) {
        free(s);
        return 0;
    }

    *target = 0;
    int n_lights = 0;
    for (char *c = start + 1; c < end; c++) {
        if (*c == '#') {
            *target |= (1ULL << n_lights);
        }
        n_lights++;
    }

    /* Extract buttons (...) */
    int n_buttons = 0;
    p = end + 1;
    while (*p) {
        if (*p == '(') {
            char *btn_start = p + 1;
            char *btn_end = strchr(btn_start, ')');
            if (!btn_end) break;

            *btn_end = '\0';
            buttons[n_buttons] = 0;

            /* Parse comma-separated indices */
            char *token = strtok(btn_start, ",");
            button_counts[n_buttons] = 0;
            button_indices[n_buttons] = malloc(MAX_LIGHTS * sizeof(int));

            while (token) {
                int idx = atoi(token);
                button_indices[n_buttons][button_counts[n_buttons]++] = idx;
                buttons[n_buttons] |= (1ULL << idx);
                token = strtok(NULL, ",");
            }

            n_buttons++;
            p = btn_end + 1;
        } else {
            p++;
        }
    }

    free(s);
    return n_buttons;
}

/* Parse joltage requirements for Part 2 */
int parse_line_part2(const char *line, int *joltage, int *button_indices[MAX_BUTTONS], int *button_counts) {
    char *s = strdup(line);
    char *p = s;

    /* Find joltage requirements {...} */
    char *start = strchr(p, '{');
    char *end = strchr(p, '}');
    if (!start || !end) {
        free(s);
        return 0;
    }

    *start = '\0';
    *end = '\0';
    int n_counters = 0;
    char *token = strtok(start + 1, ",");
    while (token) {
        joltage[n_counters++] = atoi(token);
        token = strtok(NULL, ",");
    }

    /* Extract buttons (...) */
    int n_buttons = 0;
    p = s;
    while (*p) {
        if (*p == '(') {
            char *btn_start = p + 1;
            char *btn_end = strchr(btn_start, ')');
            if (!btn_end) break;

            *btn_end = '\0';

            /* Parse comma-separated indices */
            token = strtok(btn_start, ",");
            button_counts[n_buttons] = 0;
            button_indices[n_buttons] = malloc(MAX_LIGHTS * sizeof(int));

            while (token) {
                int idx = atoi(token);
                button_indices[n_buttons][button_counts[n_buttons]++] = idx;
                token = strtok(NULL, ",");
            }

            n_buttons++;
            p = btn_end + 1;
        } else {
            p++;
        }
    }

    free(s);
    return n_buttons;
}

/* Count set bits */
int popcount(uint64_t n) {
    int count = 0;
    while (n) {
        count += n & 1;
        n >>= 1;
    }
    return count;
}

/* Solve Part 1 machine using brute force */
int solve_machine_part1(int n_buttons, uint64_t target, uint64_t *buttons) {
    if (n_buttons > 20) {
        /* Too large for brute force */
        return 0;
    }

    int min_presses = INT_MAX;

    /* Try all 2^n_buttons combinations */
    for (uint64_t mask = 0; mask < (1ULL << n_buttons); mask++) {
        uint64_t state = 0;
        int presses = 0;

        for (int i = 0; i < n_buttons; i++) {
            if (mask & (1ULL << i)) {
                state ^= buttons[i];
                presses++;
            }
        }

        if (state == target) {
            if (presses < min_presses) {
                min_presses = presses;
            }
        }
    }

    return min_presses == INT_MAX ? 0 : min_presses;
}

/* Gaussian elimination for Part 2 */
int solve_machine_part2(int n_counters, int *joltage, int n_buttons, int *button_indices[MAX_BUTTONS], int *button_counts) {
    if (n_buttons == 0) return 0;

    /* Build augmented matrix [A | b] where A is n_counters x n_buttons */
    Rational aug[MAX_LIGHTS][MAX_BUTTONS + 1];
    for (int i = 0; i < n_counters; i++) {
        for (int j = 0; j < n_buttons; j++) {
            aug[i][j] = make_rational(0, 1);
        }
        aug[i][n_buttons] = make_rational(joltage[i], 1);
    }

    /* Fill matrix A */
    for (int j = 0; j < n_buttons; j++) {
        for (int k = 0; k < button_counts[j]; k++) {
            int idx = button_indices[j][k];
            if (idx < n_counters) {
                aug[idx][j] = make_rational(1, 1);
            }
        }
    }

    /* Gaussian elimination to RREF */
    int pivot_cols[MAX_BUTTONS];
    int pivot_rows[MAX_BUTTONS];
    int n_pivots = 0;
    int pivot_row = 0;

    for (int col = 0; col < n_buttons && pivot_row < n_counters; col++) {
        /* Find non-zero entry in this column */
        int found = -1;
        for (int row = pivot_row; row < n_counters; row++) {
            if (!rational_is_zero(aug[row][col])) {
                found = row;
                break;
            }
        }

        if (found == -1) continue;

        /* Swap rows */
        if (found != pivot_row) {
            for (int c = 0; c <= n_buttons; c++) {
                Rational tmp = aug[pivot_row][c];
                aug[pivot_row][c] = aug[found][c];
                aug[found][c] = tmp;
            }
        }

        pivot_cols[n_pivots] = col;
        pivot_rows[n_pivots] = pivot_row;
        n_pivots++;

        /* Scale pivot row */
        Rational scale = aug[pivot_row][col];
        for (int c = 0; c <= n_buttons; c++) {
            aug[pivot_row][c] = div_rational(aug[pivot_row][c], scale);
        }

        /* Eliminate column in other rows */
        for (int row = 0; row < n_counters; row++) {
            if (row != pivot_row && !rational_is_zero(aug[row][col])) {
                Rational factor = aug[row][col];
                for (int c = 0; c <= n_buttons; c++) {
                    aug[row][c] = sub_rational(aug[row][c], mul_rational(factor, aug[pivot_row][c]));
                }
            }
        }

        pivot_row++;
    }

    /* Check for inconsistency */
    for (int row = pivot_row; row < n_counters; row++) {
        if (!rational_is_zero(aug[row][n_buttons])) {
            return 0;  /* No solution */
        }
    }

    /* Identify free variables */
    int is_pivot[MAX_BUTTONS] = {0};
    for (int i = 0; i < n_pivots; i++) {
        is_pivot[pivot_cols[i]] = 1;
    }

    int free_vars[MAX_BUTTONS];
    int n_free = 0;
    for (int i = 0; i < n_buttons; i++) {
        if (!is_pivot[i]) {
            free_vars[n_free++] = i;
        }
    }

    /* If no free variables, unique solution */
    if (n_free == 0) {
        Rational solution[MAX_BUTTONS];
        for (int i = 0; i < n_buttons; i++) {
            solution[i] = make_rational(0, 1);
        }

        for (int i = 0; i < n_pivots; i++) {
            solution[pivot_cols[i]] = aug[pivot_rows[i]][n_buttons];
        }

        int total = 0;
        for (int i = 0; i < n_buttons; i++) {
            if (rational_is_negative(solution[i]) || !rational_is_integer(solution[i])) {
                return 0;
            }
            total += rational_to_int(solution[i]);
        }
        return total;
    }

    /* With free variables, extract null space and particular solution */
    Rational null_vectors[MAX_BUTTONS][MAX_BUTTONS];
    Rational particular[MAX_BUTTONS];

    for (int i = 0; i < n_buttons; i++) {
        particular[i] = make_rational(0, 1);
    }

    for (int i = 0; i < n_pivots; i++) {
        particular[pivot_cols[i]] = aug[pivot_rows[i]][n_buttons];
    }

    for (int fv_idx = 0; fv_idx < n_free; fv_idx++) {
        int fv = free_vars[fv_idx];
        for (int i = 0; i < n_buttons; i++) {
            null_vectors[fv_idx][i] = make_rational(0, 1);
        }
        null_vectors[fv_idx][fv] = make_rational(1, 1);

        for (int i = 0; i < n_pivots; i++) {
            null_vectors[fv_idx][pivot_cols[i]] = make_rational(-aug[pivot_rows[i]][fv].num, aug[pivot_rows[i]][fv].den);
        }
    }

    /* Search for optimal solution */
    int max_j = 0;
    for (int i = 0; i < n_counters; i++) {
        if (joltage[i] > max_j) max_j = joltage[i];
    }

    int search_bound = max_j;
    if (search_bound < 50) search_bound = 50;
    if (search_bound > 200) search_bound = 200;

    int min_total = INT_MAX;

    /* Simple bounded search for small number of free variables */
    if (n_free == 1) {
        /* For 1D, compute tighter bounds using constraints */
        int64_t t_low = -search_bound * 10;
        int64_t t_high = search_bound * 10;

        for (int j = 0; j < n_buttons; j++) {
            double p = rational_to_double(particular[j]);
            double nv = rational_to_double(null_vectors[0][j]);

            if (nv > 0.0001) {
                int64_t bound = (int64_t)ceil(-p / nv);
                if (bound > t_low) t_low = bound;
            } else if (nv < -0.0001) {
                int64_t bound = (int64_t)floor(-p / nv);
                if (bound < t_high) t_high = bound;
            } else {
                if (p < 0) {
                    return 0;  /* No solution */
                }
            }
        }

        if (t_low > t_high) return 0;

        /* Clamp to reasonable range */
        if (t_low < -search_bound * 10) t_low = -search_bound * 10;
        if (t_high > search_bound * 10) t_high = search_bound * 10;
        for (int64_t t0 = t_low; t0 <= t_high; t0++) {
            Rational t0_r = make_rational(t0, 1);
            int valid = 1;
            int total = 0;

            for (int j = 0; j < n_buttons; j++) {
                Rational val = add_rational(particular[j], mul_rational(t0_r, null_vectors[0][j]));
                if (rational_is_negative(val) || !rational_is_integer(val)) {
                    valid = 0;
                    break;
                }
                total += rational_to_int(val);
            }

            if (valid && total < min_total) {
                min_total = total;
            }
        }
    } else if (n_free == 2) {
        for (int t0 = -search_bound * 2; t0 <= search_bound * 2; t0++) {
            Rational t0_r = make_rational(t0, 1);

            /* Compute dynamic bounds for t1 given t0 */
            double t1_low = -search_bound * 2.0;
            double t1_high = search_bound * 2.0;

            for (int j = 0; j < n_buttons; j++) {
                Rational inter = add_rational(particular[j], mul_rational(t0_r, null_vectors[0][j]));
                double p = rational_to_double(inter);
                double nv = rational_to_double(null_vectors[1][j]);

                if (nv > 0.0001) {
                    double bound = -p / nv;
                    if (bound > t1_low) t1_low = bound;
                } else if (nv < -0.0001) {
                    double bound = -p / nv;
                    if (bound < t1_high) t1_high = bound;
                }
            }

            int64_t t1_start = (int64_t)ceil(t1_low);
            int64_t t1_end = (int64_t)floor(t1_high);

            for (int64_t t1 = t1_start; t1 <= t1_end; t1++) {
                Rational t1_r = make_rational(t1, 1);
                int valid = 1;
                int total = 0;

                for (int j = 0; j < n_buttons; j++) {
                    Rational val = particular[j];
                    val = add_rational(val, mul_rational(t0_r, null_vectors[0][j]));
                    val = add_rational(val, mul_rational(t1_r, null_vectors[1][j]));

                    if (rational_is_negative(val) || !rational_is_integer(val)) {
                        valid = 0;
                        break;
                    }
                    total += rational_to_int(val);
                }

                if (valid && total < min_total) {
                    min_total = total;
                }
            }
        }
    } else if (n_free == 3) {
        for (int t0 = -search_bound; t0 <= search_bound; t0++) {
            Rational t0_r = make_rational(t0, 1);

            /* Compute dynamic bounds for t1 given t0 */
            double t1_low = -search_bound * 2.0;
            double t1_high = search_bound * 2.0;

            for (int j = 0; j < n_buttons; j++) {
                Rational inter = add_rational(particular[j], mul_rational(t0_r, null_vectors[0][j]));
                double p = rational_to_double(inter);
                double nv = rational_to_double(null_vectors[1][j]);

                if (nv > 0.0001) {
                    double bound = -p / nv - search_bound;
                    if (bound > t1_low) t1_low = bound;
                } else if (nv < -0.0001) {
                    double bound = -p / nv + search_bound;
                    if (bound < t1_high) t1_high = bound;
                }
            }

            int64_t t1_start = (int64_t)ceil(t1_low);
            int64_t t1_end = (int64_t)floor(t1_high);
            if (t1_start < -search_bound) t1_start = -search_bound;
            if (t1_end > search_bound) t1_end = search_bound;

            for (int64_t t1 = t1_start; t1 <= t1_end; t1++) {
                Rational t1_r = make_rational(t1, 1);

                /* Compute dynamic bounds for t2 given t0, t1 */
                double t2_low = -search_bound * 2.0;
                double t2_high = search_bound * 2.0;

                for (int j = 0; j < n_buttons; j++) {
                    Rational inter = add_rational(particular[j], mul_rational(t0_r, null_vectors[0][j]));
                    inter = add_rational(inter, mul_rational(t1_r, null_vectors[1][j]));
                    double p = rational_to_double(inter);
                    double nv = rational_to_double(null_vectors[2][j]);

                    if (nv > 0.0001) {
                        double bound = -p / nv;
                        if (bound > t2_low) t2_low = bound;
                    } else if (nv < -0.0001) {
                        double bound = -p / nv;
                        if (bound < t2_high) t2_high = bound;
                    }
                }

                int64_t t2_start = (int64_t)ceil(t2_low);
                int64_t t2_end = (int64_t)floor(t2_high);

                for (int64_t t2 = t2_start; t2 <= t2_end; t2++) {
                    Rational t2_r = make_rational(t2, 1);
                    int valid = 1;
                    int total = 0;

                    for (int j = 0; j < n_buttons; j++) {
                        Rational val = particular[j];
                        val = add_rational(val, mul_rational(t0_r, null_vectors[0][j]));
                        val = add_rational(val, mul_rational(t1_r, null_vectors[1][j]));
                        val = add_rational(val, mul_rational(t2_r, null_vectors[2][j]));

                        if (rational_is_negative(val) || !rational_is_integer(val)) {
                            valid = 0;
                            break;
                        }
                        total += rational_to_int(val);
                    }

                    if (valid && total < min_total) {
                        min_total = total;
                    }
                }
            }
        }
    }

    return min_total == INT_MAX ? 0 : min_total;
}

int part1(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open file");
        return 0;
    }

    char line[MAX_LINE];
    int total = 0;

    while (fgets(line, sizeof(line), f)) {
        /* Remove newline */
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) == 0) continue;

        uint64_t target;
        uint64_t buttons[MAX_BUTTONS];
        int *button_indices[MAX_BUTTONS];
        int button_counts[MAX_BUTTONS];

        int n_buttons = parse_line_part1(line, &target, buttons, button_indices, button_counts);

        if (n_buttons > 0) {
            int min_presses = solve_machine_part1(n_buttons, target, buttons);
            total += min_presses;
        }

        /* Cleanup */
        for (int i = 0; i < n_buttons; i++) {
            free(button_indices[i]);
        }
    }

    fclose(f);
    return total;
}

int part2(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open file");
        return 0;
    }

    char line[MAX_LINE];
    int total = 0;

    while (fgets(line, sizeof(line), f)) {
        /* Remove newline */
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) == 0) continue;

        int joltage[MAX_LIGHTS];
        int *button_indices[MAX_BUTTONS];
        int button_counts[MAX_BUTTONS];

        int n_buttons = parse_line_part2(line, joltage, button_indices, button_counts);

        if (n_buttons > 0) {
            /* Count joltage requirements */
            char *s = strdup(line);
            char *start = strchr(s, '{');
            char *end = strchr(s, '}');
            *start = '\0';
            *end = '\0';
            int n_counters = 0;
            char *token = strtok(start + 1, ",");
            while (token) {
                n_counters++;
                token = strtok(NULL, ",");
            }
            free(s);

            int min_presses = solve_machine_part2(n_counters, joltage, n_buttons, button_indices, button_counts);
            total += min_presses;
        }

        /* Cleanup */
        for (int i = 0; i < n_buttons; i++) {
            free(button_indices[i]);
        }
    }

    fclose(f);
    return total;
}

int main() {
    printf("Part 1: %d\n", part1("../input.txt"));
    printf("Part 2: %d\n", part2("../input.txt"));
    return 0;
}
