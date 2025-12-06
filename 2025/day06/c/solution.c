#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINES 100
#define MAX_WIDTH 4000

typedef struct {
    long long *numbers;
    int count;
    char op;
} Problem;

typedef struct {
    char **lines;
    int line_count;
    int max_width;
} Worksheet;

Worksheet read_input(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        exit(1);
    }

    Worksheet ws;
    ws.lines = malloc(MAX_LINES * sizeof(char*));
    ws.line_count = 0;
    ws.max_width = 0;

    char buffer[MAX_WIDTH];
    while (fgets(buffer, MAX_WIDTH, fp) && ws.line_count < MAX_LINES) {
        // Remove newline if present
        int len = strlen(buffer);
        if (len > 0 && buffer[len-1] == '\n') {
            buffer[len-1] = '\0';
            len--;
        }

        ws.lines[ws.line_count] = malloc((len + 1) * sizeof(char));
        strcpy(ws.lines[ws.line_count], buffer);

        if (len > ws.max_width) {
            ws.max_width = len;
        }

        ws.line_count++;
    }

    fclose(fp);
    return ws;
}

void free_worksheet(Worksheet *ws) {
    for (int i = 0; i < ws->line_count; i++) {
        free(ws->lines[i]);
    }
    free(ws->lines);
}

void free_problems(Problem *problems, int count) {
    for (int i = 0; i < count; i++) {
        free(problems[i].numbers);
    }
    free(problems);
}

// Pad all lines to the same width
char** pad_lines(Worksheet *ws) {
    char **padded = malloc(ws->line_count * sizeof(char*));
    for (int i = 0; i < ws->line_count; i++) {
        padded[i] = malloc((ws->max_width + 1) * sizeof(char));
        strcpy(padded[i], ws->lines[i]);
        int len = strlen(padded[i]);
        for (int j = len; j < ws->max_width; j++) {
            padded[i][j] = ' ';
        }
        padded[i][ws->max_width] = '\0';
    }
    return padded;
}

Problem* parse_problems_part1(Worksheet *ws, int *problem_count) {
    *problem_count = 0;

    if (ws->line_count == 0) {
        return NULL;
    }

    // Find the operator row (last non-empty row with only +, *, and spaces)
    int op_row_idx = ws->line_count - 1;
    while (op_row_idx >= 0) {
        char *line = ws->lines[op_row_idx];
        int len = strlen(line);
        if (len == 0) {
            op_row_idx--;
            continue;
        }
        // Strip trailing spaces to check if line is meaningful
        while (len > 0 && line[len-1] == ' ') {
            len--;
        }
        if (len == 0) {
            op_row_idx--;
            continue;
        }
        int valid = 1;
        for (int i = 0; line[i]; i++) {
            if (line[i] != '+' && line[i] != '*' && line[i] != ' ') {
                valid = 0;
                break;
            }
        }
        if (valid) break;
        op_row_idx--;
    }

    if (op_row_idx < 0) {
        return NULL;
    }

    // Pad all rows to same width
    char **padded = pad_lines(ws);

    Problem *problems = malloc(MAX_WIDTH * sizeof(Problem));
    int col = 0;

    while (col < ws->max_width) {
        // Skip separator columns (all spaces)
        int is_sep = 1;
        while (col < ws->max_width && is_sep) {
            is_sep = 1;
            for (int row = 0; row < op_row_idx; row++) {
                if (padded[row][col] != ' ') {
                    is_sep = 0;
                    break;
                }
            }
            if (is_sep && padded[op_row_idx][col] != ' ') {
                is_sep = 0;
            }
            if (is_sep) col++;
        }

        if (col >= ws->max_width) break;

        // Find the end of this problem
        int start_col = col;
        while (col < ws->max_width) {
            // Check if this is a separator column
            int is_separator = 1;
            for (int row = 0; row < op_row_idx; row++) {
                if (padded[row][col] != ' ') {
                    is_separator = 0;
                    break;
                }
            }
            if (is_separator && padded[op_row_idx][col] != ' ') {
                is_separator = 0;
            }
            if (is_separator) break;
            col++;
        }

        int end_col = col;

        // Extract numbers for this problem
        long long *numbers = malloc(MAX_LINES * sizeof(long long));
        int num_count = 0;

        for (int row = 0; row < op_row_idx; row++) {
            char num_str[MAX_WIDTH];
            int pos = 0;
            for (int c = start_col; c < end_col; c++) {
                if (padded[row][c] != ' ') {
                    num_str[pos++] = padded[row][c];
                }
            }
            num_str[pos] = '\0';

            if (pos > 0) {
                numbers[num_count++] = atoll(num_str);
            }
        }

        // Extract operator
        char op = '\0';
        for (int c = start_col; c < end_col; c++) {
            if (padded[op_row_idx][c] == '+' || padded[op_row_idx][c] == '*') {
                op = padded[op_row_idx][c];
                break;
            }
        }

        if (op != '\0' && num_count > 0) {
            problems[*problem_count].numbers = numbers;
            problems[*problem_count].count = num_count;
            problems[*problem_count].op = op;
            (*problem_count)++;
        } else {
            free(numbers);
        }
    }

    // Free padded lines
    for (int i = 0; i < ws->line_count; i++) {
        free(padded[i]);
    }
    free(padded);

    return problems;
}

Problem* parse_problems_part2(Worksheet *ws, int *problem_count) {
    *problem_count = 0;

    if (ws->line_count == 0) {
        return NULL;
    }

    // Find the operator row
    int op_row_idx = ws->line_count - 1;
    while (op_row_idx >= 0) {
        char *line = ws->lines[op_row_idx];
        int len = strlen(line);
        if (len == 0) {
            op_row_idx--;
            continue;
        }
        // Strip trailing spaces
        while (len > 0 && line[len-1] == ' ') {
            len--;
        }
        if (len == 0) {
            op_row_idx--;
            continue;
        }
        int valid = 1;
        for (int i = 0; line[i]; i++) {
            if (line[i] != '+' && line[i] != '*' && line[i] != ' ') {
                valid = 0;
                break;
            }
        }
        if (valid) break;
        op_row_idx--;
    }

    if (op_row_idx < 0) {
        return NULL;
    }

    // Pad all rows to same width
    char **padded = pad_lines(ws);

    Problem *problems = malloc(MAX_WIDTH * sizeof(Problem));
    int col = 0;

    while (col < ws->max_width) {
        // Skip separator columns
        int is_sep = 1;
        while (col < ws->max_width && is_sep) {
            is_sep = 1;
            for (int row = 0; row < op_row_idx; row++) {
                if (padded[row][col] != ' ') {
                    is_sep = 0;
                    break;
                }
            }
            if (is_sep && padded[op_row_idx][col] != ' ') {
                is_sep = 0;
            }
            if (is_sep) col++;
        }

        if (col >= ws->max_width) break;

        // Find the end of this problem
        int start_col = col;
        while (col < ws->max_width) {
            int is_separator = 1;
            for (int row = 0; row < op_row_idx; row++) {
                if (padded[row][col] != ' ') {
                    is_separator = 0;
                    break;
                }
            }
            if (is_separator && padded[op_row_idx][col] != ' ') {
                is_separator = 0;
            }
            if (is_separator) break;
            col++;
        }

        int end_col = col;

        // For Part 2: Read columns right-to-left
        long long *numbers = malloc(MAX_WIDTH * sizeof(long long));
        int num_count = 0;

        for (int c = end_col - 1; c >= start_col; c--) {
            char digits[MAX_LINES + 1];
            int digit_count = 0;

            for (int row = 0; row < op_row_idx; row++) {
                char ch = padded[row][c];
                if (isdigit(ch)) {
                    digits[digit_count++] = ch;
                }
            }

            if (digit_count > 0) {
                digits[digit_count] = '\0';
                numbers[num_count++] = atoll(digits);
            }
        }

        // Extract operator
        char op = '\0';
        for (int c = start_col; c < end_col; c++) {
            if (padded[op_row_idx][c] == '+' || padded[op_row_idx][c] == '*') {
                op = padded[op_row_idx][c];
                break;
            }
        }

        if (op != '\0' && num_count > 0) {
            problems[*problem_count].numbers = numbers;
            problems[*problem_count].count = num_count;
            problems[*problem_count].op = op;
            (*problem_count)++;
        } else {
            free(numbers);
        }
    }

    // Free padded lines
    for (int i = 0; i < ws->line_count; i++) {
        free(padded[i]);
    }
    free(padded);

    return problems;
}

long long solve_problem(Problem *p) {
    if (p->op == '+') {
        long long sum = 0;
        for (int i = 0; i < p->count; i++) {
            sum += p->numbers[i];
        }
        return sum;
    } else if (p->op == '*') {
        long long product = 1;
        for (int i = 0; i < p->count; i++) {
            product *= p->numbers[i];
        }
        return product;
    }
    return 0;
}

long long part1(Worksheet *ws) {
    int problem_count;
    Problem *problems = parse_problems_part1(ws, &problem_count);

    long long total = 0;
    for (int i = 0; i < problem_count; i++) {
        total += solve_problem(&problems[i]);
    }

    free_problems(problems, problem_count);
    return total;
}

long long part2(Worksheet *ws) {
    int problem_count;
    Problem *problems = parse_problems_part2(ws, &problem_count);

    long long total = 0;
    for (int i = 0; i < problem_count; i++) {
        total += solve_problem(&problems[i]);
    }

    free_problems(problems, problem_count);
    return total;
}

int main() {
    Worksheet ws = read_input("../input.txt");

    printf("Part 1: %lld\n", part1(&ws));
    printf("Part 2: %lld\n", part2(&ws));

    free_worksheet(&ws);
    return 0;
}
