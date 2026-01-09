#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STACKS 10
#define MAX_HEIGHT 100
#define MAX_MOVES 600
#define MAX_LINE 256

typedef struct {
    char crates[MAX_HEIGHT];
    int top;  // index of top element (-1 if empty)
} Stack;

typedef struct {
    int count;
    int from;
    int to;
} Move;

void stack_init(Stack *s) {
    s->top = -1;
}

void stack_push(Stack *s, char c) {
    s->crates[++s->top] = c;
}

char stack_pop(Stack *s) {
    return s->crates[s->top--];
}

char stack_peek(Stack *s) {
    return s->crates[s->top];
}

int stack_empty(Stack *s) {
    return s->top < 0;
}

void copy_stacks(Stack *dest, Stack *src, int num_stacks) {
    for (int i = 0; i < num_stacks; i++) {
        dest[i].top = src[i].top;
        memcpy(dest[i].crates, src[i].crates, src[i].top + 1);
    }
}

int main(void) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Failed to open input.txt");
        return 1;
    }

    char line[MAX_LINE];
    char stack_lines[MAX_HEIGHT][MAX_LINE];
    int num_stack_lines = 0;
    int num_stacks = 0;

    // Read stack diagram until blank line
    while (fgets(line, sizeof(line), f)) {
        // Check for blank line or line with only numbers (stack labels)
        if (line[0] == '\n' || line[0] == '\r') {
            break;
        }
        // Check if this is the number line (starts with " 1")
        if (line[1] == '1') {
            // Count stacks from the number line
            char *p = line;
            int max_num = 0;
            while (*p) {
                if (*p >= '1' && *p <= '9') {
                    int n = *p - '0';
                    if (n > max_num) max_num = n;
                }
                p++;
            }
            num_stacks = max_num;
            continue;
        }
        strcpy(stack_lines[num_stack_lines++], line);
    }

    // Initialize stacks
    Stack stacks[MAX_STACKS];
    for (int i = 0; i < MAX_STACKS; i++) {
        stack_init(&stacks[i]);
    }

    // Parse stacks bottom-up
    for (int row = num_stack_lines - 1; row >= 0; row--) {
        for (int col = 0; col < num_stacks; col++) {
            int pos = 1 + col * 4;  // Position of crate letter
            if (pos < (int)strlen(stack_lines[row])) {
                char c = stack_lines[row][pos];
                if (c != ' ' && c >= 'A' && c <= 'Z') {
                    stack_push(&stacks[col], c);
                }
            }
        }
    }

    // Parse moves
    Move moves[MAX_MOVES];
    int num_moves = 0;

    while (fgets(line, sizeof(line), f)) {
        int count, from, to;
        if (sscanf(line, "move %d from %d to %d", &count, &from, &to) == 3) {
            moves[num_moves].count = count;
            moves[num_moves].from = from - 1;  // 0-indexed
            moves[num_moves].to = to - 1;
            num_moves++;
        }
    }
    fclose(f);

    // Part 1: Move one crate at a time
    Stack stacks1[MAX_STACKS];
    copy_stacks(stacks1, stacks, num_stacks);

    for (int i = 0; i < num_moves; i++) {
        for (int j = 0; j < moves[i].count; j++) {
            char c = stack_pop(&stacks1[moves[i].from]);
            stack_push(&stacks1[moves[i].to], c);
        }
    }

    char result1[MAX_STACKS + 1];
    for (int i = 0; i < num_stacks; i++) {
        result1[i] = stack_empty(&stacks1[i]) ? ' ' : stack_peek(&stacks1[i]);
    }
    result1[num_stacks] = '\0';

    // Part 2: Move multiple crates at once (preserve order)
    Stack stacks2[MAX_STACKS];
    copy_stacks(stacks2, stacks, num_stacks);

    for (int i = 0; i < num_moves; i++) {
        int count = moves[i].count;
        int from = moves[i].from;
        int to = moves[i].to;

        // Use temp array to preserve order
        char temp[MAX_HEIGHT];
        for (int j = count - 1; j >= 0; j--) {
            temp[j] = stack_pop(&stacks2[from]);
        }
        for (int j = 0; j < count; j++) {
            stack_push(&stacks2[to], temp[j]);
        }
    }

    char result2[MAX_STACKS + 1];
    for (int i = 0; i < num_stacks; i++) {
        result2[i] = stack_empty(&stacks2[i]) ? ' ' : stack_peek(&stacks2[i]);
    }
    result2[num_stacks] = '\0';

    printf("Part 1: %s\n", result1);
    printf("Part 2: %s\n", result2);

    return 0;
}
