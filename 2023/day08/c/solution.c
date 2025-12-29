#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_NODES 1024
#define NAME_LEN 4

typedef struct {
    char name[NAME_LEN];
    char left[NAME_LEN];
    char right[NAME_LEN];
    int left_idx;
    int right_idx;
} Node;

static Node nodes[MAX_NODES];
static int node_count = 0;
static char instructions[512];
static int instruction_len = 0;

static int find_node_index(const char *name) {
    for (int i = 0; i < node_count; i++) {
        if (strcmp(nodes[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

static void resolve_indices(void) {
    for (int i = 0; i < node_count; i++) {
        nodes[i].left_idx = find_node_index(nodes[i].left);
        nodes[i].right_idx = find_node_index(nodes[i].right);
    }
}

static int64_t gcd(int64_t a, int64_t b) {
    while (b != 0) {
        int64_t t = b;
        b = a % b;
        a = t;
    }
    return a;
}

static int64_t lcm(int64_t a, int64_t b) {
    return a / gcd(a, b) * b;
}

static int64_t part1(void) {
    int aaa_idx = find_node_index("AAA");
    int zzz_idx = find_node_index("ZZZ");

    if (aaa_idx < 0 || zzz_idx < 0) {
        return 0;
    }

    int current = aaa_idx;
    int64_t steps = 0;

    while (current != zzz_idx) {
        char instr = instructions[steps % instruction_len];
        if (instr == 'L') {
            current = nodes[current].left_idx;
        } else {
            current = nodes[current].right_idx;
        }
        steps++;
    }

    return steps;
}

static int64_t find_cycle_length(int start_idx) {
    int current = start_idx;
    int64_t steps = 0;

    while (nodes[current].name[2] != 'Z') {
        char instr = instructions[steps % instruction_len];
        if (instr == 'L') {
            current = nodes[current].left_idx;
        } else {
            current = nodes[current].right_idx;
        }
        steps++;
    }

    return steps;
}

static int64_t part2(void) {
    int start_nodes[MAX_NODES];
    int start_count = 0;

    for (int i = 0; i < node_count; i++) {
        if (nodes[i].name[2] == 'A') {
            start_nodes[start_count++] = i;
        }
    }

    if (start_count == 0) {
        return 0;
    }

    int64_t result = find_cycle_length(start_nodes[0]);
    for (int i = 1; i < start_count; i++) {
        int64_t cycle = find_cycle_length(start_nodes[i]);
        result = lcm(result, cycle);
    }

    return result;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    if (fgets(instructions, sizeof(instructions), fp) == NULL) {
        fclose(fp);
        return 1;
    }

    instruction_len = (int)strlen(instructions);
    while (instruction_len > 0 && (instructions[instruction_len - 1] == '\n' ||
                                    instructions[instruction_len - 1] == '\r')) {
        instructions[--instruction_len] = '\0';
    }

    char line[64];
    while (fgets(line, sizeof(line), fp)) {
        if (strlen(line) < 10) continue;

        char name[NAME_LEN], left[NAME_LEN], right[NAME_LEN];
        if (sscanf(line, "%3s = (%3s, %3s)", name, left, right) == 3) {
            strncpy(nodes[node_count].name, name, NAME_LEN);
            strncpy(nodes[node_count].left, left, NAME_LEN);
            strncpy(nodes[node_count].right, right, NAME_LEN);
            node_count++;
        }
    }

    fclose(fp);

    resolve_indices();

    printf("Part 1: %lld\n", (long long)part1());
    printf("Part 2: %lld\n", (long long)part2());

    return 0;
}
