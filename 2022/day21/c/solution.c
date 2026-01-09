#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_MONKEYS 3000
#define NAME_LEN 5

typedef struct {
    char name[NAME_LEN];
    bool is_number;
    int64_t value;
    char left[NAME_LEN];
    char op;
    char right[NAME_LEN];
} Monkey;

static Monkey monkeys[MAX_MONKEYS];
static int monkey_count = 0;

// Hash table for fast lookup
#define HASH_SIZE 4099
static int hash_table[HASH_SIZE];

static int hash_name(const char *name) {
    int h = 0;
    for (int i = 0; name[i]; i++) {
        h = (h * 31 + name[i]) % HASH_SIZE;
    }
    return h;
}

static int find_monkey(const char *name) {
    int h = hash_name(name);
    while (hash_table[h] >= 0) {
        if (strcmp(monkeys[hash_table[h]].name, name) == 0) {
            return hash_table[h];
        }
        h = (h + 1) % HASH_SIZE;
    }
    return -1;
}

static void add_monkey_to_hash(int idx) {
    int h = hash_name(monkeys[idx].name);
    while (hash_table[h] >= 0) {
        h = (h + 1) % HASH_SIZE;
    }
    hash_table[h] = idx;
}

static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    memset(hash_table, -1, sizeof(hash_table));

    char line[64];
    while (fgets(line, sizeof(line), f)) {
        if (line[0] == '\n' || line[0] == '\0') continue;

        Monkey *m = &monkeys[monkey_count];

        // Parse name (first 4 chars before colon)
        strncpy(m->name, line, 4);
        m->name[4] = '\0';

        // Skip ": "
        char *rest = line + 6;

        // Check if it's a number or operation
        if (rest[0] >= '0' && rest[0] <= '9') {
            m->is_number = true;
            m->value = atoll(rest);
        } else {
            m->is_number = false;
            strncpy(m->left, rest, 4);
            m->left[4] = '\0';
            m->op = rest[5];
            strncpy(m->right, rest + 7, 4);
            m->right[4] = '\0';
        }

        add_monkey_to_hash(monkey_count);
        monkey_count++;
    }

    fclose(f);
}

// Memoization array for evaluate
static int64_t memo[MAX_MONKEYS];
static bool memo_valid[MAX_MONKEYS];

static int64_t evaluate(int idx) {
    if (memo_valid[idx]) {
        return memo[idx];
    }

    Monkey *m = &monkeys[idx];
    int64_t result;

    if (m->is_number) {
        result = m->value;
    } else {
        int left_idx = find_monkey(m->left);
        int right_idx = find_monkey(m->right);
        int64_t left_val = evaluate(left_idx);
        int64_t right_val = evaluate(right_idx);

        switch (m->op) {
            case '+': result = left_val + right_val; break;
            case '-': result = left_val - right_val; break;
            case '*': result = left_val * right_val; break;
            case '/': result = left_val / right_val; break;
            default: result = 0;
        }
    }

    memo[idx] = result;
    memo_valid[idx] = true;
    return result;
}

// Check if a monkey's evaluation tree contains 'humn'
static bool contains_memo[MAX_MONKEYS];
static bool contains_memo_valid[MAX_MONKEYS];

static bool contains_humn(int idx) {
    if (contains_memo_valid[idx]) {
        return contains_memo[idx];
    }

    Monkey *m = &monkeys[idx];
    bool result;

    if (strcmp(m->name, "humn") == 0) {
        result = true;
    } else if (m->is_number) {
        result = false;
    } else {
        int left_idx = find_monkey(m->left);
        int right_idx = find_monkey(m->right);
        result = contains_humn(left_idx) || contains_humn(right_idx);
    }

    contains_memo[idx] = result;
    contains_memo_valid[idx] = true;
    return result;
}

static int64_t solve_for_humn(int idx, int64_t target) {
    Monkey *m = &monkeys[idx];

    if (strcmp(m->name, "humn") == 0) {
        return target;
    }

    if (m->is_number) {
        // Can't solve if it's just a number
        return -1;
    }

    int left_idx = find_monkey(m->left);
    int right_idx = find_monkey(m->right);
    bool left_has_humn = contains_humn(left_idx);

    if (left_has_humn) {
        int64_t right_val = evaluate(right_idx);
        int64_t new_target;

        switch (m->op) {
            case '+': new_target = target - right_val; break;
            case '-': new_target = target + right_val; break;
            case '*': new_target = target / right_val; break;
            case '/': new_target = target * right_val; break;
            default: new_target = 0;
        }
        return solve_for_humn(left_idx, new_target);
    } else {
        int64_t left_val = evaluate(left_idx);
        int64_t new_target;

        switch (m->op) {
            case '+': new_target = target - left_val; break;
            case '-': new_target = left_val - target; break;
            case '*': new_target = target / left_val; break;
            case '/': new_target = left_val / target; break;
            default: new_target = 0;
        }
        return solve_for_humn(right_idx, new_target);
    }
}

static int64_t part1(void) {
    memset(memo_valid, false, sizeof(memo_valid));
    int root_idx = find_monkey("root");
    return evaluate(root_idx);
}

static int64_t part2(void) {
    memset(memo_valid, false, sizeof(memo_valid));
    memset(contains_memo_valid, false, sizeof(contains_memo_valid));

    int root_idx = find_monkey("root");
    Monkey *root = &monkeys[root_idx];

    int left_idx = find_monkey(root->left);
    int right_idx = find_monkey(root->right);

    bool left_has_humn = contains_humn(left_idx);

    if (left_has_humn) {
        int64_t target = evaluate(right_idx);
        return solve_for_humn(left_idx, target);
    } else {
        int64_t target = evaluate(left_idx);
        return solve_for_humn(right_idx, target);
    }
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %lld\n", (long long)part1());
    printf("Part 2: %lld\n", (long long)part2());

    return 0;
}
