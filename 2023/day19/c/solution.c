/* Day 19: Aplenty - Workflow processing and range analysis */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_WORKFLOWS 600
#define MAX_RULES 10
#define MAX_PARTS 300
#define MAX_NAME_LEN 8

typedef struct {
    char attr;          /* 'x', 'm', 'a', 's', or '\0' for default */
    char op;            /* '<' or '>' or '\0' for default */
    int value;
    char destination[MAX_NAME_LEN];
} Rule;

typedef struct {
    char name[MAX_NAME_LEN];
    Rule rules[MAX_RULES];
    int num_rules;
} Workflow;

typedef struct {
    int x, m, a, s;
} Part;

typedef struct {
    int lo, hi;  /* inclusive range */
} Range;

typedef struct {
    Range x, m, a, s;
} Ranges;

static Workflow workflows[MAX_WORKFLOWS];
static int num_workflows = 0;
static Part parts[MAX_PARTS];
static int num_parts = 0;

static int find_workflow(const char *name) {
    for (int i = 0; i < num_workflows; i++) {
        if (strcmp(workflows[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

static void parse_workflow(char *line) {
    if (num_workflows >= MAX_WORKFLOWS) {
        fprintf(stderr, "Error: exceeded maximum number of workflows (%d)\n", MAX_WORKFLOWS);
        exit(1);
    }
    Workflow *wf = &workflows[num_workflows++];

    /* Parse name */
    char *brace = strchr(line, '{');
    size_t name_len = brace - line;
    if (name_len >= MAX_NAME_LEN) {
        name_len = MAX_NAME_LEN - 1;
    }
    strncpy(wf->name, line, name_len);
    wf->name[name_len] = '\0';

    /* Parse rules */
    char *rules_start = brace + 1;
    char *rules_end = strchr(rules_start, '}');
    *rules_end = '\0';

    wf->num_rules = 0;
    char *token = strtok(rules_start, ",");
    while (token != NULL) {
        Rule *rule = &wf->rules[wf->num_rules++];

        char *colon = strchr(token, ':');
        if (colon != NULL) {
            /* Conditional rule */
            rule->attr = token[0];
            rule->op = token[1];
            rule->value = atoi(token + 2);
            strncpy(rule->destination, colon + 1, MAX_NAME_LEN - 1);
            rule->destination[MAX_NAME_LEN - 1] = '\0';
        } else {
            /* Default rule */
            rule->attr = '\0';
            rule->op = '\0';
            rule->value = 0;
            strncpy(rule->destination, token, MAX_NAME_LEN - 1);
            rule->destination[MAX_NAME_LEN - 1] = '\0';
        }

        token = strtok(NULL, ",");
    }
}

static void parse_part(char *line) {
    Part *p = &parts[num_parts++];
    /* Format: {x=787,m=2655,a=1222,s=2876} */
    sscanf(line, "{x=%d,m=%d,a=%d,s=%d}", &p->x, &p->m, &p->a, &p->s);
}

static void parse_input(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Failed to open file");
        exit(1);
    }

    char line[256];
    bool in_parts = false;

    while (fgets(line, sizeof(line), fp)) {
        /* Remove newline */
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') {
            line[len-1] = '\0';
            len--;
        }

        if (len == 0) {
            in_parts = true;
            continue;
        }

        if (!in_parts) {
            parse_workflow(line);
        } else {
            parse_part(line);
        }
    }

    fclose(fp);
}

static int get_attr(const Part *p, char attr) {
    switch (attr) {
        case 'x': return p->x;
        case 'm': return p->m;
        case 'a': return p->a;
        case 's': return p->s;
    }
    return 0;
}

static bool process_part(const Part *p) {
    int wf_idx = find_workflow("in");

    while (1) {
        const Workflow *wf = &workflows[wf_idx];

        for (int i = 0; i < wf->num_rules; i++) {
            const Rule *rule = &wf->rules[i];
            bool matches = false;

            if (rule->attr == '\0') {
                /* Default rule always matches */
                matches = true;
            } else {
                int val = get_attr(p, rule->attr);
                if (rule->op == '<') {
                    matches = (val < rule->value);
                } else {
                    matches = (val > rule->value);
                }
            }

            if (matches) {
                if (strcmp(rule->destination, "A") == 0) {
                    return true;
                }
                if (strcmp(rule->destination, "R") == 0) {
                    return false;
                }
                wf_idx = find_workflow(rule->destination);
                break;
            }
        }
    }
}

static long part1(void) {
    long total = 0;
    for (int i = 0; i < num_parts; i++) {
        if (process_part(&parts[i])) {
            total += parts[i].x + parts[i].m + parts[i].a + parts[i].s;
        }
    }
    return total;
}

static Range *get_range(Ranges *r, char attr) {
    switch (attr) {
        case 'x': return &r->x;
        case 'm': return &r->m;
        case 'a': return &r->a;
        case 's': return &r->s;
    }
    return NULL;
}

static int64_t range_count(const Ranges *r) {
    int64_t result = 1;
    result *= (r->x.hi - r->x.lo + 1);
    result *= (r->m.hi - r->m.lo + 1);
    result *= (r->a.hi - r->a.lo + 1);
    result *= (r->s.hi - r->s.lo + 1);
    return result;
}

static int64_t count_accepted(const char *workflow_name, Ranges ranges) {
    if (strcmp(workflow_name, "R") == 0) {
        return 0;
    }
    if (strcmp(workflow_name, "A") == 0) {
        return range_count(&ranges);
    }

    int wf_idx = find_workflow(workflow_name);
    if (wf_idx < 0) {
        fprintf(stderr, "Error: workflow '%s' not found\n", workflow_name);
        return 0;
    }
    const Workflow *wf = &workflows[wf_idx];

    int64_t total = 0;

    for (int i = 0; i < wf->num_rules; i++) {
        const Rule *rule = &wf->rules[i];

        if (rule->attr == '\0') {
            /* Default rule */
            total += count_accepted(rule->destination, ranges);
        } else {
            Range *r = get_range(&ranges, rule->attr);
            int lo = r->lo;
            int hi = r->hi;

            if (rule->op == '<') {
                /* Split: [lo, value-1] goes to destination, [value, hi] continues */
                if (lo < rule->value) {
                    /* Part that matches the condition */
                    Ranges new_ranges = ranges;
                    Range *new_r = get_range(&new_ranges, rule->attr);
                    new_r->hi = (hi < rule->value - 1) ? hi : rule->value - 1;
                    total += count_accepted(rule->destination, new_ranges);
                }
                /* Remaining part continues to next rule */
                if (hi >= rule->value) {
                    r->lo = (lo > rule->value) ? lo : rule->value;
                } else {
                    break;  /* No remaining range */
                }
            } else {  /* op == '>' */
                /* Split: [value+1, hi] goes to destination, [lo, value] continues */
                if (hi > rule->value) {
                    /* Part that matches the condition */
                    Ranges new_ranges = ranges;
                    Range *new_r = get_range(&new_ranges, rule->attr);
                    new_r->lo = (lo > rule->value + 1) ? lo : rule->value + 1;
                    total += count_accepted(rule->destination, new_ranges);
                }
                /* Remaining part continues to next rule */
                if (lo <= rule->value) {
                    r->hi = (hi < rule->value) ? hi : rule->value;
                } else {
                    break;  /* No remaining range */
                }
            }
        }
    }

    return total;
}

static int64_t part2(void) {
    Ranges initial = {
        .x = {1, 4000},
        .m = {1, 4000},
        .a = {1, 4000},
        .s = {1, 4000}
    };
    return count_accepted("in", initial);
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %ld\n", part1());
    printf("Part 2: %lld\n", (long long)part2());

    return 0;
}
