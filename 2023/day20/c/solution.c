#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_MODULES 64
#define MAX_DESTINATIONS 16
#define MAX_INPUTS 16
#define MAX_NAME_LEN 16
#define QUEUE_SIZE 4096

typedef enum {
    TYPE_BROADCASTER,
    TYPE_FLIPFLOP,
    TYPE_CONJUNCTION,
    TYPE_OUTPUT
} ModuleType;

typedef struct {
    char name[MAX_NAME_LEN];
    ModuleType type;
    char destinations[MAX_DESTINATIONS][MAX_NAME_LEN];
    int num_destinations;

    // Flip-flop state
    bool ff_state;

    // Conjunction memory
    char inputs[MAX_INPUTS][MAX_NAME_LEN];
    bool input_memory[MAX_INPUTS];
    int num_inputs;
} Module;

typedef struct {
    int source_idx;
    int dest_idx;
    bool pulse;  // false = low, true = high
} Pulse;

static Module modules[MAX_MODULES];
static int num_modules = 0;

static Pulse queue[QUEUE_SIZE];
static int queue_head = 0;
static int queue_tail = 0;

static int find_module(const char *name) {
    for (int i = 0; i < num_modules; i++) {
        if (strcmp(modules[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

static int get_or_create_module(const char *name) {
    int idx = find_module(name);
    if (idx >= 0) return idx;

    idx = num_modules++;
    strncpy(modules[idx].name, name, MAX_NAME_LEN - 1);
    modules[idx].name[MAX_NAME_LEN - 1] = '\0';
    modules[idx].type = TYPE_OUTPUT;
    modules[idx].num_destinations = 0;
    modules[idx].ff_state = false;
    modules[idx].num_inputs = 0;
    return idx;
}

static void add_conjunction_input(int module_idx, const char *input_name) {
    Module *m = &modules[module_idx];
    for (int i = 0; i < m->num_inputs; i++) {
        if (strcmp(m->inputs[i], input_name) == 0) return;
    }
    strncpy(m->inputs[m->num_inputs], input_name, MAX_NAME_LEN - 1);
    m->inputs[m->num_inputs][MAX_NAME_LEN - 1] = '\0';
    m->input_memory[m->num_inputs] = false;
    m->num_inputs++;
}

static int find_input_idx(Module *m, const char *input_name) {
    for (int i = 0; i < m->num_inputs; i++) {
        if (strcmp(m->inputs[i], input_name) == 0) {
            return i;
        }
    }
    return -1;
}

static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[256];
    num_modules = 0;

    // First pass: create all modules
    while (fgets(line, sizeof(line), f)) {
        if (line[0] == '\n' || line[0] == '\0') continue;

        char *arrow = strstr(line, " -> ");
        if (!arrow) continue;

        *arrow = '\0';
        char *name_part = line;
        char *dest_part = arrow + 4;

        // Remove newline from dest_part
        char *nl = strchr(dest_part, '\n');
        if (nl) *nl = '\0';

        ModuleType type;
        char name[MAX_NAME_LEN];

        if (strcmp(name_part, "broadcaster") == 0) {
            type = TYPE_BROADCASTER;
            strcpy(name, "broadcaster");
        } else if (name_part[0] == '%') {
            type = TYPE_FLIPFLOP;
            strncpy(name, name_part + 1, MAX_NAME_LEN - 1);
            name[MAX_NAME_LEN - 1] = '\0';
        } else if (name_part[0] == '&') {
            type = TYPE_CONJUNCTION;
            strncpy(name, name_part + 1, MAX_NAME_LEN - 1);
            name[MAX_NAME_LEN - 1] = '\0';
        } else {
            continue;
        }

        int idx = get_or_create_module(name);
        modules[idx].type = type;

        // Parse destinations
        char *token = strtok(dest_part, ", ");
        while (token) {
            // Trim whitespace
            while (*token == ' ') token++;
            char *end = token + strlen(token) - 1;
            while (end > token && *end == ' ') *end-- = '\0';

            if (strlen(token) > 0) {
                strncpy(modules[idx].destinations[modules[idx].num_destinations],
                        token, MAX_NAME_LEN - 1);
                modules[idx].destinations[modules[idx].num_destinations][MAX_NAME_LEN - 1] = '\0';
                modules[idx].num_destinations++;

                // Ensure destination module exists
                get_or_create_module(token);
            }
            token = strtok(NULL, ", ");
        }
    }

    fclose(f);

    // Second pass: set up conjunction inputs
    for (int i = 0; i < num_modules; i++) {
        for (int j = 0; j < modules[i].num_destinations; j++) {
            int dest_idx = find_module(modules[i].destinations[j]);
            if (dest_idx >= 0 && modules[dest_idx].type == TYPE_CONJUNCTION) {
                add_conjunction_input(dest_idx, modules[i].name);
            }
        }
    }
}

static void reset_state(void) {
    for (int i = 0; i < num_modules; i++) {
        modules[i].ff_state = false;
        for (int j = 0; j < modules[i].num_inputs; j++) {
            modules[i].input_memory[j] = false;
        }
    }
}

static void enqueue(int source, int dest, bool pulse) {
    queue[queue_tail].source_idx = source;
    queue[queue_tail].dest_idx = dest;
    queue[queue_tail].pulse = pulse;
    queue_tail = (queue_tail + 1) % QUEUE_SIZE;
}

static bool dequeue(Pulse *p) {
    if (queue_head == queue_tail) return false;
    *p = queue[queue_head];
    queue_head = (queue_head + 1) % QUEUE_SIZE;
    return true;
}

// For part 2: track which watch nodes sent high pulses
static int watch_nodes[MAX_INPUTS];
static int num_watch_nodes = 0;
static bool high_sent[MAX_INPUTS];

static void simulate_button_press(int64_t *low_count, int64_t *high_count) {
    queue_head = queue_tail = 0;

    // Reset high_sent tracking
    for (int i = 0; i < num_watch_nodes; i++) {
        high_sent[i] = false;
    }

    int bc_idx = find_module("broadcaster");
    if (bc_idx < 0) return;

    // Button sends low to broadcaster
    enqueue(-1, bc_idx, false);

    Pulse p;
    while (dequeue(&p)) {
        if (p.pulse) {
            (*high_count)++;
        } else {
            (*low_count)++;
        }

        // Track watched nodes sending high
        if (p.pulse) {
            for (int i = 0; i < num_watch_nodes; i++) {
                if (p.source_idx == watch_nodes[i]) {
                    high_sent[i] = true;
                }
            }
        }

        if (p.dest_idx < 0) continue;

        Module *m = &modules[p.dest_idx];

        switch (m->type) {
            case TYPE_BROADCASTER:
                for (int i = 0; i < m->num_destinations; i++) {
                    int dest_idx = find_module(m->destinations[i]);
                    enqueue(p.dest_idx, dest_idx, p.pulse);
                }
                break;

            case TYPE_FLIPFLOP:
                if (!p.pulse) {
                    m->ff_state = !m->ff_state;
                    for (int i = 0; i < m->num_destinations; i++) {
                        int dest_idx = find_module(m->destinations[i]);
                        enqueue(p.dest_idx, dest_idx, m->ff_state);
                    }
                }
                break;

            case TYPE_CONJUNCTION: {
                // Update memory for the source
                if (p.source_idx >= 0) {
                    int input_idx = find_input_idx(m, modules[p.source_idx].name);
                    if (input_idx >= 0) {
                        m->input_memory[input_idx] = p.pulse;
                    }
                }

                // Check if all inputs are high
                bool all_high = true;
                for (int i = 0; i < m->num_inputs; i++) {
                    if (!m->input_memory[i]) {
                        all_high = false;
                        break;
                    }
                }

                bool output = !all_high;
                for (int i = 0; i < m->num_destinations; i++) {
                    int dest_idx = find_module(m->destinations[i]);
                    enqueue(p.dest_idx, dest_idx, output);
                }
                break;
            }

            case TYPE_OUTPUT:
                // Do nothing
                break;
        }
    }
}

static int64_t part1(void) {
    reset_state();

    int64_t total_low = 0, total_high = 0;

    for (int i = 0; i < 1000; i++) {
        simulate_button_press(&total_low, &total_high);
    }

    return total_low * total_high;
}

static int64_t gcd(int64_t a, int64_t b) {
    while (b) {
        int64_t t = b;
        b = a % b;
        a = t;
    }
    return a;
}

static int64_t lcm(int64_t a, int64_t b) {
    return a / gcd(a, b) * b;
}

static int64_t part2(void) {
    reset_state();

    // Find the module that feeds into rx
    int rx_idx = find_module("rx");
    if (rx_idx < 0) return 0;

    int rx_input_idx = -1;
    for (int i = 0; i < num_modules; i++) {
        for (int j = 0; j < modules[i].num_destinations; j++) {
            if (strcmp(modules[i].destinations[j], "rx") == 0) {
                rx_input_idx = i;
                break;
            }
        }
        if (rx_input_idx >= 0) break;
    }

    if (rx_input_idx < 0) return 0;

    // Set up watch nodes (inputs to rx_input)
    Module *rx_input = &modules[rx_input_idx];
    num_watch_nodes = rx_input->num_inputs;
    for (int i = 0; i < num_watch_nodes; i++) {
        watch_nodes[i] = find_module(rx_input->inputs[i]);
    }

    // Track cycle lengths
    int64_t cycle_lengths[MAX_INPUTS] = {0};
    int found = 0;

    int64_t button_press = 0;
    while (found < num_watch_nodes) {
        button_press++;
        int64_t low = 0, high = 0;
        simulate_button_press(&low, &high);

        for (int i = 0; i < num_watch_nodes; i++) {
            if (high_sent[i] && cycle_lengths[i] == 0) {
                cycle_lengths[i] = button_press;
                found++;
            }
        }
    }

    // Calculate LCM of all cycle lengths
    int64_t result = 1;
    for (int i = 0; i < num_watch_nodes; i++) {
        result = lcm(result, cycle_lengths[i]);
    }

    return result;
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %lld\n", part1());

    // Re-parse for clean state
    num_modules = 0;
    parse_input("../input.txt");

    printf("Part 2: %lld\n", part2());

    return 0;
}
