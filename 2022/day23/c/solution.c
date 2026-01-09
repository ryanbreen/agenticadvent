#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ELVES 10000
#define HASH_SIZE 65536

typedef struct {
    int r, c;
} Pos;

typedef struct Node {
    int r, c;
    int count;
    struct Node *next;
} Node;

typedef struct {
    Node *buckets[HASH_SIZE];
} HashMap;

// Hash function for positions
static inline unsigned int hash_pos(int r, int c) {
    unsigned int h = (unsigned int)(r + 10000) * 31 + (unsigned int)(c + 10000);
    return h & (HASH_SIZE - 1);
}

// Initialize hash map
void hashmap_init(HashMap *map) {
    memset(map->buckets, 0, sizeof(map->buckets));
}

// Free hash map
void hashmap_free(HashMap *map) {
    for (int i = 0; i < HASH_SIZE; i++) {
        Node *n = map->buckets[i];
        while (n) {
            Node *tmp = n;
            n = n->next;
            free(tmp);
        }
        map->buckets[i] = NULL;
    }
}

// Check if position exists in hash map
bool hashmap_contains(HashMap *map, int r, int c) {
    unsigned int h = hash_pos(r, c);
    Node *n = map->buckets[h];
    while (n) {
        if (n->r == r && n->c == c) return true;
        n = n->next;
    }
    return false;
}

// Add position to hash map (for elves set)
void hashmap_add(HashMap *map, int r, int c) {
    if (hashmap_contains(map, r, c)) return;
    unsigned int h = hash_pos(r, c);
    Node *n = malloc(sizeof(Node));
    n->r = r;
    n->c = c;
    n->count = 1;
    n->next = map->buckets[h];
    map->buckets[h] = n;
}

// Increment count for position (for proposal counting)
void hashmap_inc(HashMap *map, int r, int c) {
    unsigned int h = hash_pos(r, c);
    Node *n = map->buckets[h];
    while (n) {
        if (n->r == r && n->c == c) {
            n->count++;
            return;
        }
        n = n->next;
    }
    // Not found, add with count 1
    n = malloc(sizeof(Node));
    n->r = r;
    n->c = c;
    n->count = 1;
    n->next = map->buckets[h];
    map->buckets[h] = n;
}

// Get count for position
int hashmap_get_count(HashMap *map, int r, int c) {
    unsigned int h = hash_pos(r, c);
    Node *n = map->buckets[h];
    while (n) {
        if (n->r == r && n->c == c) return n->count;
        n = n->next;
    }
    return 0;
}

// Direction checks
// N: check (-1,-1), (-1,0), (-1,1); move (-1,0)
// S: check (1,-1), (1,0), (1,1); move (1,0)
// W: check (-1,-1), (0,-1), (1,-1); move (0,-1)
// E: check (-1,1), (0,1), (1,1); move (0,1)

typedef struct {
    int check[3][2];
    int move[2];
} Direction;

static Direction directions[4] = {
    // N
    {{{-1,-1}, {-1,0}, {-1,1}}, {-1, 0}},
    // S
    {{{1,-1}, {1,0}, {1,1}}, {1, 0}},
    // W
    {{{-1,-1}, {0,-1}, {1,-1}}, {0, -1}},
    // E
    {{{-1,1}, {0,1}, {1,1}}, {0, 1}}
};

// All 8 neighbors
static int all_neighbors[8][2] = {
    {-1,-1}, {-1,0}, {-1,1}, {0,-1}, {0,1}, {1,-1}, {1,0}, {1,1}
};

// Elf data
Pos elves[MAX_ELVES];
int num_elves = 0;

// Proposals
int proposals[MAX_ELVES][2];
bool has_proposal[MAX_ELVES];

// Simulate one round
bool simulate_round(HashMap *elf_set, int dir_order[4]) {
    HashMap proposal_counts;
    hashmap_init(&proposal_counts);

    // Phase 1: Each elf proposes a move
    for (int i = 0; i < num_elves; i++) {
        int r = elves[i].r;
        int c = elves[i].c;
        has_proposal[i] = false;

        // Check if any neighbors
        bool has_neighbor = false;
        for (int n = 0; n < 8; n++) {
            int nr = r + all_neighbors[n][0];
            int nc = c + all_neighbors[n][1];
            if (hashmap_contains(elf_set, nr, nc)) {
                has_neighbor = true;
                break;
            }
        }

        if (!has_neighbor) continue;

        // Try each direction in order
        for (int d = 0; d < 4; d++) {
            Direction *dir = &directions[dir_order[d]];
            bool can_move = true;

            for (int k = 0; k < 3; k++) {
                int cr = r + dir->check[k][0];
                int cc = c + dir->check[k][1];
                if (hashmap_contains(elf_set, cr, cc)) {
                    can_move = false;
                    break;
                }
            }

            if (can_move) {
                proposals[i][0] = r + dir->move[0];
                proposals[i][1] = c + dir->move[1];
                has_proposal[i] = true;
                hashmap_inc(&proposal_counts, proposals[i][0], proposals[i][1]);
                break;
            }
        }
    }

    // Phase 2: Execute moves
    bool moved = false;
    hashmap_free(elf_set);
    hashmap_init(elf_set);

    for (int i = 0; i < num_elves; i++) {
        if (has_proposal[i]) {
            int pr = proposals[i][0];
            int pc = proposals[i][1];
            if (hashmap_get_count(&proposal_counts, pr, pc) == 1) {
                elves[i].r = pr;
                elves[i].c = pc;
                moved = true;
            }
        }
        hashmap_add(elf_set, elves[i].r, elves[i].c);
    }

    hashmap_free(&proposal_counts);
    return moved;
}

// Calculate empty tiles in bounding rectangle
int bounding_rect_empty() {
    int min_r = elves[0].r, max_r = elves[0].r;
    int min_c = elves[0].c, max_c = elves[0].c;

    for (int i = 1; i < num_elves; i++) {
        if (elves[i].r < min_r) min_r = elves[i].r;
        if (elves[i].r > max_r) max_r = elves[i].r;
        if (elves[i].c < min_c) min_c = elves[i].c;
        if (elves[i].c > max_c) max_c = elves[i].c;
    }

    int area = (max_r - min_r + 1) * (max_c - min_c + 1);
    return area - num_elves;
}

// Parse input
void parse_input(FILE *f) {
    char line[256];
    int row = 0;

    while (fgets(line, sizeof(line), f)) {
        int len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }

        for (int col = 0; col < len; col++) {
            if (line[col] == '#') {
                elves[num_elves].r = row;
                elves[num_elves].c = col;
                num_elves++;
            }
        }
        row++;
    }
}

// Copy elves for reuse
Pos elves_backup[MAX_ELVES];

void backup_elves() {
    memcpy(elves_backup, elves, sizeof(Pos) * num_elves);
}

void restore_elves() {
    memcpy(elves, elves_backup, sizeof(Pos) * num_elves);
}

int main() {
    // Open input file
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        fprintf(stderr, "Cannot open input.txt\n");
        return 1;
    }

    parse_input(f);
    fclose(f);

    backup_elves();

    // Part 1: 10 rounds
    HashMap elf_set;
    hashmap_init(&elf_set);
    for (int i = 0; i < num_elves; i++) {
        hashmap_add(&elf_set, elves[i].r, elves[i].c);
    }

    int dir_order[4] = {0, 1, 2, 3}; // N, S, W, E

    for (int round = 0; round < 10; round++) {
        simulate_round(&elf_set, dir_order);
        // Rotate direction order
        int first = dir_order[0];
        dir_order[0] = dir_order[1];
        dir_order[1] = dir_order[2];
        dir_order[2] = dir_order[3];
        dir_order[3] = first;
    }

    int part1 = bounding_rect_empty();
    hashmap_free(&elf_set);

    // Part 2: Find first round with no movement
    restore_elves();
    hashmap_init(&elf_set);
    for (int i = 0; i < num_elves; i++) {
        hashmap_add(&elf_set, elves[i].r, elves[i].c);
    }

    dir_order[0] = 0; dir_order[1] = 1; dir_order[2] = 2; dir_order[3] = 3;

    int part2 = 0;
    while (1) {
        part2++;
        bool moved = simulate_round(&elf_set, dir_order);
        if (!moved) break;

        // Rotate direction order
        int first = dir_order[0];
        dir_order[0] = dir_order[1];
        dir_order[1] = dir_order[2];
        dir_order[2] = dir_order[3];
        dir_order[3] = first;
    }

    hashmap_free(&elf_set);

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    return 0;
}
