#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_BLUEPRINTS 64
#define HASH_SIZE 1000003

typedef struct {
    int id;
    int ore_ore;    // ore cost for ore robot
    int clay_ore;   // ore cost for clay robot
    int obs_ore;    // ore cost for obsidian robot
    int obs_clay;   // clay cost for obsidian robot
    int geo_ore;    // ore cost for geode robot
    int geo_obs;    // obsidian cost for geode robot
} Blueprint;

// Hash table for memoization
typedef struct HashEntry {
    uint64_t key;
    int geodes;
    struct HashEntry *next;
} HashEntry;

HashEntry *hash_table[HASH_SIZE];

void hash_clear(void) {
    for (int i = 0; i < HASH_SIZE; i++) {
        HashEntry *e = hash_table[i];
        while (e) {
            HashEntry *next = e->next;
            free(e);
            e = next;
        }
        hash_table[i] = NULL;
    }
}

// Pack state into 64-bit key
// time: 6 bits, ore: 8 bits, clay: 8 bits, obs: 8 bits, oreR: 6 bits, clayR: 6 bits, obsR: 6 bits, geoR: 6 bits
static inline uint64_t make_key(int time, int ore, int clay, int obs, int oreR, int clayR, int obsR, int geoR) {
    return ((uint64_t)time << 54) |
           ((uint64_t)(ore & 0xFF) << 46) |
           ((uint64_t)(clay & 0xFF) << 38) |
           ((uint64_t)(obs & 0xFF) << 30) |
           ((uint64_t)(oreR & 0x3F) << 24) |
           ((uint64_t)(clayR & 0x3F) << 18) |
           ((uint64_t)(obsR & 0x3F) << 12) |
           ((uint64_t)(geoR & 0x3F) << 6);
}

int hash_check_and_set(uint64_t key, int geodes) {
    int idx = key % HASH_SIZE;
    HashEntry *e = hash_table[idx];
    while (e) {
        if (e->key == key) {
            if (e->geodes >= geodes) return 0; // Already seen with equal or better geodes
            e->geodes = geodes;
            return 1;
        }
        e = e->next;
    }
    // Insert new entry
    HashEntry *new_entry = malloc(sizeof(HashEntry));
    new_entry->key = key;
    new_entry->geodes = geodes;
    new_entry->next = hash_table[idx];
    hash_table[idx] = new_entry;
    return 1;
}

static int best_geodes;
static Blueprint *current_bp;
static int time_limit;
static int max_ore, max_clay, max_obs;

static inline int min(int a, int b) { return a < b ? a : b; }
static inline int max(int a, int b) { return a > b ? a : b; }

void dfs(int time, int ore, int clay, int obs, int geodes, int oreR, int clayR, int obsR, int geoR) {
    int remaining = time_limit - time;

    // Upper bound pruning
    int upper_bound = geodes + geoR * remaining + (remaining * (remaining - 1)) / 2;
    if (upper_bound <= best_geodes) return;

    if (time == time_limit) {
        if (geodes > best_geodes) best_geodes = geodes;
        return;
    }

    // Cap resources
    int capped_ore = min(ore, remaining * max_ore);
    int capped_clay = min(clay, remaining * max_clay);
    int capped_obs = min(obs, remaining * max_obs);

    // State memoization
    uint64_t key = make_key(time, capped_ore, capped_clay, capped_obs, oreR, clayR, obsR, geoR);
    if (!hash_check_and_set(key, geodes)) return;

    // Collect resources
    int new_ore = capped_ore + oreR;
    int new_clay = capped_clay + clayR;
    int new_obs = capped_obs + obsR;
    int new_geodes = geodes + geoR;

    // Try building geode robot (always do if possible)
    if (capped_ore >= current_bp->geo_ore && capped_obs >= current_bp->geo_obs) {
        dfs(time + 1, new_ore - current_bp->geo_ore, new_clay, new_obs - current_bp->geo_obs, new_geodes,
            oreR, clayR, obsR, geoR + 1);
        return; // If we can build geode, always do
    }

    // Try building obsidian robot
    if (capped_ore >= current_bp->obs_ore && capped_clay >= current_bp->obs_clay && obsR < max_obs) {
        dfs(time + 1, new_ore - current_bp->obs_ore, new_clay - current_bp->obs_clay, new_obs, new_geodes,
            oreR, clayR, obsR + 1, geoR);
    }

    // Try building clay robot
    if (capped_ore >= current_bp->clay_ore && clayR < max_clay) {
        dfs(time + 1, new_ore - current_bp->clay_ore, new_clay, new_obs, new_geodes,
            oreR, clayR + 1, obsR, geoR);
    }

    // Try building ore robot
    if (capped_ore >= current_bp->ore_ore && oreR < max_ore) {
        dfs(time + 1, new_ore - current_bp->ore_ore, new_clay, new_obs, new_geodes,
            oreR + 1, clayR, obsR, geoR);
    }

    // Do nothing (wait)
    dfs(time + 1, new_ore, new_clay, new_obs, new_geodes,
        oreR, clayR, obsR, geoR);
}

int max_geodes_for_blueprint(Blueprint *bp, int limit) {
    current_bp = bp;
    time_limit = limit;
    best_geodes = 0;

    // Calculate max robots needed
    max_ore = bp->ore_ore;
    if (bp->clay_ore > max_ore) max_ore = bp->clay_ore;
    if (bp->obs_ore > max_ore) max_ore = bp->obs_ore;
    if (bp->geo_ore > max_ore) max_ore = bp->geo_ore;

    max_clay = bp->obs_clay;
    max_obs = bp->geo_obs;

    hash_clear();
    dfs(0, 0, 0, 0, 0, 1, 0, 0, 0);

    return best_geodes;
}

int main(void) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Failed to open input.txt");
        return 1;
    }

    Blueprint blueprints[MAX_BLUEPRINTS];
    int num_blueprints = 0;

    char line[512];
    while (fgets(line, sizeof(line), f) && num_blueprints < MAX_BLUEPRINTS) {
        Blueprint *bp = &blueprints[num_blueprints];
        if (sscanf(line, "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.",
                   &bp->id, &bp->ore_ore, &bp->clay_ore, &bp->obs_ore, &bp->obs_clay, &bp->geo_ore, &bp->geo_obs) == 7) {
            num_blueprints++;
        }
    }
    fclose(f);

    // Part 1: Sum of quality levels (id * geodes) for 24 minutes
    int part1 = 0;
    for (int i = 0; i < num_blueprints; i++) {
        int geodes = max_geodes_for_blueprint(&blueprints[i], 24);
        part1 += blueprints[i].id * geodes;
    }
    printf("Part 1: %d\n", part1);

    // Part 2: Product of geodes for first 3 blueprints at 32 minutes
    int count = num_blueprints < 3 ? num_blueprints : 3;
    long long part2 = 1;
    for (int i = 0; i < count; i++) {
        int geodes = max_geodes_for_blueprint(&blueprints[i], 32);
        part2 *= geodes;
    }
    printf("Part 2: %lld\n", part2);

    return 0;
}
