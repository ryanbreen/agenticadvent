#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_BRICKS 1500
#define MAX_X 10
#define MAX_Y 10
#define MAX_Z 400

typedef struct {
    int x1, y1, z1;
    int x2, y2, z2;
    int id;
} Brick;

typedef struct {
    int items[MAX_BRICKS];
    int count;
} IntSet;

// Global arrays for brick support relationships
static IntSet supports[MAX_BRICKS];   // supports[i] = bricks that brick i supports (above)
static IntSet supporters[MAX_BRICKS]; // supporters[i] = bricks that support brick i (below)

static inline int min(int a, int b) { return a < b ? a : b; }
static inline int max(int a, int b) { return a > b ? a : b; }

void set_init(IntSet *s) {
    s->count = 0;
}

bool set_contains(IntSet *s, int val) {
    for (int i = 0; i < s->count; i++) {
        if (s->items[i] == val) return true;
    }
    return false;
}

void set_add(IntSet *s, int val) {
    if (!set_contains(s, val)) {
        s->items[s->count++] = val;
    }
}

bool set_is_subset_of(IntSet *subset, bool *superset_flags) {
    for (int i = 0; i < subset->count; i++) {
        if (!superset_flags[subset->items[i]]) return false;
    }
    return true;
}

int compare_bricks(const void *a, const void *b) {
    const Brick *ba = (const Brick *)a;
    const Brick *bb = (const Brick *)b;
    int za = min(ba->z1, ba->z2);
    int zb = min(bb->z1, bb->z2);
    return za - zb;
}

int parse_input(const char *filename, Brick *bricks) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    int count = 0;
    char line[100];
    while (fgets(line, sizeof(line), f)) {
        if (line[0] == '\n' || line[0] == '\0') continue;

        int x1, y1, z1, x2, y2, z2;
        if (sscanf(line, "%d,%d,%d~%d,%d,%d", &x1, &y1, &z1, &x2, &y2, &z2) == 6) {
            // Ensure z1 <= z2
            if (z1 > z2) {
                int tmp;
                tmp = x1; x1 = x2; x2 = tmp;
                tmp = y1; y1 = y2; y2 = tmp;
                tmp = z1; z1 = z2; z2 = tmp;
            }
            bricks[count].x1 = x1;
            bricks[count].y1 = y1;
            bricks[count].z1 = z1;
            bricks[count].x2 = x2;
            bricks[count].y2 = y2;
            bricks[count].z2 = z2;
            bricks[count].id = count;
            count++;
        }
    }

    fclose(f);
    return count;
}

void settle_bricks(Brick *bricks, int n) {
    // Create array of pointers to sort
    Brick *sorted[MAX_BRICKS];
    for (int i = 0; i < n; i++) {
        sorted[i] = &bricks[i];
    }

    // Sort by minimum z
    for (int i = 0; i < n - 1; i++) {
        for (int j = i + 1; j < n; j++) {
            int za = min(sorted[i]->z1, sorted[i]->z2);
            int zb = min(sorted[j]->z1, sorted[j]->z2);
            if (zb < za) {
                Brick *tmp = sorted[i];
                sorted[i] = sorted[j];
                sorted[j] = tmp;
            }
        }
    }

    // occupied[x][y][z] = brick id that occupies that cell, -1 if empty
    static int occupied[MAX_X][MAX_Y][MAX_Z];
    memset(occupied, -1, sizeof(occupied));

    // Initialize support sets
    for (int i = 0; i < n; i++) {
        set_init(&supports[i]);
        set_init(&supporters[i]);
    }

    // Process each brick in z-order
    for (int i = 0; i < n; i++) {
        Brick *b = sorted[i];
        int orig_idx = b->id;

        int x1 = b->x1, y1 = b->y1, z1 = b->z1;
        int x2 = b->x2, y2 = b->y2, z2 = b->z2;

        // Find maximum drop distance
        int drop = z1 - 1;  // Maximum drop to z=1

        for (int x = min(x1, x2); x <= max(x1, x2); x++) {
            for (int y = min(y1, y2); y <= max(y1, y2); y++) {
                for (int z = z1 - 1; z >= 1; z--) {
                    if (occupied[x][y][z] != -1) {
                        drop = min(drop, z1 - z - 1);
                        break;
                    }
                }
            }
        }

        // Apply the drop
        int new_z1 = z1 - drop;
        int new_z2 = z2 - drop;
        b->z1 = new_z1;
        b->z2 = new_z2;

        // Mark cells as occupied and find supporters
        for (int x = min(x1, x2); x <= max(x1, x2); x++) {
            for (int y = min(y1, y2); y <= max(y1, y2); y++) {
                // Check cell directly below
                if (new_z1 > 1 && occupied[x][y][new_z1 - 1] != -1) {
                    int supporter_idx = occupied[x][y][new_z1 - 1];
                    set_add(&supporters[orig_idx], supporter_idx);
                    set_add(&supports[supporter_idx], orig_idx);
                }

                // Mark all cells of this brick
                for (int z = new_z1; z <= new_z2; z++) {
                    occupied[x][y][z] = orig_idx;
                }
            }
        }
    }
}

int part1(int n) {
    int safe_count = 0;

    for (int i = 0; i < n; i++) {
        bool can_remove = true;

        // Check all bricks that brick i supports
        for (int j = 0; j < supports[i].count; j++) {
            int supported = supports[i].items[j];
            // If this brick has only one supporter (brick i), can't remove
            if (supporters[supported].count == 1) {
                can_remove = false;
                break;
            }
        }

        if (can_remove) safe_count++;
    }

    return safe_count;
}

int part2(int n) {
    int total_falls = 0;

    static int queue[MAX_BRICKS];
    static bool falling[MAX_BRICKS];

    for (int start = 0; start < n; start++) {
        // BFS to find all bricks that would fall
        memset(falling, false, sizeof(bool) * n);

        int head = 0, tail = 0;
        queue[tail++] = start;
        falling[start] = true;

        while (head < tail) {
            int brick = queue[head++];

            // Check all bricks that this brick supports
            for (int j = 0; j < supports[brick].count; j++) {
                int supported = supports[brick].items[j];

                if (falling[supported]) continue;

                // Check if all supporters of this brick have fallen
                if (set_is_subset_of(&supporters[supported], falling)) {
                    falling[supported] = true;
                    queue[tail++] = supported;
                }
            }
        }

        // Count falls (excluding the initial brick)
        total_falls += tail - 1;
    }

    return total_falls;
}

int main(void) {
    static Brick bricks[MAX_BRICKS];

    int n = parse_input("../input.txt", bricks);

    settle_bricks(bricks, n);

    printf("Part 1: %d\n", part1(n));
    printf("Part 2: %d\n", part2(n));

    return 0;
}
