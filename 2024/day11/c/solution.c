#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_STONES 20
#define CACHE_SIZE 1000000

// Hash table entry for memoization
typedef struct CacheEntry {
    uint64_t value;
    int blinks;
    uint64_t result;
    struct CacheEntry *next;
} CacheEntry;

CacheEntry *cache[CACHE_SIZE];

// Hash function for (value, blinks) pair
unsigned int hash(uint64_t value, int blinks) {
    uint64_t combined = value * 1000 + blinks;
    return (unsigned int)(combined % CACHE_SIZE);
}

// Get cached result or return 0 if not found
int get_cache(uint64_t value, int blinks, uint64_t *result) {
    unsigned int idx = hash(value, blinks);
    CacheEntry *entry = cache[idx];

    while (entry != NULL) {
        if (entry->value == value && entry->blinks == blinks) {
            *result = entry->result;
            return 1;
        }
        entry = entry->next;
    }
    return 0;
}

// Store result in cache
void set_cache(uint64_t value, int blinks, uint64_t result) {
    unsigned int idx = hash(value, blinks);
    CacheEntry *entry = malloc(sizeof(CacheEntry));
    entry->value = value;
    entry->blinks = blinks;
    entry->result = result;
    entry->next = cache[idx];
    cache[idx] = entry;
}

// Count number of digits in a number
int count_digits(uint64_t n) {
    if (n == 0) return 1;
    int count = 0;
    while (n > 0) {
        count++;
        n /= 10;
    }
    return count;
}

// Split a number into left and right halves
void split_number(uint64_t value, int digits, uint64_t *left, uint64_t *right) {
    int mid = digits / 2;
    uint64_t divisor = 1;
    for (int i = 0; i < mid; i++) {
        divisor *= 10;
    }
    *left = value / divisor;
    *right = value % divisor;
}

// Count how many stones result from a single stone after N blinks
uint64_t count_stones(uint64_t value, int blinks) {
    // Base case
    if (blinks == 0) {
        return 1;
    }

    // Check cache
    uint64_t cached_result;
    if (get_cache(value, blinks, &cached_result)) {
        return cached_result;
    }

    uint64_t result;

    // Rule 1: 0 becomes 1
    if (value == 0) {
        result = count_stones(1, blinks - 1);
    }
    // Rule 2: Even number of digits -> split
    else {
        int digits = count_digits(value);
        if (digits % 2 == 0) {
            uint64_t left, right;
            split_number(value, digits, &left, &right);
            result = count_stones(left, blinks - 1) + count_stones(right, blinks - 1);
        }
        // Rule 3: Multiply by 2024
        else {
            result = count_stones(value * 2024, blinks - 1);
        }
    }

    // Store in cache
    set_cache(value, blinks, result);

    return result;
}

// Free cache memory
void free_cache() {
    for (int i = 0; i < CACHE_SIZE; i++) {
        CacheEntry *entry = cache[i];
        while (entry != NULL) {
            CacheEntry *temp = entry;
            entry = entry->next;
            free(temp);
        }
        cache[i] = NULL;
    }
}

int main() {
    FILE *file = fopen("../input.txt", "r");
    if (!file) {
        perror("Failed to open input.txt");
        return 1;
    }

    // Read all stones from input
    uint64_t stones[MAX_STONES];
    int stone_count = 0;

    while (fscanf(file, "%llu", &stones[stone_count]) == 1) {
        stone_count++;
        if (stone_count >= MAX_STONES) {
            fprintf(stderr, "Too many stones in input\n");
            fclose(file);
            return 1;
        }
    }
    fclose(file);

    // Initialize cache
    memset(cache, 0, sizeof(cache));

    // Part 1: 25 blinks
    uint64_t part1_total = 0;
    for (int i = 0; i < stone_count; i++) {
        part1_total += count_stones(stones[i], 25);
    }
    printf("Part 1: %llu\n", part1_total);

    // Part 2: 75 blinks (cache is already populated from part 1)
    uint64_t part2_total = 0;
    for (int i = 0; i < stone_count; i++) {
        part2_total += count_stones(stones[i], 75);
    }
    printf("Part 2: %llu\n", part2_total);

    // Cleanup
    free_cache();

    return 0;
}
