#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 1000
#define HASH_SIZE 100003  // Prime number for better distribution

// Hash table entry
typedef struct {
    int key;
    int count;
    int used;
} HashEntry;

// Hash function
unsigned int hash(int key) {
    return (unsigned int)key % HASH_SIZE;
}

// Insert or increment count in hash table
void hash_insert(HashEntry *table, int key) {
    unsigned int idx = hash(key);

    // Linear probing for collision resolution
    while (table[idx].used && table[idx].key != key) {
        idx = (idx + 1) % HASH_SIZE;
    }

    if (!table[idx].used) {
        table[idx].key = key;
        table[idx].count = 1;
        table[idx].used = 1;
    } else {
        table[idx].count++;
    }
}

// Get count from hash table
int hash_get(HashEntry *table, int key) {
    unsigned int idx = hash(key);

    // Linear probing to find the key
    while (table[idx].used) {
        if (table[idx].key == key) {
            return table[idx].count;
        }
        idx = (idx + 1) % HASH_SIZE;
    }

    return 0;  // Not found
}

// Comparison function for qsort
int compare_ints(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int main() {
    FILE *file = fopen("../input.txt", "r");
    if (!file) {
        perror("Failed to open input.txt");
        return 1;
    }

    int left[MAX_LINES], right[MAX_LINES];
    int count = 0;

    // Read input
    while (fscanf(file, "%d %d", &left[count], &right[count]) == 2) {
        count++;
        if (count >= MAX_LINES) break;
    }
    fclose(file);

    // Part 1: Sort both lists and calculate total distance
    int left_sorted[MAX_LINES], right_sorted[MAX_LINES];
    memcpy(left_sorted, left, count * sizeof(int));
    memcpy(right_sorted, right, count * sizeof(int));

    qsort(left_sorted, count, sizeof(int), compare_ints);
    qsort(right_sorted, count, sizeof(int), compare_ints);

    long long part1_sum = 0;
    for (int i = 0; i < count; i++) {
        int diff = left_sorted[i] - right_sorted[i];
        part1_sum += diff > 0 ? diff : -diff;
    }

    // Part 2: Calculate similarity score using hash table
    // Build frequency map of right list
    HashEntry *freq_map = calloc(HASH_SIZE, sizeof(HashEntry));
    if (!freq_map) {
        perror("Failed to allocate hash table");
        return 1;
    }

    for (int i = 0; i < count; i++) {
        hash_insert(freq_map, right[i]);
    }

    // Calculate similarity score
    long long part2_sum = 0;
    for (int i = 0; i < count; i++) {
        int num = left[i];
        int occurrences = hash_get(freq_map, num);
        part2_sum += (long long)num * occurrences;
    }

    free(freq_map);

    printf("Part 1: %lld\n", part1_sum);
    printf("Part 2: %lld\n", part2_sum);

    return 0;
}
