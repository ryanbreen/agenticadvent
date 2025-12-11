/*
 * Advent of Code 2024 Day 9: Disk Fragmenter
 *
 * Compact a fragmented disk by moving file blocks to fill gaps.
 * Part 1: Move blocks one at a time from end to leftmost free space
 * Part 2: Move whole files to leftmost span that fits
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_DISK_MAP 20100
#define MAX_BLOCKS 200000

typedef struct {
    int start;
    int length;
} FileInfo;

/*
 * Parse disk map into expanded block representation.
 * Returns number of blocks created.
 * blocks array where each element is file ID or -1 for free space.
 */
int parse_disk_map(const char *filename, int *blocks) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open file");
        exit(1);
    }

    char disk_map[MAX_DISK_MAP];
    if (!fgets(disk_map, sizeof(disk_map), f)) {
        fprintf(stderr, "Failed to read disk map\n");
        exit(1);
    }
    fclose(f);

    // Remove trailing newline
    size_t len = strlen(disk_map);
    if (len > 0 && disk_map[len - 1] == '\n') {
        disk_map[len - 1] = '\0';
        len--;
    }

    int block_count = 0;
    int file_id = 0;
    int is_file = 1;

    for (size_t i = 0; i < len; i++) {
        int digit = disk_map[i] - '0';
        if (digit < 0 || digit > 9) {
            continue; // Skip non-digit characters
        }

        if (is_file) {
            for (int j = 0; j < digit; j++) {
                blocks[block_count++] = file_id;
            }
            file_id++;
        } else {
            for (int j = 0; j < digit; j++) {
                blocks[block_count++] = -1; // -1 represents free space
            }
        }
        is_file = !is_file;
    }

    return block_count;
}

/*
 * Compact disk by moving blocks one at a time from end to leftmost free space.
 */
void compact_blocks(int *blocks, int block_count) {
    int left = 0;
    int right = block_count - 1;

    while (left < right) {
        // Find leftmost free space
        while (left < right && blocks[left] != -1) {
            left++;
        }
        // Find rightmost file block
        while (left < right && blocks[right] == -1) {
            right--;
        }

        if (left < right) {
            // Swap
            blocks[left] = blocks[right];
            blocks[right] = -1;
            left++;
            right--;
        }
    }
}

/*
 * Calculate filesystem checksum: sum of position * file_id for each block.
 */
long long calculate_checksum(const int *blocks, int block_count) {
    long long checksum = 0;
    for (int pos = 0; pos < block_count; pos++) {
        if (blocks[pos] != -1) {
            checksum += (long long)pos * blocks[pos];
        }
    }
    return checksum;
}

/*
 * Part 1: Compact by moving individual blocks, return checksum.
 */
long long part1(const char *filename) {
    int *blocks = malloc(MAX_BLOCKS * sizeof(int));
    if (!blocks) {
        perror("Memory allocation failed");
        exit(1);
    }

    int block_count = parse_disk_map(filename, blocks);
    compact_blocks(blocks, block_count);
    long long checksum = calculate_checksum(blocks, block_count);

    free(blocks);
    return checksum;
}

/*
 * Part 2: Compact by moving whole files (highest ID first), return checksum.
 */
long long part2(const char *filename) {
    int *blocks = malloc(MAX_BLOCKS * sizeof(int));
    if (!blocks) {
        perror("Memory allocation failed");
        exit(1);
    }

    int block_count = parse_disk_map(filename, blocks);

    // Find all files: file_id -> (start_pos, length)
    FileInfo *files = calloc(10000, sizeof(FileInfo));
    if (!files) {
        perror("Memory allocation failed");
        exit(1);
    }

    int max_file_id = -1;
    int i = 0;
    while (i < block_count) {
        if (blocks[i] != -1) {
            int file_id = blocks[i];
            int start = i;
            while (i < block_count && blocks[i] == file_id) {
                i++;
            }
            files[file_id].start = start;
            files[file_id].length = i - start;
            if (file_id > max_file_id) {
                max_file_id = file_id;
            }
        } else {
            i++;
        }
    }

    // Process files in decreasing order of file ID
    for (int file_id = max_file_id; file_id >= 0; file_id--) {
        int start = files[file_id].start;
        int length = files[file_id].length;

        // Find leftmost span of free space that fits this file
        // Must be to the left of current position
        int free_start = -1;
        i = 0;
        while (i < start) {
            if (blocks[i] == -1) {
                // Count consecutive free blocks
                int span_start = i;
                int span_length = 0;
                while (i < start && blocks[i] == -1) {
                    span_length++;
                    i++;
                }
                if (span_length >= length) {
                    free_start = span_start;
                    break;
                }
            } else {
                i++;
            }
        }

        // Move file if we found a suitable span
        if (free_start != -1) {
            // Clear old position
            for (int j = start; j < start + length; j++) {
                blocks[j] = -1;
            }
            // Write to new position
            for (int j = free_start; j < free_start + length; j++) {
                blocks[j] = file_id;
            }
            // Update file position
            files[file_id].start = free_start;
        }
    }

    long long checksum = calculate_checksum(blocks, block_count);

    free(files);
    free(blocks);
    return checksum;
}

int main(void) {
    printf("Part 1: %lld\n", part1("../input.txt"));
    printf("Part 2: %lld\n", part2("../input.txt"));
    return 0;
}
