#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Check if a number is "invalid" - made of a sequence of digits repeated twice
bool is_invalid_id(long long num) {
    char str[32];
    sprintf(str, "%lld", num);
    int len = strlen(str);

    // Must have even length to be repeated twice
    if (len % 2 != 0) {
        return false;
    }

    int half_len = len / 2;

    // Check if first half equals second half
    for (int i = 0; i < half_len; i++) {
        if (str[i] != str[half_len + i]) {
            return false;
        }
    }

    return true;
}

int main() {
    FILE *fp = fopen("/Users/wrb/fun/code/advent2025/day02/input.txt", "r");
    if (!fp) {
        fprintf(stderr, "Error opening input file\n");
        return 1;
    }

    // Read the entire input line
    char *line = NULL;
    size_t len = 0;
    ssize_t read = getline(&line, &len, fp);
    fclose(fp);

    if (read == -1) {
        fprintf(stderr, "Error reading input\n");
        return 1;
    }

    long long total_sum = 0;

    // Parse ranges separated by commas
    char *token = strtok(line, ",");
    while (token != NULL) {
        // Parse range (start-end)
        long long start, end;
        if (sscanf(token, "%lld-%lld", &start, &end) == 2) {
            // Check each number in the range
            for (long long num = start; num <= end; num++) {
                if (is_invalid_id(num)) {
                    total_sum += num;
                }
            }
        }
        token = strtok(NULL, ",");
    }

    printf("Total sum of invalid IDs: %lld\n", total_sum);

    free(line);
    return 0;
}
