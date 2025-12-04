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
    // Test with the example from the problem
    const char *input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
                       "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
                       "824824821-824824827,2121212118-2121212124";

    char *line = strdup(input);
    long long total_sum = 0;

    // Parse ranges separated by commas
    char *token = strtok(line, ",");
    while (token != NULL) {
        // Parse range (start-end)
        long long start, end;
        if (sscanf(token, "%lld-%lld", &start, &end) == 2) {
            printf("Range %lld-%lld: ", start, end);
            int count = 0;
            // Check each number in the range
            for (long long num = start; num <= end; num++) {
                if (is_invalid_id(num)) {
                    printf("%lld ", num);
                    total_sum += num;
                    count++;
                }
            }
            if (count == 0) {
                printf("none");
            }
            printf("\n");
        }
        token = strtok(NULL, ",");
    }

    printf("\nTotal sum: %lld (expected: 1227775554)\n", total_sum);

    free(line);
    return 0;
}
