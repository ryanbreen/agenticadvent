#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 1024
#define MAX_SEQUENCES 256
#define MAX_VALUES 128

static long sequences[MAX_SEQUENCES][MAX_VALUES];
static int sequence_lengths[MAX_SEQUENCES];

static int get_differences(long *src, int src_len, long *dst) {
    for (int i = 0; i < src_len - 1; i++) {
        dst[i] = src[i + 1] - src[i];
    }
    return src_len - 1;
}

static bool all_zeros(long *seq, int len) {
    for (int i = 0; i < len; i++) {
        if (seq[i] != 0) {
            return false;
        }
    }
    return true;
}

static long extrapolate_next(long *seq, int len) {
    int num_sequences = 0;

    memcpy(sequences[0], seq, len * sizeof(long));
    sequence_lengths[0] = len;
    num_sequences = 1;

    long *current = sequences[0];
    int current_len = len;

    while (!all_zeros(current, current_len)) {
        sequence_lengths[num_sequences] = get_differences(current, current_len,
                                                          sequences[num_sequences]);
        current = sequences[num_sequences];
        current_len = sequence_lengths[num_sequences];
        num_sequences++;
    }

    for (int i = num_sequences - 2; i >= 0; i--) {
        int this_len = sequence_lengths[i];
        int next_len = sequence_lengths[i + 1];
        long new_val = sequences[i][this_len - 1] + sequences[i + 1][next_len - 1];
        sequences[i][this_len] = new_val;
        sequence_lengths[i]++;
    }

    return sequences[0][sequence_lengths[0] - 1];
}

static long extrapolate_prev(long *seq, int len) {
    int num_sequences = 0;

    memcpy(sequences[0], seq, len * sizeof(long));
    sequence_lengths[0] = len;
    num_sequences = 1;

    long *current = sequences[0];
    int current_len = len;

    while (!all_zeros(current, current_len)) {
        sequence_lengths[num_sequences] = get_differences(current, current_len,
                                                          sequences[num_sequences]);
        current = sequences[num_sequences];
        current_len = sequence_lengths[num_sequences];
        num_sequences++;
    }

    for (int i = num_sequences - 2; i >= 0; i--) {
        long new_val = sequences[i][0] - sequences[i + 1][0];
        int this_len = sequence_lengths[i];
        memmove(&sequences[i][1], &sequences[i][0], this_len * sizeof(long));
        sequences[i][0] = new_val;
        sequence_lengths[i]++;
    }

    return sequences[0][0];
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    long part1 = 0;
    long part2 = 0;

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), fp)) {
        long values[MAX_VALUES];
        int count = 0;

        char *token = strtok(line, " \t\n");
        while (token != NULL && count < MAX_VALUES) {
            values[count++] = atol(token);
            token = strtok(NULL, " \t\n");
        }

        if (count > 0) {
            long copy1[MAX_VALUES];
            long copy2[MAX_VALUES];
            memcpy(copy1, values, count * sizeof(long));
            memcpy(copy2, values, count * sizeof(long));

            part1 += extrapolate_next(copy1, count);
            part2 += extrapolate_prev(copy2, count);
        }
    }

    fclose(fp);

    printf("Part 1: %ld\n", part1);
    printf("Part 2: %ld\n", part2);

    return 0;
}
