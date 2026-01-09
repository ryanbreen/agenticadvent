#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NUMBERS 10000

typedef struct {
    int orig_idx;
    long long value;
} Node;

static Node indexed[MAX_NUMBERS];
static int n;

void mix(int times) {
    for (int t = 0; t < times; t++) {
        for (int orig_idx = 0; orig_idx < n; orig_idx++) {
            // Find current position of element with this original index
            int curr_pos = 0;
            for (int i = 0; i < n; i++) {
                if (indexed[i].orig_idx == orig_idx) {
                    curr_pos = i;
                    break;
                }
            }

            Node elem = indexed[curr_pos];

            // Remove from current position (shift elements left)
            for (int i = curr_pos; i < n - 1; i++) {
                indexed[i] = indexed[i + 1];
            }

            // Calculate new position (modulo n-1 because we removed the element)
            long long new_pos = (curr_pos + elem.value) % (n - 1);
            if (new_pos < 0) {
                new_pos += (n - 1);
            }

            // Insert at new position (shift elements right)
            for (int i = n - 1; i > new_pos; i--) {
                indexed[i] = indexed[i - 1];
            }
            indexed[new_pos] = elem;
        }
    }
}

long long grove_coordinates(void) {
    // Find zero position
    int zero_idx = 0;
    for (int i = 0; i < n; i++) {
        if (indexed[i].value == 0) {
            zero_idx = i;
            break;
        }
    }

    long long sum = 0;
    sum += indexed[(zero_idx + 1000) % n].value;
    sum += indexed[(zero_idx + 2000) % n].value;
    sum += indexed[(zero_idx + 3000) % n].value;
    return sum;
}

int main(void) {
    // Read input
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Failed to open input file");
        return 1;
    }

    static long long numbers[MAX_NUMBERS];
    n = 0;

    while (fscanf(f, "%lld", &numbers[n]) == 1) {
        n++;
    }
    fclose(f);

    // Part 1: Mix once
    for (int i = 0; i < n; i++) {
        indexed[i].orig_idx = i;
        indexed[i].value = numbers[i];
    }

    mix(1);
    printf("Part 1: %lld\n", grove_coordinates());

    // Part 2: Apply decryption key and mix 10 times
    long long decryption_key = 811589153LL;
    for (int i = 0; i < n; i++) {
        indexed[i].orig_idx = i;
        indexed[i].value = numbers[i] * decryption_key;
    }

    mix(10);
    printf("Part 2: %lld\n", grove_coordinates());

    return 0;
}
