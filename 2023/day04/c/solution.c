#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_CARDS 250
#define MAX_NUMBERS 50
#define MAX_LINE 256

typedef struct {
    int winning[MAX_NUMBERS];
    int winning_count;
    int have[MAX_NUMBERS];
    int have_count;
} Card;

Card cards[MAX_CARDS];
int card_count = 0;

// Check if a number exists in an array
bool contains(int *arr, int count, int value) {
    for (int i = 0; i < count; i++) {
        if (arr[i] == value) {
            return true;
        }
    }
    return false;
}

// Count matching numbers between winning and have sets
int count_matches(Card *card) {
    int matches = 0;
    for (int i = 0; i < card->have_count; i++) {
        if (contains(card->winning, card->winning_count, card->have[i])) {
            matches++;
        }
    }
    return matches;
}

// Parse a line into a Card structure
void parse_card(char *line, Card *card) {
    card->winning_count = 0;
    card->have_count = 0;

    // Skip "Card N:" prefix
    char *colon = strchr(line, ':');
    if (!colon) return;
    char *ptr = colon + 1;

    // Find the pipe separator
    char *pipe = strchr(ptr, '|');
    if (!pipe) return;

    // Parse winning numbers (before pipe)
    char winning_str[MAX_LINE];
    int winning_len = pipe - ptr;
    strncpy(winning_str, ptr, winning_len);
    winning_str[winning_len] = '\0';

    char *token = strtok(winning_str, " ");
    while (token != NULL) {
        card->winning[card->winning_count++] = atoi(token);
        token = strtok(NULL, " ");
    }

    // Parse "have" numbers (after pipe)
    char have_str[MAX_LINE];
    strcpy(have_str, pipe + 1);

    token = strtok(have_str, " \n");
    while (token != NULL) {
        card->have[card->have_count++] = atoi(token);
        token = strtok(NULL, " \n");
    }
}

int part1(void) {
    int total = 0;

    for (int i = 0; i < card_count; i++) {
        int matches = count_matches(&cards[i]);
        if (matches > 0) {
            total += 1 << (matches - 1);  // 2^(matches-1)
        }
    }

    return total;
}

int part2(void) {
    // Track how many copies of each card we have
    int copies[MAX_CARDS];
    for (int i = 0; i < card_count; i++) {
        copies[i] = 1;  // Start with 1 original of each card
    }

    // Process each card
    for (int i = 0; i < card_count; i++) {
        int matches = count_matches(&cards[i]);

        // Win copies of the next 'matches' cards
        for (int j = i + 1; j < i + 1 + matches && j < card_count; j++) {
            copies[j] += copies[i];
        }
    }

    // Sum all copies
    int total = 0;
    for (int i = 0; i < card_count; i++) {
        total += copies[i];
    }

    return total;
}

int main(int argc, char *argv[]) {
    const char *filename = (argc > 1) ? argv[1] : "../input.txt";
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Error opening file");
        return 1;
    }

    // Read and parse all cards
    char line[MAX_LINE];
    while (fgets(line, sizeof(line), f) && card_count < MAX_CARDS) {
        if (strlen(line) > 1) {  // Skip empty lines
            parse_card(line, &cards[card_count]);
            card_count++;
        }
    }
    fclose(f);

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
