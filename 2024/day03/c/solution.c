#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define MAX_FILE_SIZE 1000000

typedef struct {
    size_t pos;
    char type;  // 'm' for mul, 'd' for do, 'n' for don't
    int x;
    int y;
} Event;

// Parse a number (1-3 digits) starting at position i
// Returns the number, or -1 if invalid
// Updates *pos to the position after the number
int parse_number(const char *str, size_t *pos) {
    size_t start = *pos;
    int num = 0;
    int digits = 0;

    while (isdigit(str[*pos]) && digits < 3) {
        num = num * 10 + (str[*pos] - '0');
        (*pos)++;
        digits++;
    }

    if (digits == 0) return -1;
    return num;
}

// Try to parse mul(X,Y) at position i
// Returns true if successful, fills x and y
bool parse_mul(const char *str, size_t i, int *x, int *y, size_t *end_pos) {
    if (strncmp(&str[i], "mul(", 4) != 0) return false;

    size_t pos = i + 4;

    // Parse first number
    *x = parse_number(str, &pos);
    if (*x == -1) return false;

    // Expect comma
    if (str[pos] != ',') return false;
    pos++;

    // Parse second number
    *y = parse_number(str, &pos);
    if (*y == -1) return false;

    // Expect closing paren
    if (str[pos] != ')') return false;
    pos++;

    *end_pos = pos;
    return true;
}

int part1(const char *data) {
    int total = 0;
    size_t len = strlen(data);

    for (size_t i = 0; i < len; i++) {
        int x, y;
        size_t end_pos;
        if (parse_mul(data, i, &x, &y, &end_pos)) {
            total += x * y;
        }
    }

    return total;
}

int compare_events(const void *a, const void *b) {
    const Event *ea = (const Event *)a;
    const Event *eb = (const Event *)b;
    if (ea->pos < eb->pos) return -1;
    if (ea->pos > eb->pos) return 1;
    return 0;
}

int part2(const char *data) {
    size_t len = strlen(data);
    Event *events = malloc(len * sizeof(Event));
    size_t event_count = 0;

    // Find all mul instructions
    for (size_t i = 0; i < len; i++) {
        int x, y;
        size_t end_pos;
        if (parse_mul(data, i, &x, &y, &end_pos)) {
            events[event_count].pos = i;
            events[event_count].type = 'm';
            events[event_count].x = x;
            events[event_count].y = y;
            event_count++;
        }
    }

    // Find all do() instructions
    for (size_t i = 0; i < len - 3; i++) {
        if (strncmp(&data[i], "do()", 4) == 0) {
            events[event_count].pos = i;
            events[event_count].type = 'd';
            events[event_count].x = 0;
            events[event_count].y = 0;
            event_count++;
        }
    }

    // Find all don't() instructions
    for (size_t i = 0; i < len - 6; i++) {
        if (strncmp(&data[i], "don't()", 7) == 0) {
            events[event_count].pos = i;
            events[event_count].type = 'n';
            events[event_count].x = 0;
            events[event_count].y = 0;
            event_count++;
        }
    }

    // Sort events by position
    qsort(events, event_count, sizeof(Event), compare_events);

    // Process events
    int total = 0;
    bool enabled = true;

    for (size_t i = 0; i < event_count; i++) {
        switch (events[i].type) {
            case 'd':
                enabled = true;
                break;
            case 'n':
                enabled = false;
                break;
            case 'm':
                if (enabled) {
                    total += events[i].x * events[i].y;
                }
                break;
        }
    }

    free(events);
    return total;
}

int main() {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Error opening file");
        return 1;
    }

    // Read entire file into memory
    char *data = malloc(MAX_FILE_SIZE);
    size_t bytes_read = fread(data, 1, MAX_FILE_SIZE - 1, fp);
    data[bytes_read] = '\0';
    fclose(fp);

    printf("Part 1: %d\n", part1(data));
    printf("Part 2: %d\n", part2(data));

    free(data);
    return 0;
}
