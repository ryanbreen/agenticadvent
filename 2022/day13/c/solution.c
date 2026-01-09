#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Packet element can be an integer or a list
typedef struct Element {
    int is_list;  // 0 = integer, 1 = list
    int value;    // integer value (if is_list == 0)
    struct Element **items;  // array of child elements (if is_list == 1)
    int count;    // number of items in list
    int capacity; // allocated capacity
} Element;

// Create a new integer element
Element *new_int(int value) {
    Element *e = malloc(sizeof(Element));
    e->is_list = 0;
    e->value = value;
    e->items = NULL;
    e->count = 0;
    e->capacity = 0;
    return e;
}

// Create a new list element
Element *new_list(void) {
    Element *e = malloc(sizeof(Element));
    e->is_list = 1;
    e->value = 0;
    e->count = 0;
    e->capacity = 4;
    e->items = malloc(e->capacity * sizeof(Element *));
    return e;
}

// Add an item to a list
void list_add(Element *list, Element *item) {
    if (list->count >= list->capacity) {
        list->capacity *= 2;
        list->items = realloc(list->items, list->capacity * sizeof(Element *));
    }
    list->items[list->count++] = item;
}

// Free an element recursively
void free_element(Element *e) {
    if (e == NULL) return;
    if (e->is_list) {
        for (int i = 0; i < e->count; i++) {
            free_element(e->items[i]);
        }
        free(e->items);
    }
    free(e);
}

// Parse a packet from a string, returns pointer to next position
Element *parse(const char **s) {
    while (**s == ' ') (*s)++;

    if (**s == '[') {
        (*s)++;  // consume '['
        Element *list = new_list();

        while (**s != ']') {
            while (**s == ' ') (*s)++;
            if (**s == ']') break;

            Element *item = parse(s);
            list_add(list, item);

            while (**s == ' ') (*s)++;
            if (**s == ',') (*s)++;
        }
        (*s)++;  // consume ']'
        return list;
    } else if (isdigit(**s)) {
        int value = 0;
        while (isdigit(**s)) {
            value = value * 10 + (**s - '0');
            (*s)++;
        }
        return new_int(value);
    }

    return NULL;
}

// Compare two elements
// Returns: -1 if left < right, 1 if left > right, 0 if equal
int compare(Element *left, Element *right) {
    // Both integers
    if (!left->is_list && !right->is_list) {
        if (left->value < right->value) return -1;
        if (left->value > right->value) return 1;
        return 0;
    }

    // Both lists
    if (left->is_list && right->is_list) {
        int min_len = left->count < right->count ? left->count : right->count;
        for (int i = 0; i < min_len; i++) {
            int result = compare(left->items[i], right->items[i]);
            if (result != 0) return result;
        }
        if (left->count < right->count) return -1;
        if (left->count > right->count) return 1;
        return 0;
    }

    // Mixed types - convert integer to list
    if (!left->is_list) {
        // left is int, right is list
        Element *temp = new_list();
        list_add(temp, new_int(left->value));
        int result = compare(temp, right);
        free_element(temp);
        return result;
    } else {
        // left is list, right is int
        Element *temp = new_list();
        list_add(temp, new_int(right->value));
        int result = compare(left, temp);
        free_element(temp);
        return result;
    }
}

// Create divider packet [[n]]
Element *create_divider(int n) {
    Element *outer = new_list();
    Element *inner = new_list();
    list_add(inner, new_int(n));
    list_add(outer, inner);
    return outer;
}

// Comparison function for qsort
int cmp_for_sort(const void *a, const void *b) {
    Element *left = *(Element **)a;
    Element *right = *(Element **)b;
    return compare(left, right);
}

// Check if packet matches divider [[n]]
int is_divider(Element *e, int n) {
    if (!e->is_list || e->count != 1) return 0;
    Element *inner = e->items[0];
    if (!inner->is_list || inner->count != 1) return 0;
    Element *val = inner->items[0];
    if (val->is_list) return 0;
    return val->value == n;
}

int main(void) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Failed to open input.txt");
        return 1;
    }

    // Read entire file
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *content = malloc(size + 1);
    fread(content, 1, size, f);
    content[size] = '\0';
    fclose(f);

    // Part 1: Parse pairs and sum indices of correct order
    // Part 2: Collect all packets for sorting

    Element **all_packets = malloc(500 * sizeof(Element *));
    int packet_count = 0;

    int part1_sum = 0;
    int pair_index = 1;

    char *line = strtok(content, "\n");
    while (line != NULL) {
        // Skip empty lines
        while (line != NULL && strlen(line) == 0) {
            line = strtok(NULL, "\n");
        }
        if (line == NULL) break;

        // Parse first packet of pair
        const char *s1 = line;
        Element *left = parse(&s1);
        all_packets[packet_count++] = left;

        line = strtok(NULL, "\n");
        if (line == NULL) break;

        // Parse second packet of pair
        const char *s2 = line;
        Element *right = parse(&s2);
        all_packets[packet_count++] = right;

        // Compare for Part 1
        if (compare(left, right) == -1) {
            part1_sum += pair_index;
        }

        pair_index++;
        line = strtok(NULL, "\n");
    }

    printf("Part 1: %d\n", part1_sum);

    // Part 2: Add dividers and sort
    Element *div2 = create_divider(2);
    Element *div6 = create_divider(6);
    all_packets[packet_count++] = div2;
    all_packets[packet_count++] = div6;

    qsort(all_packets, packet_count, sizeof(Element *), cmp_for_sort);

    int pos2 = 0, pos6 = 0;
    for (int i = 0; i < packet_count; i++) {
        if (is_divider(all_packets[i], 2)) pos2 = i + 1;
        if (is_divider(all_packets[i], 6)) pos6 = i + 1;
    }

    printf("Part 2: %d\n", pos2 * pos6);

    // Cleanup
    for (int i = 0; i < packet_count; i++) {
        free_element(all_packets[i]);
    }
    free(all_packets);
    free(content);

    return 0;
}
