#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 100
#define MAX_MOVES 20000
#define BOX_HASH_SIZE 512

typedef struct {
    int r, c;
} Pos;

typedef struct {
    char grid[MAX_GRID_SIZE][MAX_GRID_SIZE * 2];
    int rows;
    int cols;
} Grid;

typedef struct {
    int r, c;
    int generation;
} BoxHashEntry;

typedef struct {
    BoxHashEntry hash[BOX_HASH_SIZE];
    Pos boxes[10000];
    int count;
    int generation;
} BoxSet;

void read_input(const char* filename, Grid* grid, char* moves, int* move_count) {
    FILE* f = fopen(filename, "r");
    if (!f) {
        perror("Error opening file");
        exit(1);
    }

    char line[512];
    grid->rows = 0;
    grid->cols = 0;

    // Read grid
    while (fgets(line, sizeof(line), f)) {
        if (line[0] == '\n') break;
        int len = strlen(line);
        if (line[len-1] == '\n') line[len-1] = '\0';

        strcpy(grid->grid[grid->rows], line);
        if (grid->cols == 0) grid->cols = strlen(line);
        grid->rows++;
    }

    // Read moves
    *move_count = 0;
    while (fgets(line, sizeof(line), f)) {
        for (int i = 0; line[i] && line[i] != '\n'; i++) {
            moves[(*move_count)++] = line[i];
        }
    }
    moves[*move_count] = '\0';

    fclose(f);
}

Pos find_robot(const Grid* grid) {
    for (int r = 0; r < grid->rows; r++) {
        for (int c = 0; c < grid->cols; c++) {
            if (grid->grid[r][c] == '@') {
                return (Pos){r, c};
            }
        }
    }
    return (Pos){-1, -1};
}

void get_direction(char dir, int* dr, int* dc) {
    *dr = *dc = 0;
    if (dir == '<') *dc = -1;
    else if (dir == '>') *dc = 1;
    else if (dir == '^') *dr = -1;
    else if (dir == 'v') *dr = 1;
}

Pos move_robot(Grid* grid, Pos robot_pos, char direction) {
    int dr, dc;
    get_direction(direction, &dr, &dc);

    int nr = robot_pos.r + dr;
    int nc = robot_pos.c + dc;

    if (grid->grid[nr][nc] == '#') {
        return robot_pos;
    }

    if (grid->grid[nr][nc] == '.') {
        grid->grid[robot_pos.r][robot_pos.c] = '.';
        grid->grid[nr][nc] = '@';
        return (Pos){nr, nc};
    }

    if (grid->grid[nr][nc] == 'O') {
        int check_r = nr, check_c = nc;
        while (grid->grid[check_r][check_c] == 'O') {
            check_r += dr;
            check_c += dc;
        }

        if (grid->grid[check_r][check_c] == '#') {
            return robot_pos;
        }

        grid->grid[check_r][check_c] = 'O';
        grid->grid[robot_pos.r][robot_pos.c] = '.';
        grid->grid[nr][nc] = '@';
        return (Pos){nr, nc};
    }

    return robot_pos;
}

long calculate_gps(const Grid* grid, char box_char) {
    long total = 0;
    for (int r = 0; r < grid->rows; r++) {
        for (int c = 0; c < grid->cols; c++) {
            if (grid->grid[r][c] == box_char) {
                total += 100 * r + c;
            }
        }
    }
    return total;
}

void copy_grid(const Grid* src, Grid* dst) {
    dst->rows = src->rows;
    dst->cols = src->cols;
    for (int r = 0; r < src->rows; r++) {
        strcpy(dst->grid[r], src->grid[r]);
    }
}

long part1(const Grid* original_grid, const char* moves, int move_count) {
    Grid grid;
    copy_grid(original_grid, &grid);

    Pos robot_pos = find_robot(&grid);

    for (int i = 0; i < move_count; i++) {
        robot_pos = move_robot(&grid, robot_pos, moves[i]);
    }

    return calculate_gps(&grid, 'O');
}

void scale_grid(const Grid* src, Grid* dst) {
    dst->rows = src->rows;
    dst->cols = src->cols * 2;

    for (int r = 0; r < src->rows; r++) {
        int new_c = 0;
        for (int c = 0; c < src->cols; c++) {
            char cell = src->grid[r][c];
            if (cell == '#') {
                dst->grid[r][new_c++] = '#';
                dst->grid[r][new_c++] = '#';
            } else if (cell == 'O') {
                dst->grid[r][new_c++] = '[';
                dst->grid[r][new_c++] = ']';
            } else if (cell == '.') {
                dst->grid[r][new_c++] = '.';
                dst->grid[r][new_c++] = '.';
            } else if (cell == '@') {
                dst->grid[r][new_c++] = '@';
                dst->grid[r][new_c++] = '.';
            }
        }
        dst->grid[r][new_c] = '\0';
    }
}

static inline int hash_pos(int r, int c) {
    return ((r * 31) + c) & (BOX_HASH_SIZE - 1);
}

static inline void init_boxset(BoxSet* set) {
    static int global_generation = 1;
    set->count = 0;
    set->generation = global_generation++;
}

bool box_in_set(BoxSet* set, Pos box) {
    int h = hash_pos(box.r, box.c);
    for (int i = 0; i < BOX_HASH_SIZE; i++) {
        int idx = (h + i) & (BOX_HASH_SIZE - 1);
        if (set->hash[idx].generation != set->generation) return false;
        if (set->hash[idx].r == box.r && set->hash[idx].c == box.c) return true;
    }
    return false;
}

void add_box_to_set(BoxSet* set, Pos box) {
    int h = hash_pos(box.r, box.c);
    for (int i = 0; i < BOX_HASH_SIZE; i++) {
        int idx = (h + i) & (BOX_HASH_SIZE - 1);
        if (set->hash[idx].generation != set->generation) {
            set->hash[idx].r = box.r;
            set->hash[idx].c = box.c;
            set->hash[idx].generation = set->generation;
            set->boxes[set->count++] = box;
            return;
        }
        if (set->hash[idx].r == box.r && set->hash[idx].c == box.c) {
            return;  // Already in set
        }
    }
}

bool can_move_box_vertical(Grid* grid, int box_left_c, int r, int dr) {
    int nr = r + dr;
    int left_c = box_left_c;
    int right_c = box_left_c + 1;

    char left_target = grid->grid[nr][left_c];
    char right_target = grid->grid[nr][right_c];

    if (left_target == '#' || right_target == '#') {
        return false;
    }

    BoxSet boxes_to_check;
    init_boxset(&boxes_to_check);

    if (left_target == '[') {
        add_box_to_set(&boxes_to_check, (Pos){nr, left_c});
    } else if (left_target == ']') {
        add_box_to_set(&boxes_to_check, (Pos){nr, left_c - 1});
    }

    if (right_target == '[') {
        add_box_to_set(&boxes_to_check, (Pos){nr, right_c});
    } else if (right_target == ']') {
        add_box_to_set(&boxes_to_check, (Pos){nr, right_c - 1});
    }

    for (int i = 0; i < boxes_to_check.count; i++) {
        if (!can_move_box_vertical(grid, boxes_to_check.boxes[i].c,
                                   boxes_to_check.boxes[i].r, dr)) {
            return false;
        }
    }

    return true;
}

void collect_boxes_vertical(Grid* grid, int box_left_c, int r, int dr, BoxSet* collected) {
    Pos box = {r, box_left_c};
    add_box_to_set(collected, box);

    int nr = r + dr;
    int left_c = box_left_c;
    int right_c = box_left_c + 1;

    char left_target = grid->grid[nr][left_c];
    char right_target = grid->grid[nr][right_c];

    BoxSet boxes_to_check;
    init_boxset(&boxes_to_check);

    if (left_target == '[') {
        add_box_to_set(&boxes_to_check, (Pos){nr, left_c});
    } else if (left_target == ']') {
        add_box_to_set(&boxes_to_check, (Pos){nr, left_c - 1});
    }

    if (right_target == '[') {
        add_box_to_set(&boxes_to_check, (Pos){nr, right_c});
    } else if (right_target == ']') {
        add_box_to_set(&boxes_to_check, (Pos){nr, right_c - 1});
    }

    for (int i = 0; i < boxes_to_check.count; i++) {
        if (!box_in_set(collected, boxes_to_check.boxes[i])) {
            collect_boxes_vertical(grid, boxes_to_check.boxes[i].c,
                                  boxes_to_check.boxes[i].r, dr, collected);
        }
    }
}

int compare_boxes_desc(const void* a, const void* b) {
    return ((Pos*)b)->r - ((Pos*)a)->r;
}

int compare_boxes_asc(const void* a, const void* b) {
    return ((Pos*)a)->r - ((Pos*)b)->r;
}

Pos move_robot_wide(Grid* grid, Pos robot_pos, char direction) {
    int dr, dc;
    get_direction(direction, &dr, &dc);

    int r = robot_pos.r;
    int c = robot_pos.c;
    int nr = r + dr;
    int nc = c + dc;

    char target = grid->grid[nr][nc];

    if (target == '#') {
        return robot_pos;
    }

    if (target == '.') {
        grid->grid[r][c] = '.';
        grid->grid[nr][nc] = '@';
        return (Pos){nr, nc};
    }

    if (target == '[' || target == ']') {
        if (dc != 0) {  // Horizontal movement
            int check_c = nc;
            while (grid->grid[r][check_c] == '[' || grid->grid[r][check_c] == ']') {
                check_c += dc;
            }

            if (grid->grid[r][check_c] == '#') {
                return robot_pos;
            }

            // Shift all boxes
            if (dc > 0) {  // Moving right
                for (int col = check_c; col > nc; col--) {
                    grid->grid[r][col] = grid->grid[r][col - 1];
                }
            } else {  // Moving left
                for (int col = check_c; col < nc; col++) {
                    grid->grid[r][col] = grid->grid[r][col + 1];
                }
            }

            grid->grid[r][c] = '.';
            grid->grid[nr][nc] = '@';
            return (Pos){nr, nc};

        } else {  // Vertical movement
            int box_left_c = (target == '[') ? nc : nc - 1;

            if (!can_move_box_vertical(grid, box_left_c, nr, dr)) {
                return robot_pos;
            }

            BoxSet boxes_to_move;
            init_boxset(&boxes_to_move);
            collect_boxes_vertical(grid, box_left_c, nr, dr, &boxes_to_move);

            // Sort boxes by row
            if (dr > 0) {
                qsort(boxes_to_move.boxes, boxes_to_move.count, sizeof(Pos), compare_boxes_desc);
            } else {
                qsort(boxes_to_move.boxes, boxes_to_move.count, sizeof(Pos), compare_boxes_asc);
            }

            // Move all boxes
            for (int i = 0; i < boxes_to_move.count; i++) {
                int box_r = boxes_to_move.boxes[i].r;
                int box_c = boxes_to_move.boxes[i].c;
                grid->grid[box_r][box_c] = '.';
                grid->grid[box_r][box_c + 1] = '.';
                grid->grid[box_r + dr][box_c] = '[';
                grid->grid[box_r + dr][box_c + 1] = ']';
            }

            // Move robot
            grid->grid[r][c] = '.';
            grid->grid[nr][nc] = '@';
            return (Pos){nr, nc};
        }
    }

    return robot_pos;
}

long part2(const Grid* original_grid, const char* moves, int move_count) {
    Grid scaled_grid;
    scale_grid(original_grid, &scaled_grid);

    Pos robot_pos = find_robot(&scaled_grid);

    for (int i = 0; i < move_count; i++) {
        robot_pos = move_robot_wide(&scaled_grid, robot_pos, moves[i]);
    }

    return calculate_gps(&scaled_grid, '[');
}

int main() {
    Grid* grid = malloc(sizeof(Grid));
    char* moves = malloc(MAX_MOVES);
    int move_count;

    read_input("../input.txt", grid, moves, &move_count);

    printf("Part 1: %ld\n", part1(grid, moves, move_count));
    printf("Part 2: %ld\n", part2(grid, moves, move_count));

    free(grid);
    free(moves);
    return 0;
}
