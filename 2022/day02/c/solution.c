#include <stdio.h>
#include <stdlib.h>

int part1(FILE *f) {
    // Shape scores: X(Rock)=1, Y(Paper)=2, Z(Scissors)=3
    // Outcome lookup table: outcomes[opponent][me]
    // opponent: A=0, B=1, C=2 (Rock, Paper, Scissors)
    // me: X=0, Y=1, Z=2 (Rock, Paper, Scissors)
    // Values: 0=loss, 3=draw, 6=win
    int outcomes[3][3] = {
        {3, 6, 0},  // A: Rock vs X(Rock)=draw, Y(Paper)=win, Z(Scissors)=loss
        {0, 3, 6},  // B: Paper vs X(Rock)=loss, Y(Paper)=draw, Z(Scissors)=win
        {6, 0, 3}   // C: Scissors vs X(Rock)=win, Y(Paper)=loss, Z(Scissors)=draw
    };

    int total = 0;
    char line[16];

    while (fgets(line, sizeof(line), f)) {
        if (line[0] == '\0' || line[0] == '\n') continue;

        int opp = line[0] - 'A';  // 0, 1, or 2
        int me = line[2] - 'X';   // 0, 1, or 2

        int shape_score = me + 1;  // Rock=1, Paper=2, Scissors=3
        total += shape_score + outcomes[opp][me];
    }

    return total;
}

int part2(FILE *f) {
    // What shape to play given opponent and desired outcome
    // choices[opponent][outcome] = shape to play (1=Rock, 2=Paper, 3=Scissors)
    // opponent: A=0, B=1, C=2
    // outcome: X=0(lose), Y=1(draw), Z=2(win)
    int choices[3][3] = {
        {3, 1, 2},  // A(Rock): lose->Scissors(3), draw->Rock(1), win->Paper(2)
        {1, 2, 3},  // B(Paper): lose->Rock(1), draw->Paper(2), win->Scissors(3)
        {2, 3, 1}   // C(Scissors): lose->Paper(2), draw->Scissors(3), win->Rock(1)
    };

    // Outcome scores: X=0(lose), Y=3(draw), Z=6(win)
    int outcome_scores[3] = {0, 3, 6};

    int total = 0;
    char line[16];

    while (fgets(line, sizeof(line), f)) {
        if (line[0] == '\0' || line[0] == '\n') continue;

        int opp = line[0] - 'A';      // 0, 1, or 2
        int outcome = line[2] - 'X';  // 0, 1, or 2

        total += choices[opp][outcome] + outcome_scores[outcome];
    }

    return total;
}

int main() {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Could not open input file");
        return 1;
    }

    int result1 = part1(f);

    // Rewind for part 2
    rewind(f);

    int result2 = part2(f);

    fclose(f);

    printf("Part 1: %d\n", result1);
    printf("Part 2: %d\n", result2);

    return 0;
}
