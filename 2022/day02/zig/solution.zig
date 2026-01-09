const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var part1_total: i32 = 0;
    var part2_total: i32 = 0;

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len < 3) continue;

        const opp = line[0]; // A, B, or C
        const me = line[2]; // X, Y, or Z

        // Part 1: X=Rock(1), Y=Paper(2), Z=Scissors(3)
        const shape_score: i32 = switch (me) {
            'X' => 1,
            'Y' => 2,
            'Z' => 3,
            else => 0,
        };

        // Part 1 outcome: 0=loss, 3=draw, 6=win
        const outcome_score: i32 = switch (opp) {
            'A' => switch (me) { // Rock
                'X' => 3, // Rock vs Rock = draw
                'Y' => 6, // Rock vs Paper = win
                'Z' => 0, // Rock vs Scissors = loss
                else => 0,
            },
            'B' => switch (me) { // Paper
                'X' => 0, // Paper vs Rock = loss
                'Y' => 3, // Paper vs Paper = draw
                'Z' => 6, // Paper vs Scissors = win
                else => 0,
            },
            'C' => switch (me) { // Scissors
                'X' => 6, // Scissors vs Rock = win
                'Y' => 0, // Scissors vs Paper = loss
                'Z' => 3, // Scissors vs Scissors = draw
                else => 0,
            },
            else => 0,
        };

        part1_total += shape_score + outcome_score;

        // Part 2: X=lose(0), Y=draw(3), Z=win(6)
        const p2_outcome: i32 = switch (me) {
            'X' => 0, // lose
            'Y' => 3, // draw
            'Z' => 6, // win
            else => 0,
        };

        // Determine shape to play (1=Rock, 2=Paper, 3=Scissors)
        const p2_shape: i32 = switch (opp) {
            'A' => switch (me) { // Rock
                'X' => 3, // need to lose -> Scissors
                'Y' => 1, // need to draw -> Rock
                'Z' => 2, // need to win -> Paper
                else => 0,
            },
            'B' => switch (me) { // Paper
                'X' => 1, // need to lose -> Rock
                'Y' => 2, // need to draw -> Paper
                'Z' => 3, // need to win -> Scissors
                else => 0,
            },
            'C' => switch (me) { // Scissors
                'X' => 2, // need to lose -> Paper
                'Y' => 3, // need to draw -> Scissors
                'Z' => 1, // need to win -> Rock
                else => 0,
            },
            else => 0,
        };

        part2_total += p2_shape + p2_outcome;
    }

    std.debug.print("Part 1: {d}\n", .{part1_total});
    std.debug.print("Part 2: {d}\n", .{part2_total});
}
