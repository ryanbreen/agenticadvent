const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
};

fn sign(x: i32) i32 {
    if (x == 0) return 0;
    return if (x > 0) 1 else -1;
}

fn moveTail(head: Point, tail: Point) Point {
    const dx = head.x - tail.x;
    const dy = head.y - tail.y;

    // If adjacent or overlapping, don't move
    if (@abs(dx) <= 1 and @abs(dy) <= 1) {
        return tail;
    }

    // Move toward head
    return Point{
        .x = tail.x + sign(dx),
        .y = tail.y + sign(dy),
    };
}

fn simulateRope(moves: []const u8, rope_length: usize, allocator: std.mem.Allocator) !usize {
    var knots = try allocator.alloc(Point, rope_length);
    defer allocator.free(knots);

    for (knots) |*knot| {
        knot.* = Point{ .x = 0, .y = 0 };
    }

    var visited = std.AutoHashMap(Point, void).init(allocator);
    defer visited.deinit();

    try visited.put(knots[rope_length - 1], {});

    var lines = std.mem.splitScalar(u8, moves, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const direction = line[0];
        const count = std.fmt.parseInt(i32, std.mem.trim(u8, line[2..], " "), 10) catch continue;

        var dx: i32 = 0;
        var dy: i32 = 0;

        switch (direction) {
            'U' => dy = 1,
            'D' => dy = -1,
            'L' => dx = -1,
            'R' => dx = 1,
            else => continue,
        }

        var step: i32 = 0;
        while (step < count) : (step += 1) {
            // Move head
            knots[0].x += dx;
            knots[0].y += dy;

            // Move each subsequent knot
            for (1..rope_length) |i| {
                knots[i] = moveTail(knots[i - 1], knots[i]);
            }

            try visited.put(knots[rope_length - 1], {});
        }
    }

    return visited.count();
}

fn part1(moves: []const u8, allocator: std.mem.Allocator) !usize {
    return simulateRope(moves, 2, allocator);
}

fn part2(moves: []const u8, allocator: std.mem.Allocator) !usize {
    return simulateRope(moves, 10, allocator);
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const content = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(content);

    const moves = std.mem.trim(u8, content, "\n\r ");

    const result1 = try part1(moves, allocator);
    const result2 = try part2(moves, allocator);

    std.debug.print("Part 1: {d}\n", .{result1});
    std.debug.print("Part 2: {d}\n", .{result2});
}
