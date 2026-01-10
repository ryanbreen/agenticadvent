const std = @import("std");

const Direction = enum { forward, down, up };

const Command = struct {
    direction: Direction,
    value: i64,
};

fn parseInput(allocator: std.mem.Allocator) !struct { items: []Command, allocator: std.mem.Allocator } {
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var commands = std.ArrayListUnmanaged(Command){};

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.splitScalar(u8, line, ' ');
        const dir_str = parts.next() orelse continue;
        const val_str = parts.next() orelse continue;

        const direction: Direction = if (std.mem.eql(u8, dir_str, "forward"))
            .forward
        else if (std.mem.eql(u8, dir_str, "down"))
            .down
        else if (std.mem.eql(u8, dir_str, "up"))
            .up
        else
            continue;

        const value = std.fmt.parseInt(i64, val_str, 10) catch continue;

        try commands.append(allocator, Command{ .direction = direction, .value = value });
    }

    return .{ .items = commands.items, .allocator = allocator };
}

fn part1(commands: []const Command) i64 {
    var horizontal: i64 = 0;
    var depth: i64 = 0;

    for (commands) |cmd| {
        switch (cmd.direction) {
            .forward => horizontal += cmd.value,
            .down => depth += cmd.value,
            .up => depth -= cmd.value,
        }
    }

    return horizontal * depth;
}

fn part2(commands: []const Command) i64 {
    var horizontal: i64 = 0;
    var depth: i64 = 0;
    var aim: i64 = 0;

    for (commands) |cmd| {
        switch (cmd.direction) {
            .forward => {
                horizontal += cmd.value;
                depth += aim * cmd.value;
            },
            .down => aim += cmd.value,
            .up => aim -= cmd.value,
        }
    }

    return horizontal * depth;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const result = try parseInput(allocator);
    defer allocator.free(result.items);

    std.debug.print("Part 1: {d}\n", .{part1(result.items)});
    std.debug.print("Part 2: {d}\n", .{part2(result.items)});
}
