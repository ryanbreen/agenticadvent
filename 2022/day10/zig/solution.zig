const std = @import("std");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const content = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(content);

    // Parse instructions and simulate CPU
    var x: i32 = 1;
    var cycle: u32 = 0;
    var part1_sum: i64 = 0;
    var screen: [6][40]u8 = undefined;

    // Initialize screen with dots
    for (&screen) |*row| {
        @memset(row, '.');
    }

    var lines = std.mem.tokenizeScalar(u8, content, '\n');

    while (lines.next()) |line| {
        if (std.mem.eql(u8, line, "noop")) {
            // noop: 1 cycle
            cycle += 1;
            processCycle(cycle, x, &part1_sum, &screen);
        } else if (std.mem.startsWith(u8, line, "addx ")) {
            // addx V: 2 cycles, then X += V
            const v = std.fmt.parseInt(i32, line[5..], 10) catch continue;

            cycle += 1;
            processCycle(cycle, x, &part1_sum, &screen);

            cycle += 1;
            processCycle(cycle, x, &part1_sum, &screen);

            x += v;
        }
    }

    // Output results
    std.debug.print("Part 1: {d}\n", .{part1_sum});
    std.debug.print("Part 2:\n", .{});
    for (screen) |row| {
        std.debug.print("{s}\n", .{row});
    }
}

fn processCycle(cycle: u32, x: i32, part1_sum: *i64, screen: *[6][40]u8) void {
    // Part 1: Check for target cycles
    if (cycle == 20 or cycle == 60 or cycle == 100 or cycle == 140 or cycle == 180 or cycle == 220) {
        part1_sum.* += @as(i64, cycle) * @as(i64, x);
    }

    // Part 2: Draw pixel
    const pos: i32 = @intCast((cycle - 1) % 40);
    const row: usize = @intCast((cycle - 1) / 40);

    if (row < 6) {
        // Sprite is 3 pixels wide centered at X
        if (pos >= x - 1 and pos <= x + 1) {
            screen[row][@intCast(pos)] = '#';
        }
    }
}
