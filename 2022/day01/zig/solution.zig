const std = @import("std");

fn compareDesc(_: void, a: u64, b: u64) bool {
    return a > b;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    // Parse input and calculate elf totals
    var elves: std.ArrayList(u64) = .{};
    defer elves.deinit(allocator);

    var current_total: u64 = 0;
    var lines = std.mem.splitScalar(u8, buffer, '\n');

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \r");
        if (trimmed.len == 0) {
            // Blank line - end of current elf's inventory
            if (current_total > 0) {
                try elves.append(allocator, current_total);
                current_total = 0;
            }
        } else {
            // Parse calorie value and add to current elf's total
            const calories = std.fmt.parseInt(u64, trimmed, 10) catch continue;
            current_total += calories;
        }
    }

    // Don't forget the last elf if file doesn't end with blank line
    if (current_total > 0) {
        try elves.append(allocator, current_total);
    }

    // Part 1: Find maximum
    var max_calories: u64 = 0;
    for (elves.items) |total| {
        if (total > max_calories) {
            max_calories = total;
        }
    }

    // Part 2: Find sum of top 3
    // Sort in descending order
    std.mem.sort(u64, elves.items, {}, compareDesc);

    var top_three_sum: u64 = 0;
    const count = @min(3, elves.items.len);
    for (elves.items[0..count]) |total| {
        top_three_sum += total;
    }

    std.debug.print("Part 1: {d}\n", .{max_calories});
    std.debug.print("Part 2: {d}\n", .{top_three_sum});
}
