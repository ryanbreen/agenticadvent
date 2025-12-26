const std = @import("std");

const Lock = [5]u8;
const Key = [5]u8;

fn parseInput(allocator: std.mem.Allocator, text: []const u8) !struct { locks: []Lock, keys: []Key } {
    var locks: std.ArrayList([5]u8) = .{};
    defer locks.deinit(allocator);
    var keys: std.ArrayList([5]u8) = .{};
    defer keys.deinit(allocator);

    var schematic_iter = std.mem.splitSequence(u8, text, "\n\n");
    while (schematic_iter.next()) |schematic_text| {
        const schematic = std.mem.trim(u8, schematic_text, &std.ascii.whitespace);
        if (schematic.len == 0) continue;

        var lines: std.ArrayList([]const u8) = .{};
        defer lines.deinit(allocator);

        var line_iter = std.mem.splitScalar(u8, schematic, '\n');
        while (line_iter.next()) |line| {
            const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
            if (trimmed.len > 0) {
                try lines.append(allocator, trimmed);
            }
        }

        if (lines.items.len < 7) continue;

        // Check if it's a lock (top row all #) or key (top row all .)
        const is_lock = std.mem.eql(u8, lines.items[0], "#####");

        var heights: [5]u8 = undefined;

        if (is_lock) {
            // Lock: count # from top (excluding top row)
            for (0..5) |col| {
                var height: u8 = 0;
                for (1..7) |row| {
                    if (lines.items[row][col] == '#') {
                        height += 1;
                    } else {
                        break;
                    }
                }
                heights[col] = height;
            }
            try locks.append(allocator, heights);
        } else {
            // Key: count # from bottom (excluding bottom row)
            for (0..5) |col| {
                var height: u8 = 0;
                var row: usize = 5;
                while (true) : (row -= 1) {
                    if (lines.items[row][col] == '#') {
                        height += 1;
                    } else {
                        break;
                    }
                    if (row == 0) break;
                }
                heights[col] = height;
            }
            try keys.append(allocator, heights);
        }
    }

    return .{
        .locks = try allocator.dupe([5]u8, locks.items),
        .keys = try allocator.dupe([5]u8, keys.items),
    };
}

fn fits(lock: Lock, key: Key) bool {
    for (0..5) |i| {
        if (lock[i] + key[i] > 5) {
            return false;
        }
    }
    return true;
}

fn part1(locks: []const Lock, keys: []const Key) u32 {
    var count: u32 = 0;
    for (locks) |lock| {
        for (keys) |key| {
            if (fits(lock, key)) {
                count += 1;
            }
        }
    }
    return count;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "../input.txt";
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const text = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(text);

    const parsed = try parseInput(allocator, text);
    defer allocator.free(parsed.locks);
    defer allocator.free(parsed.keys);

    const answer1 = part1(parsed.locks, parsed.keys);
    std.debug.print("Part 1: {d}\n", .{answer1});
    std.debug.print("Part 2: Merry Christmas!\n", .{});
}
