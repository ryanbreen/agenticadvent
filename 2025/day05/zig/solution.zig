const std = @import("std");

const Range = struct {
    start: u64,
    end: u64,
};

fn parseInput(allocator: std.mem.Allocator, content: []const u8) !struct {
    ranges: std.ArrayList(Range),
    ingredient_ids: std.ArrayList(u64),
} {
    var ranges: std.ArrayList(Range) = .{};
    var ingredient_ids: std.ArrayList(u64) = .{};

    var lines = std.mem.splitScalar(u8, content, '\n');
    var in_ranges = true;

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) {
            in_ranges = false;
            continue;
        }

        if (in_ranges) {
            // Parse range (e.g., "3-5")
            var parts = std.mem.splitScalar(u8, trimmed, '-');
            const start_str = parts.next() orelse continue;
            const end_str = parts.next() orelse continue;

            const start = try std.fmt.parseInt(u64, start_str, 10);
            const end = try std.fmt.parseInt(u64, end_str, 10);

            try ranges.append(allocator, Range{ .start = start, .end = end });
        } else {
            // Parse ingredient ID
            const id = try std.fmt.parseInt(u64, trimmed, 10);
            try ingredient_ids.append(allocator, id);
        }
    }

    return .{
        .ranges = ranges,
        .ingredient_ids = ingredient_ids,
    };
}

fn part1(allocator: std.mem.Allocator, content: []const u8) !u64 {
    var parsed = try parseInput(allocator, content);
    defer parsed.ranges.deinit(allocator);
    defer parsed.ingredient_ids.deinit(allocator);

    var fresh_count: u64 = 0;

    for (parsed.ingredient_ids.items) |ingredient_id| {
        for (parsed.ranges.items) |range| {
            if (ingredient_id >= range.start and ingredient_id <= range.end) {
                fresh_count += 1;
                break; // Found a match, no need to check other ranges
            }
        }
    }

    return fresh_count;
}

fn lessThan(_: void, lhs: Range, rhs: Range) bool {
    return lhs.start < rhs.start;
}

fn part2(allocator: std.mem.Allocator, content: []const u8) !u64 {
    var parsed = try parseInput(allocator, content);
    defer parsed.ingredient_ids.deinit(allocator);

    // Sort ranges by start position
    std.mem.sort(Range, parsed.ranges.items, {}, lessThan);

    // Merge overlapping ranges
    var merged: std.ArrayList(Range) = .{};
    defer merged.deinit(allocator);

    for (parsed.ranges.items) |range| {
        if (merged.items.len > 0 and range.start <= merged.items[merged.items.len - 1].end + 1) {
            // Overlapping or adjacent - merge with the last range
            const last_idx = merged.items.len - 1;
            merged.items[last_idx].end = @max(merged.items[last_idx].end, range.end);
        } else {
            // No overlap - add as new range
            try merged.append(allocator, range);
        }
    }

    parsed.ranges.deinit(allocator);

    // Count total unique IDs covered by merged ranges
    var total_count: u64 = 0;
    for (merged.items) |range| {
        total_count += (range.end - range.start + 1);
    }

    return total_count;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(content);

    const p1 = try part1(allocator, content);
    std.debug.print("Part 1: {d}\n", .{p1});

    const p2 = try part2(allocator, content);
    std.debug.print("Part 2: {d}\n", .{p2});
}
