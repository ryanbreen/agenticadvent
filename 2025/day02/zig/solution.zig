const std = @import("std");

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

    // Trim trailing whitespace
    const input = std.mem.trim(u8, buffer, &std.ascii.whitespace);

    const part1_result = try part1(allocator, input);
    const part2_result = try part2(allocator, input);

    std.debug.print("Part 1: {d}\n", .{part1_result});
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn isInvalidIdPart1(num: i64) bool {
    // Check if a number is invalid (a pattern repeated twice)
    var buffer: [32]u8 = undefined;
    const s = std.fmt.bufPrint(&buffer, "{d}", .{num}) catch return false;
    const length = s.len;

    // Must have even length to be repeated twice
    if (length % 2 != 0) return false;

    // Check if it starts with 0 (leading zeros not allowed)
    if (s[0] == '0') return false;

    // Split in half and check if both halves are identical
    const mid = length / 2;
    const first_half = s[0..mid];
    const second_half = s[mid..];

    return std.mem.eql(u8, first_half, second_half);
}

fn isInvalidIdPart2(num: i64) bool {
    // Check if a number is invalid (a pattern repeated at least twice)
    var buffer: [32]u8 = undefined;
    const s = std.fmt.bufPrint(&buffer, "{d}", .{num}) catch return false;
    const length = s.len;

    // Check if it starts with 0 (leading zeros not allowed)
    if (s[0] == '0') return false;

    // Try all possible pattern lengths from 1 to length/2
    var pattern_length: usize = 1;
    while (pattern_length <= length / 2) : (pattern_length += 1) {
        // Check if the string length is divisible by pattern_length
        if (length % pattern_length == 0) {
            const pattern = s[0..pattern_length];
            const repetitions = length / pattern_length;

            // Check if repeating the pattern gives us the original string
            var match = true;
            var i: usize = 0;
            while (i < repetitions) : (i += 1) {
                const start = i * pattern_length;
                const end = start + pattern_length;
                if (!std.mem.eql(u8, pattern, s[start..end])) {
                    match = false;
                    break;
                }
            }
            if (match) return true;
        }
    }

    return false;
}

const Range = struct { start: i64, end: i64 };

fn part1(allocator: std.mem.Allocator, input: []const u8) !i64 {
    // Parse ranges from input
    var ranges: std.ArrayList(Range) = .empty;
    defer ranges.deinit(allocator);

    var parts = std.mem.splitScalar(u8, input, ',');
    while (parts.next()) |part| {
        const trimmed = std.mem.trim(u8, part, &std.ascii.whitespace);
        if (trimmed.len == 0) continue;

        // Find the dash separator
        if (std.mem.indexOf(u8, trimmed, "-")) |dash_idx| {
            const start_str = trimmed[0..dash_idx];
            const end_str = trimmed[dash_idx + 1 ..];

            const start = try std.fmt.parseInt(i64, start_str, 10);
            const end = try std.fmt.parseInt(i64, end_str, 10);

            try ranges.append(allocator, .{ .start = start, .end = end });
        }
    }

    var total: i64 = 0;
    for (ranges.items) |range| {
        var num = range.start;
        while (num <= range.end) : (num += 1) {
            if (isInvalidIdPart1(num)) {
                total += num;
            }
        }
    }

    return total;
}

fn part2(allocator: std.mem.Allocator, input: []const u8) !i64 {
    // Parse ranges from input
    var ranges: std.ArrayList(Range) = .empty;
    defer ranges.deinit(allocator);

    var parts = std.mem.splitScalar(u8, input, ',');
    while (parts.next()) |part| {
        const trimmed = std.mem.trim(u8, part, &std.ascii.whitespace);
        if (trimmed.len == 0) continue;

        // Find the dash separator
        if (std.mem.indexOf(u8, trimmed, "-")) |dash_idx| {
            const start_str = trimmed[0..dash_idx];
            const end_str = trimmed[dash_idx + 1 ..];

            const start = try std.fmt.parseInt(i64, start_str, 10);
            const end = try std.fmt.parseInt(i64, end_str, 10);

            try ranges.append(allocator, .{ .start = start, .end = end });
        }
    }

    var total: i64 = 0;
    for (ranges.items) |range| {
        var num = range.start;
        while (num <= range.end) : (num += 1) {
            if (isInvalidIdPart2(num)) {
                total += num;
            }
        }
    }

    return total;
}
