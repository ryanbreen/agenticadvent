const std = @import("std");
const Allocator = std.mem.Allocator;

const IndexedValue = struct {
    orig_idx: usize,
    value: i64,
};

fn parseNumbers(allocator: Allocator, input: []const u8) !std.ArrayList(i64) {
    var numbers: std.ArrayList(i64) = .{};
    var lines = std.mem.splitScalar(u8, input, '\n');

    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const num = try std.fmt.parseInt(i64, line, 10);
        try numbers.append(allocator, num);
    }

    return numbers;
}

fn mix(allocator: Allocator, numbers: []const i64, times: usize) !std.ArrayList(i64) {
    const n = numbers.len;

    // Create indexed list of (original_index, value)
    var indexed: std.ArrayList(IndexedValue) = .{};
    defer indexed.deinit(allocator);

    for (numbers, 0..) |val, i| {
        try indexed.append(allocator, .{ .orig_idx = i, .value = val });
    }

    // Mix the specified number of times
    for (0..times) |_| {
        for (0..n) |orig_idx| {
            // Find current position of element with this original index
            var curr_pos: usize = 0;
            for (indexed.items, 0..) |item, pos| {
                if (item.orig_idx == orig_idx) {
                    curr_pos = pos;
                    break;
                }
            }

            const val = indexed.items[curr_pos].value;

            // Remove from current position
            _ = indexed.orderedRemove(curr_pos);

            // Calculate new position (modulo n-1 because we removed the element)
            const mod_val: i64 = @intCast(n - 1);
            var new_pos_i64: i64 = @intCast(curr_pos);
            new_pos_i64 += val;
            new_pos_i64 = @mod(new_pos_i64, mod_val);
            const new_pos: usize = @intCast(new_pos_i64);

            // Insert at new position
            try indexed.insert(allocator, new_pos, .{ .orig_idx = orig_idx, .value = val });
        }
    }

    // Extract values from indexed list
    var result: std.ArrayList(i64) = .{};
    for (indexed.items) |item| {
        try result.append(allocator, item.value);
    }

    return result;
}

fn groveCoordinates(mixed: []const i64) i64 {
    const n = mixed.len;

    // Find index of 0
    var zero_idx: usize = 0;
    for (mixed, 0..) |val, i| {
        if (val == 0) {
            zero_idx = i;
            break;
        }
    }

    // Sum values at offsets 1000, 2000, 3000 from zero
    const offsets = [_]usize{ 1000, 2000, 3000 };
    var sum: i64 = 0;
    for (offsets) |offset| {
        const idx = (zero_idx + offset) % n;
        sum += mixed[idx];
    }

    return sum;
}

fn part1(allocator: Allocator, numbers: []const i64) !i64 {
    var mixed = try mix(allocator, numbers, 1);
    defer mixed.deinit(allocator);
    return groveCoordinates(mixed.items);
}

fn part2(allocator: Allocator, numbers: []const i64) !i64 {
    const decryption_key: i64 = 811589153;

    // Multiply all numbers by decryption key
    var scaled: std.ArrayList(i64) = .{};
    defer scaled.deinit(allocator);

    for (numbers) |num| {
        try scaled.append(allocator, num * decryption_key);
    }

    var mixed = try mix(allocator, scaled.items, 10);
    defer mixed.deinit(allocator);
    return groveCoordinates(mixed.items);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    const trimmed = std.mem.trim(u8, buffer, &[_]u8{ '\n', '\r', ' ' });

    var numbers = try parseNumbers(allocator, trimmed);
    defer numbers.deinit(allocator);

    const answer1 = try part1(allocator, numbers.items);
    const answer2 = try part2(allocator, numbers.items);

    std.debug.print("Part 1: {d}\n", .{answer1});
    std.debug.print("Part 2: {d}\n", .{answer2});
}
