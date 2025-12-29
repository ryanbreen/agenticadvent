const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file at runtime
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    const trimmed = std.mem.trim(u8, input, &std.ascii.whitespace);

    var part1_sum: i64 = 0;
    var part2_sum: i64 = 0;

    var lines = std.mem.splitScalar(u8, trimmed, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var numbers: std.ArrayListUnmanaged(i64) = .empty;
        defer numbers.deinit(allocator);

        var tokens = std.mem.tokenizeAny(u8, line, " \t");
        while (tokens.next()) |token| {
            const num = try std.fmt.parseInt(i64, token, 10);
            try numbers.append(allocator, num);
        }

        const next_val = try extrapolateNext(allocator, numbers.items);
        const prev_val = try extrapolatePrev(allocator, numbers.items);

        part1_sum += next_val;
        part2_sum += prev_val;
    }

    std.debug.print("Part 1: {d}\n", .{part1_sum});
    std.debug.print("Part 2: {d}\n", .{part2_sum});
}

fn getDifferences(allocator: std.mem.Allocator, seq: []const i64) !std.ArrayListUnmanaged(i64) {
    var result: std.ArrayListUnmanaged(i64) = .empty;
    for (0..seq.len - 1) |i| {
        try result.append(allocator, seq[i + 1] - seq[i]);
    }
    return result;
}

fn allZeros(seq: []const i64) bool {
    for (seq) |val| {
        if (val != 0) return false;
    }
    return true;
}

fn extrapolateNext(allocator: std.mem.Allocator, seq: []const i64) !i64 {
    var sequences: std.ArrayListUnmanaged(std.ArrayListUnmanaged(i64)) = .empty;
    defer {
        for (sequences.items) |*s| {
            s.deinit(allocator);
        }
        sequences.deinit(allocator);
    }

    // Create a copy of the initial sequence
    var initial: std.ArrayListUnmanaged(i64) = .empty;
    try initial.appendSlice(allocator, seq);
    try sequences.append(allocator, initial);

    // Build difference sequences until all zeros
    while (!allZeros(sequences.items[sequences.items.len - 1].items)) {
        const current = sequences.items[sequences.items.len - 1].items;
        const diff = try getDifferences(allocator, current);
        try sequences.append(allocator, diff);
    }

    // Extrapolate next value from bottom up
    // Add 0 to the last sequence (all zeros)
    try sequences.items[sequences.items.len - 1].append(allocator, 0);

    // Work backwards, adding last[i] + last[i+1] to each sequence
    var i: usize = sequences.items.len - 1;
    while (i > 0) {
        i -= 1;
        const below_last = sequences.items[i + 1].items[sequences.items[i + 1].items.len - 1];
        const current_last = sequences.items[i].items[sequences.items[i].items.len - 1];
        try sequences.items[i].append(allocator, current_last + below_last);
    }

    return sequences.items[0].items[sequences.items[0].items.len - 1];
}

fn extrapolatePrev(allocator: std.mem.Allocator, seq: []const i64) !i64 {
    var sequences: std.ArrayListUnmanaged(std.ArrayListUnmanaged(i64)) = .empty;
    defer {
        for (sequences.items) |*s| {
            s.deinit(allocator);
        }
        sequences.deinit(allocator);
    }

    // Create a copy of the initial sequence
    var initial: std.ArrayListUnmanaged(i64) = .empty;
    try initial.appendSlice(allocator, seq);
    try sequences.append(allocator, initial);

    // Build difference sequences until all zeros
    while (!allZeros(sequences.items[sequences.items.len - 1].items)) {
        const current = sequences.items[sequences.items.len - 1].items;
        const diff = try getDifferences(allocator, current);
        try sequences.append(allocator, diff);
    }

    // For previous extrapolation, we track the "prepended" value
    // Start with 0 at the bottom
    var prepended_value: i64 = 0;

    // Work backwards: first[i] - prepended_value from below
    var i: usize = sequences.items.len - 1;
    while (i > 0) {
        i -= 1;
        const current_first = sequences.items[i].items[0];
        prepended_value = current_first - prepended_value;
    }

    return prepended_value;
}
