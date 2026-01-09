const std = @import("std");

const Allocator = std.mem.Allocator;

/// A packet value can be either an integer or a list of values
const Value = union(enum) {
    integer: i32,
    list: []Value,

    fn deinit(self: Value, allocator: Allocator) void {
        switch (self) {
            .list => |items| {
                for (items) |item| {
                    item.deinit(allocator);
                }
                allocator.free(items);
            },
            .integer => {},
        }
    }
};

const ParseError = error{OutOfMemory};

/// Parse a packet string into a Value
fn parse(allocator: Allocator, input: []const u8) ParseError!Value {
    var pos: usize = 0;
    return parseValue(allocator, input, &pos);
}

fn parseValue(allocator: Allocator, input: []const u8, pos: *usize) ParseError!Value {
    if (input[pos.*] == '[') {
        return parseList(allocator, input, pos);
    } else {
        return parseInteger(input, pos);
    }
}

fn parseList(allocator: Allocator, input: []const u8, pos: *usize) ParseError!Value {
    var items: std.ArrayListUnmanaged(Value) = .empty;
    errdefer {
        for (items.items) |item| {
            item.deinit(allocator);
        }
        items.deinit(allocator);
    }

    pos.* += 1; // skip '['

    while (pos.* < input.len and input[pos.*] != ']') {
        if (input[pos.*] == ',') {
            pos.* += 1;
            continue;
        }
        const value = try parseValue(allocator, input, pos);
        try items.append(allocator, value);
    }

    pos.* += 1; // skip ']'

    return Value{ .list = try items.toOwnedSlice(allocator) };
}

fn parseInteger(input: []const u8, pos: *usize) ParseError!Value {
    var value: i32 = 0;
    while (pos.* < input.len and input[pos.*] >= '0' and input[pos.*] <= '9') {
        value = value * 10 + @as(i32, @intCast(input[pos.*] - '0'));
        pos.* += 1;
    }
    return Value{ .integer = value };
}

/// Compare two values
/// Returns: -1 if left < right (correct order)
///           1 if left > right (wrong order)
///           0 if equal (continue)
fn compare(left: Value, right: Value) i32 {
    switch (left) {
        .integer => |left_int| {
            switch (right) {
                .integer => |right_int| {
                    // Both integers
                    if (left_int < right_int) return -1;
                    if (left_int > right_int) return 1;
                    return 0;
                },
                .list => |right_list| {
                    // Left is int, right is list - wrap left in list
                    const wrapped = [_]Value{Value{ .integer = left_int }};
                    return compareList(&wrapped, right_list);
                },
            }
        },
        .list => |left_list| {
            switch (right) {
                .integer => |right_int| {
                    // Left is list, right is int - wrap right in list
                    const wrapped = [_]Value{Value{ .integer = right_int }};
                    return compareList(left_list, &wrapped);
                },
                .list => |right_list| {
                    // Both lists
                    return compareList(left_list, right_list);
                },
            }
        },
    }
}

fn compareList(left: []const Value, right: []const Value) i32 {
    const min_len = @min(left.len, right.len);

    for (0..min_len) |i| {
        const result = compare(left[i], right[i]);
        if (result != 0) return result;
    }

    // Check lengths
    if (left.len < right.len) return -1;
    if (left.len > right.len) return 1;
    return 0;
}

/// Comparison function for sorting (returns true if left < right)
fn lessThan(_: void, left: Value, right: Value) bool {
    return compare(left, right) < 0;
}

fn part1(allocator: Allocator, input: []const u8) !i32 {
    var pairs = std.mem.splitSequence(u8, input, "\n\n");
    var pair_index: i32 = 0;
    var total: i32 = 0;

    while (pairs.next()) |pair| {
        pair_index += 1;

        var lines = std.mem.tokenizeScalar(u8, pair, '\n');
        const line1 = lines.next() orelse continue;
        const line2 = lines.next() orelse continue;

        const left = try parse(allocator, line1);
        defer left.deinit(allocator);
        const right = try parse(allocator, line2);
        defer right.deinit(allocator);

        if (compare(left, right) == -1) {
            total += pair_index;
        }
    }

    return total;
}

fn part2(allocator: Allocator, input: []const u8) !i32 {
    var packets: std.ArrayListUnmanaged(Value) = .empty;
    defer {
        for (packets.items) |packet| {
            packet.deinit(allocator);
        }
        packets.deinit(allocator);
    }

    // Parse all packets
    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const packet = try parse(allocator, line);
        try packets.append(allocator, packet);
    }

    // Add divider packets [[2]] and [[6]]
    const divider1 = try parse(allocator, "[[2]]");
    try packets.append(allocator, divider1);
    const divider2 = try parse(allocator, "[[6]]");
    try packets.append(allocator, divider2);

    // Sort packets
    std.mem.sort(Value, packets.items, {}, lessThan);

    // Find positions of dividers (1-indexed)
    var pos1: i32 = 0;
    var pos2: i32 = 0;

    for (packets.items, 1..) |packet, idx| {
        if (isDivider(packet, 2)) {
            pos1 = @intCast(idx);
        } else if (isDivider(packet, 6)) {
            pos2 = @intCast(idx);
        }
    }

    return pos1 * pos2;
}

/// Check if a packet is a divider [[n]]
fn isDivider(packet: Value, n: i32) bool {
    switch (packet) {
        .list => |outer| {
            if (outer.len != 1) return false;
            switch (outer[0]) {
                .list => |inner| {
                    if (inner.len != 1) return false;
                    switch (inner[0]) {
                        .integer => |val| return val == n,
                        .list => return false,
                    }
                },
                .integer => return false,
            }
        },
        .integer => return false,
    }
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    const trimmed = std.mem.trim(u8, input, "\n\r ");

    const answer1 = try part1(allocator, trimmed);
    const answer2 = try part2(allocator, trimmed);

    std.debug.print("Part 1: {d}\n", .{answer1});
    std.debug.print("Part 2: {d}\n", .{answer2});
}
