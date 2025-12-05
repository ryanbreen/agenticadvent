const std = @import("std");

const Event = struct {
    pos: usize,
    kind: enum { mul, do_enable, dont_disable },
    x: i32,
    y: i32,
};

fn lessThan(_: void, a: Event, b: Event) bool {
    return a.pos < b.pos;
}

fn part1(data: []const u8) !i32 {
    var total: i32 = 0;
    var i: usize = 0;

    while (i < data.len) {
        // Look for "mul("
        if (i + 4 < data.len and std.mem.eql(u8, data[i..i+4], "mul(")) {
            var j = i + 4;

            // Parse first number (1-3 digits)
            var x: i32 = 0;
            var digit_count: usize = 0;
            while (j < data.len and digit_count < 3 and std.ascii.isDigit(data[j])) {
                x = x * 10 + @as(i32, data[j] - '0');
                j += 1;
                digit_count += 1;
            }

            // Must have at least 1 digit
            if (digit_count == 0) {
                i += 1;
                continue;
            }

            // Must have comma
            if (j >= data.len or data[j] != ',') {
                i += 1;
                continue;
            }
            j += 1;

            // Parse second number (1-3 digits)
            var y: i32 = 0;
            digit_count = 0;
            while (j < data.len and digit_count < 3 and std.ascii.isDigit(data[j])) {
                y = y * 10 + @as(i32, data[j] - '0');
                j += 1;
                digit_count += 1;
            }

            // Must have at least 1 digit
            if (digit_count == 0) {
                i += 1;
                continue;
            }

            // Must have closing paren
            if (j >= data.len or data[j] != ')') {
                i += 1;
                continue;
            }

            // Valid mul instruction!
            total += x * y;
            i = j + 1;
        } else {
            i += 1;
        }
    }

    return total;
}

fn part2(allocator: std.mem.Allocator, data: []const u8) !i32 {
    var events = try std.ArrayList(Event).initCapacity(allocator, 100);
    defer events.deinit(allocator);

    var i: usize = 0;

    // Find all mul instructions
    while (i < data.len) {
        if (i + 4 < data.len and std.mem.eql(u8, data[i..i+4], "mul(")) {
            var j = i + 4;

            // Parse first number (1-3 digits)
            var x: i32 = 0;
            var digit_count: usize = 0;
            while (j < data.len and digit_count < 3 and std.ascii.isDigit(data[j])) {
                x = x * 10 + @as(i32, data[j] - '0');
                j += 1;
                digit_count += 1;
            }

            if (digit_count == 0 or j >= data.len or data[j] != ',') {
                i += 1;
                continue;
            }
            j += 1;

            // Parse second number (1-3 digits)
            var y: i32 = 0;
            digit_count = 0;
            while (j < data.len and digit_count < 3 and std.ascii.isDigit(data[j])) {
                y = y * 10 + @as(i32, data[j] - '0');
                j += 1;
                digit_count += 1;
            }

            if (digit_count == 0 or j >= data.len or data[j] != ')') {
                i += 1;
                continue;
            }

            // Valid mul instruction!
            try events.append(allocator, Event{
                .pos = i,
                .kind = .mul,
                .x = x,
                .y = y,
            });
            i = j + 1;
        } else {
            i += 1;
        }
    }

    // Find all do() instructions
    i = 0;
    while (i < data.len) {
        if (i + 4 <= data.len and std.mem.eql(u8, data[i..i+4], "do()")) {
            try events.append(allocator, Event{
                .pos = i,
                .kind = .do_enable,
                .x = 0,
                .y = 0,
            });
            i += 4;
        } else {
            i += 1;
        }
    }

    // Find all don't() instructions
    i = 0;
    while (i < data.len) {
        if (i + 7 <= data.len and std.mem.eql(u8, data[i..i+7], "don't()")) {
            try events.append(allocator, Event{
                .pos = i,
                .kind = .dont_disable,
                .x = 0,
                .y = 0,
            });
            i += 7;
        } else {
            i += 1;
        }
    }

    // Sort events by position
    std.mem.sort(Event, events.items, {}, lessThan);

    // Process events in order
    var total: i32 = 0;
    var enabled = true;

    for (events.items) |event| {
        switch (event.kind) {
            .do_enable => enabled = true,
            .dont_disable => enabled = false,
            .mul => {
                if (enabled) {
                    total += event.x * event.y;
                }
            },
        }
    }

    return total;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const data = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(data);

    const p1 = try part1(data);
    const p2 = try part2(allocator, data);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
