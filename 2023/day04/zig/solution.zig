const std = @import("std");

const Card = struct {
    winning: []u32,
    have: []u32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const content = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(content);

    // Trim trailing whitespace
    const trimmed = std.mem.trimRight(u8, content, " \t\n\r");

    // Collect lines using ArrayList (single pass)
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, trimmed, '\n');
    while (line_iter.next()) |line| {
        try lines.append(allocator, line);
    }

    // Part 1
    const part1_result = try part1(allocator, lines.items);
    std.debug.print("Part 1: {d}\n", .{part1_result});

    // Part 2
    const part2_result = try part2(allocator, lines.items);
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn parseNumbers(allocator: std.mem.Allocator, text: []const u8) ![]u32 {
    var numbers: std.ArrayListUnmanaged(u32) = .empty;
    errdefer numbers.deinit(allocator);

    var iter = std.mem.tokenizeAny(u8, text, " ");
    while (iter.next()) |num_str| {
        try numbers.append(allocator, try std.fmt.parseInt(u32, num_str, 10));
    }

    return numbers.toOwnedSlice(allocator);
}

fn countMatches(winning: []const u32, have: []const u32) usize {
    var matches: usize = 0;
    for (have) |h| {
        for (winning) |w| {
            if (h == w) {
                matches += 1;
                break;
            }
        }
    }
    return matches;
}

fn parseCard(allocator: std.mem.Allocator, line: []const u8) !Card {
    // Split on ':'
    var colon_iter = std.mem.splitScalar(u8, line, ':');
    _ = colon_iter.next(); // Skip "Card X"
    const numbers_part = colon_iter.next() orelse return error.InvalidInput;

    // Split on '|'
    var pipe_iter = std.mem.splitScalar(u8, numbers_part, '|');
    const winning_part = pipe_iter.next() orelse return error.InvalidInput;
    const have_part = pipe_iter.next() orelse return error.InvalidInput;

    const winning = try parseNumbers(allocator, winning_part);
    const have = try parseNumbers(allocator, have_part);

    return Card{ .winning = winning, .have = have };
}

fn part1(allocator: std.mem.Allocator, lines: []const []const u8) !u64 {
    var total: u64 = 0;

    for (lines) |line| {
        const card = try parseCard(allocator, line);
        defer allocator.free(card.winning);
        defer allocator.free(card.have);

        const matches = countMatches(card.winning, card.have);
        if (matches > 0) {
            // Score is 2^(matches-1)
            total += @as(u64, 1) << @intCast(matches - 1);
        }
    }

    return total;
}

fn part2(allocator: std.mem.Allocator, lines: []const []const u8) !u64 {
    const card_count = lines.len;

    // Allocate array to track match counts for each card
    const matches = try allocator.alloc(usize, card_count);
    defer allocator.free(matches);

    // Calculate matches for each card
    for (lines, 0..) |line, i| {
        const card = try parseCard(allocator, line);
        defer allocator.free(card.winning);
        defer allocator.free(card.have);

        matches[i] = countMatches(card.winning, card.have);
    }

    // Track copies of each card (start with 1 of each)
    const copies = try allocator.alloc(u64, card_count);
    defer allocator.free(copies);
    @memset(copies, 1);

    // Process cards and cascade copies
    for (0..card_count) |i| {
        const match_count = matches[i];
        const current_copies = copies[i];

        // Win copies of the next match_count cards
        const end = @min(i + 1 + match_count, card_count);
        for (copies[i + 1 .. end]) |*c| {
            c.* += current_copies;
        }
    }

    // Sum all copies
    var total: u64 = 0;
    for (copies) |c| {
        total += c;
    }

    return total;
}
