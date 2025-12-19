const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const input = std.mem.trim(u8, content, "\n\r ");

    // Parse input - split by double newline
    var sections = std.mem.splitSequence(u8, input, "\n\n");
    const patterns_line = sections.next() orelse return error.InvalidInput;
    const designs_section = sections.next() orelse return error.InvalidInput;

    // Parse patterns
    var patterns_list: ArrayList([]const u8) = .{};
    defer patterns_list.deinit(allocator);

    var pattern_iter = std.mem.splitSequence(u8, patterns_line, ", ");
    while (pattern_iter.next()) |p| {
        const trimmed = std.mem.trim(u8, p, " \t\r");
        if (trimmed.len > 0) {
            try patterns_list.append(allocator, trimmed);
        }
    }
    const patterns = patterns_list.items;

    // Parse designs
    var designs_list: ArrayList([]const u8) = .{};
    defer designs_list.deinit(allocator);

    var design_iter = std.mem.splitScalar(u8, designs_section, '\n');
    while (design_iter.next()) |d| {
        const trimmed = std.mem.trim(u8, d, " \t\r");
        if (trimmed.len > 0) {
            try designs_list.append(allocator, trimmed);
        }
    }
    const designs = designs_list.items;

    // Part 1: Count designs that can be formed
    // Part 2: Sum the number of ways
    var part1: u64 = 0;
    var part2: u64 = 0;

    for (designs) |design| {
        const ways = try countWays(allocator, design, patterns);
        if (ways > 0) {
            part1 += 1;
        }
        part2 += ways;
    }

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}

fn countWays(allocator: std.mem.Allocator, design: []const u8, patterns: []const []const u8) !u64 {
    // dp[pos] = number of ways to form design[pos..]
    // Use a HashMap for memoization
    var memo = AutoHashMap(usize, u64).init(allocator);
    defer memo.deinit();

    return try dp(&memo, design, patterns, 0);
}

fn dp(memo: *AutoHashMap(usize, u64), design: []const u8, patterns: []const []const u8, pos: usize) !u64 {
    if (pos == design.len) {
        return 1;
    }

    // Check memo
    if (memo.get(pos)) |cached| {
        return cached;
    }

    var total: u64 = 0;
    for (patterns) |pattern| {
        const plen = pattern.len;
        if (pos + plen <= design.len) {
            // Check if pattern matches at current position
            if (std.mem.eql(u8, design[pos .. pos + plen], pattern)) {
                total += try dp(memo, design, patterns, pos + plen);
            }
        }
    }

    try memo.put(pos, total);
    return total;
}
