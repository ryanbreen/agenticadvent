const std = @import("std");

const Pattern = struct {
    lines: [][]const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Pattern) void {
        for (self.lines) |line| {
            self.allocator.free(line);
        }
        self.allocator.free(self.lines);
    }
};

fn parseInput(allocator: std.mem.Allocator, text: []const u8) ![]Pattern {
    var patterns: std.ArrayListUnmanaged(Pattern) = .empty;
    errdefer {
        for (patterns.items) |*pattern| {
            pattern.deinit();
        }
        patterns.deinit(allocator);
    }

    var blocks = std.mem.splitSequence(u8, text, "\n\n");
    while (blocks.next()) |block| {
        if (block.len == 0) continue;

        var lines: std.ArrayListUnmanaged([]const u8) = .empty;
        errdefer {
            for (lines.items) |line| {
                allocator.free(line);
            }
            lines.deinit(allocator);
        }

        var line_iter = std.mem.splitScalar(u8, block, '\n');
        while (line_iter.next()) |line| {
            if (line.len == 0) continue;
            const line_copy = try allocator.dupe(u8, line);
            try lines.append(allocator, line_copy);
        }

        if (lines.items.len > 0) {
            try patterns.append(allocator, Pattern{
                .lines = try lines.toOwnedSlice(allocator),
                .allocator = allocator,
            });
        }
    }

    return try patterns.toOwnedSlice(allocator);
}

fn findVerticalReflection(pattern: Pattern) usize {
    if (pattern.lines.len == 0) return 0;
    const width = pattern.lines[0].len;

    var col: usize = 1;
    while (col < width) : (col += 1) {
        var is_reflection = true;

        for (pattern.lines) |row| {
            // Compare left side with right side (mirrored)
            const left_end = col;
            const right_start = col;

            var i: usize = 0;
            while (i < left_end and right_start + i < row.len) : (i += 1) {
                const left_idx = left_end - 1 - i;
                const right_idx = right_start + i;
                if (row[left_idx] != row[right_idx]) {
                    is_reflection = false;
                    break;
                }
            }
            if (!is_reflection) break;
        }

        if (is_reflection) return col;
    }

    return 0;
}

fn findHorizontalReflection(pattern: Pattern) usize {
    if (pattern.lines.len == 0) return 0;
    const height = pattern.lines.len;

    var row: usize = 1;
    while (row < height) : (row += 1) {
        var is_reflection = true;

        // Compare top with bottom (mirrored)
        var i: usize = 0;
        while (i < row and row + i < height) : (i += 1) {
            const top_idx = row - 1 - i;
            const bottom_idx = row + i;
            if (!std.mem.eql(u8, pattern.lines[top_idx], pattern.lines[bottom_idx])) {
                is_reflection = false;
                break;
            }
        }

        if (is_reflection) return row;
    }

    return 0;
}

fn summarizePattern(pattern: Pattern) usize {
    const v = findVerticalReflection(pattern);
    if (v > 0) return v;
    const h = findHorizontalReflection(pattern);
    return h * 100;
}

fn part1(patterns: []Pattern) usize {
    var sum: usize = 0;
    for (patterns) |pattern| {
        sum += summarizePattern(pattern);
    }
    return sum;
}

fn countDifferences(s1: []const u8, s2: []const u8) usize {
    var count: usize = 0;
    const min_len = @min(s1.len, s2.len);
    for (0..min_len) |i| {
        if (s1[i] != s2[i]) count += 1;
    }
    return count;
}

fn findVerticalReflectionWithSmudge(pattern: Pattern) usize {
    if (pattern.lines.len == 0) return 0;
    const width = pattern.lines[0].len;

    var col: usize = 1;
    while (col < width) : (col += 1) {
        var total_diff: usize = 0;

        for (pattern.lines) |row| {
            const left_end = col;
            const right_start = col;

            var i: usize = 0;
            while (i < left_end and right_start + i < row.len) : (i += 1) {
                const left_idx = left_end - 1 - i;
                const right_idx = right_start + i;
                if (row[left_idx] != row[right_idx]) {
                    total_diff += 1;
                    if (total_diff > 1) break;
                }
            }
            if (total_diff > 1) break;
        }

        if (total_diff == 1) return col;
    }

    return 0;
}

fn findHorizontalReflectionWithSmudge(pattern: Pattern) usize {
    if (pattern.lines.len == 0) return 0;
    const height = pattern.lines.len;

    var row: usize = 1;
    while (row < height) : (row += 1) {
        var total_diff: usize = 0;

        var i: usize = 0;
        while (i < row and row + i < height) : (i += 1) {
            const top_idx = row - 1 - i;
            const bottom_idx = row + i;
            total_diff += countDifferences(pattern.lines[top_idx], pattern.lines[bottom_idx]);
            if (total_diff > 1) break;
        }

        if (total_diff == 1) return row;
    }

    return 0;
}

fn summarizePatternWithSmudge(pattern: Pattern) usize {
    const v = findVerticalReflectionWithSmudge(pattern);
    if (v > 0) return v;
    const h = findHorizontalReflectionWithSmudge(pattern);
    return h * 100;
}

fn part2(patterns: []Pattern) usize {
    var sum: usize = 0;
    for (patterns) |pattern| {
        sum += summarizePatternWithSmudge(pattern);
    }
    return sum;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const text = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(text);

    // Parse input
    const patterns = try parseInput(allocator, text);
    defer {
        for (patterns) |*pattern| {
            pattern.deinit();
        }
        allocator.free(patterns);
    }

    // Solve
    const p1 = part1(patterns);
    const p2 = part2(patterns);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
