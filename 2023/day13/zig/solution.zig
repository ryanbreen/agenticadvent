const std = @import("std");

const Pattern = struct {
    lines: []const []const u8,
    allocator: std.mem.Allocator,

    fn deinit(self: Pattern) void {
        for (self.lines) |line| {
            self.allocator.free(line);
        }
        self.allocator.free(self.lines);
    }

    fn width(self: Pattern) usize {
        return if (self.lines.len > 0) self.lines[0].len else 0;
    }

    fn height(self: Pattern) usize {
        return self.lines.len;
    }

    /// Find vertical reflection line with exactly `target_diff` total character differences.
    /// Part 1 uses target_diff=0 (perfect reflection), Part 2 uses target_diff=1 (smudge).
    fn findVerticalReflection(self: Pattern, target_diff: usize) usize {
        const w = self.width();
        if (w == 0) return 0;

        for (1..w) |col| {
            var total_diff: usize = 0;

            for (self.lines) |row| {
                var i: usize = 0;
                while (i < col and col + i < row.len) : (i += 1) {
                    if (row[col - 1 - i] != row[col + i]) {
                        total_diff += 1;
                        if (total_diff > target_diff) break;
                    }
                }
                if (total_diff > target_diff) break;
            }

            if (total_diff == target_diff) return col;
        }

        return 0;
    }

    /// Find horizontal reflection line with exactly `target_diff` total character differences.
    /// Part 1 uses target_diff=0 (perfect reflection), Part 2 uses target_diff=1 (smudge).
    fn findHorizontalReflection(self: Pattern, target_diff: usize) usize {
        const h = self.height();
        if (h == 0) return 0;

        for (1..h) |row| {
            var total_diff: usize = 0;

            var i: usize = 0;
            while (i < row and row + i < h) : (i += 1) {
                total_diff += countDifferences(self.lines[row - 1 - i], self.lines[row + i]);
                if (total_diff > target_diff) break;
            }

            if (total_diff == target_diff) return row;
        }

        return 0;
    }

    /// Summarize pattern by finding reflection line with target_diff differences.
    /// Returns columns left of vertical reflection, or 100 * rows above horizontal reflection.
    fn summarize(self: Pattern, target_diff: usize) usize {
        const v = self.findVerticalReflection(target_diff);
        if (v > 0) return v;
        return self.findHorizontalReflection(target_diff) * 100;
    }
};

fn countDifferences(s1: []const u8, s2: []const u8) usize {
    const len = @min(s1.len, s2.len);
    var count: usize = 0;
    for (0..len) |i| {
        if (s1[i] != s2[i]) count += 1;
    }
    return count;
}

fn parseInput(allocator: std.mem.Allocator, text: []const u8) ![]Pattern {
    var patterns: std.ArrayListUnmanaged(Pattern) = .empty;
    errdefer {
        for (patterns.items) |pattern| {
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
            try lines.append(allocator, try allocator.dupe(u8, line));
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

fn solve(patterns: []const Pattern, target_diff: usize) usize {
    var sum: usize = 0;
    for (patterns) |pattern| {
        sum += pattern.summarize(target_diff);
    }
    return sum;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const text = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(text);

    const patterns = try parseInput(allocator, text);
    defer {
        for (patterns) |pattern| {
            pattern.deinit();
        }
        allocator.free(patterns);
    }

    var buffer: [256]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    try stdout.interface.print("Part 1: {d}\n", .{solve(patterns, 0)});
    try stdout.interface.print("Part 2: {d}\n", .{solve(patterns, 1)});
    try stdout.interface.flush();
}
