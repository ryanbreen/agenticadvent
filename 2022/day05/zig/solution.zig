const std = @import("std");

const MAX_STACKS = 10;
const MAX_HEIGHT = 100;

const Move = struct {
    count: usize,
    from: usize,
    to: usize,
};

const Stacks = struct {
    data: [MAX_STACKS][MAX_HEIGHT]u8,
    heights: [MAX_STACKS]usize,
    num_stacks: usize,

    fn init() Stacks {
        return Stacks{
            .data = undefined,
            .heights = [_]usize{0} ** MAX_STACKS,
            .num_stacks = 0,
        };
    }

    fn push(self: *Stacks, stack_idx: usize, crate: u8) void {
        self.data[stack_idx][self.heights[stack_idx]] = crate;
        self.heights[stack_idx] += 1;
    }

    fn pop(self: *Stacks, stack_idx: usize) u8 {
        self.heights[stack_idx] -= 1;
        return self.data[stack_idx][self.heights[stack_idx]];
    }

    fn top(self: *const Stacks, stack_idx: usize) u8 {
        return self.data[stack_idx][self.heights[stack_idx] - 1];
    }

    fn clone(self: *const Stacks) Stacks {
        var result = Stacks.init();
        result.num_stacks = self.num_stacks;
        for (0..self.num_stacks) |i| {
            result.heights[i] = self.heights[i];
            for (0..self.heights[i]) |j| {
                result.data[i][j] = self.data[i][j];
            }
        }
        return result;
    }

    fn getTopCrates(self: *const Stacks, buffer: []u8) []u8 {
        var idx: usize = 0;
        for (0..self.num_stacks) |i| {
            if (self.heights[i] > 0) {
                buffer[idx] = self.top(i);
                idx += 1;
            }
        }
        return buffer[0..idx];
    }
};

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    // Split by double newline to separate stacks from moves
    var parts = std.mem.splitSequence(u8, input, "\n\n");
    const stack_part = parts.next() orelse return error.InvalidInput;
    const moves_part = parts.next() orelse return error.InvalidInput;

    // Parse stacks
    var stacks = Stacks.init();

    // Collect stack lines
    var stack_lines_buf: [20][]const u8 = undefined;
    var stack_line_count: usize = 0;

    var lines_iter = std.mem.splitScalar(u8, stack_part, '\n');
    while (lines_iter.next()) |line| {
        stack_lines_buf[stack_line_count] = line;
        stack_line_count += 1;
    }
    const stack_lines = stack_lines_buf[0..stack_line_count];

    // Find number of stacks from the last line
    const num_line = stack_lines[stack_lines.len - 1];
    var num_iter = std.mem.tokenizeScalar(u8, num_line, ' ');
    var num_count: usize = 0;
    while (num_iter.next()) |_| {
        num_count += 1;
    }
    stacks.num_stacks = num_count;

    // Parse crate positions (bottom-up, excluding the number line)
    // Process from bottom to top of the stack diagram
    var line_idx: usize = stack_lines.len - 2;
    while (true) {
        const line = stack_lines[line_idx];
        for (0..stacks.num_stacks) |i| {
            const pos = 1 + i * 4;
            if (pos < line.len and line[pos] != ' ') {
                stacks.push(i, line[pos]);
            }
        }
        if (line_idx == 0) break;
        line_idx -= 1;
    }

    // Parse moves
    var moves_buf: [1000]Move = undefined;
    var moves_count: usize = 0;

    var moves_iter = std.mem.splitScalar(u8, moves_part, '\n');
    while (moves_iter.next()) |line| {
        if (line.len == 0) continue;

        // Parse "move N from X to Y"
        var tokens = std.mem.tokenizeScalar(u8, line, ' ');
        _ = tokens.next(); // "move"
        const count_str = tokens.next() orelse continue;
        _ = tokens.next(); // "from"
        const from_str = tokens.next() orelse continue;
        _ = tokens.next(); // "to"
        const to_str = tokens.next() orelse continue;

        const count = try std.fmt.parseInt(usize, count_str, 10);
        const from = try std.fmt.parseInt(usize, from_str, 10) - 1;
        const to = try std.fmt.parseInt(usize, to_str, 10) - 1;

        moves_buf[moves_count] = Move{ .count = count, .from = from, .to = to };
        moves_count += 1;
    }
    const moves = moves_buf[0..moves_count];

    // Part 1: Move crates one at a time
    var stacks1 = stacks.clone();
    for (moves) |move| {
        for (0..move.count) |_| {
            const crate = stacks1.pop(move.from);
            stacks1.push(move.to, crate);
        }
    }

    var buffer1: [MAX_STACKS]u8 = undefined;
    const result1 = stacks1.getTopCrates(&buffer1);

    // Part 2: Move multiple crates at once (preserve order)
    var stacks2 = stacks.clone();
    var temp_buffer: [MAX_HEIGHT]u8 = undefined;
    for (moves) |move| {
        // Pop crates into temporary buffer, then push in reverse order
        for (0..move.count) |i| {
            temp_buffer[move.count - 1 - i] = stacks2.pop(move.from);
        }
        for (0..move.count) |i| {
            stacks2.push(move.to, temp_buffer[i]);
        }
    }

    var buffer2: [MAX_STACKS]u8 = undefined;
    const result2 = stacks2.getTopCrates(&buffer2);

    std.debug.print("Part 1: {s}\n", .{result1});
    std.debug.print("Part 2: {s}\n", .{result2});
}
