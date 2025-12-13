const std = @import("std");

const DIRS = [_][2]i32{
    .{ -1, 0 }, // up
    .{ 1, 0 },  // down
    .{ 0, -1 }, // left
    .{ 0, 1 },  // right
};

const Position = struct {
    r: usize,
    c: usize,
};

const Grid = struct {
    data: [][]u8,
    rows: usize,
    cols: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, input: []const u8) !Grid {
        var lines: std.ArrayList([]u8) = .{};
        defer lines.deinit(allocator);
        errdefer {
            for (lines.items) |row| {
                allocator.free(row);
            }
        }

        var line_iter = std.mem.tokenizeScalar(u8, input, '\n');
        while (line_iter.next()) |line| {
            const row = try allocator.alloc(u8, line.len);
            for (line, 0..) |c, i| {
                row[i] = c - '0';
            }
            try lines.append(allocator, row);
        }

        const rows = lines.items.len;
        const cols = if (rows > 0) lines.items[0].len else 0;

        const data = try lines.toOwnedSlice(allocator);
        return Grid{
            .data = data,
            .rows = rows,
            .cols = cols,
            .allocator = allocator,
        };
    }

    fn deinit(self: *Grid) void {
        for (self.data) |row| {
            self.allocator.free(row);
        }
        self.allocator.free(self.data);
    }

    fn findTrailheads(self: *const Grid, allocator: std.mem.Allocator) ![]Position {
        var trailheads: std.ArrayList(Position) = .{};
        for (0..self.rows) |r| {
            for (0..self.cols) |c| {
                if (self.data[r][c] == 0) {
                    try trailheads.append(allocator, Position{ .r = r, .c = c });
                }
            }
        }
        return trailheads.toOwnedSlice(allocator);
    }

    fn isValidCoord(self: *const Grid, r_signed: i32, c_signed: i32) ?Position {
        if (r_signed < 0 or c_signed < 0) return null;
        const r = @as(usize, @intCast(r_signed));
        const c = @as(usize, @intCast(c_signed));
        if (r >= self.rows or c >= self.cols) return null;
        return Position{ .r = r, .c = c };
    }

    fn countReachableNines(self: *const Grid, allocator: std.mem.Allocator, start_r: usize, start_c: usize) !usize {
        var visited = std.AutoHashMap(Position, void).init(allocator);
        defer visited.deinit();

        var queue: std.ArrayList(Position) = .{};
        defer queue.deinit(allocator);

        var nines = std.AutoHashMap(Position, void).init(allocator);
        defer nines.deinit();

        const start_pos = Position{ .r = start_r, .c = start_c };
        try visited.put(start_pos, {});
        try queue.append(allocator, start_pos);

        var read_idx: usize = 0;
        while (read_idx < queue.items.len) : (read_idx += 1) {
            const pos = queue.items[read_idx];
            const current_height = self.data[pos.r][pos.c];

            if (current_height == 9) {
                try nines.put(pos, {});
                continue;
            }

            for (DIRS) |dir| {
                const nr_signed = @as(i32, @intCast(pos.r)) + dir[0];
                const nc_signed = @as(i32, @intCast(pos.c)) + dir[1];

                const next_pos = self.isValidCoord(nr_signed, nc_signed) orelse continue;
                if (visited.contains(next_pos)) continue;

                if (self.data[next_pos.r][next_pos.c] == current_height + 1) {
                    try visited.put(next_pos, {});
                    try queue.append(allocator, next_pos);
                }
            }
        }

        return nines.count();
    }

    fn countDistinctTrails(self: *const Grid, start_r: usize, start_c: usize) u64 {
        return self.dfs(start_r, start_c);
    }

    fn dfs(self: *const Grid, r: usize, c: usize) u64 {
        const current_height = self.data[r][c];
        if (current_height == 9) {
            return 1;
        }

        var total: u64 = 0;
        for (DIRS) |dir| {
            const nr_signed = @as(i32, @intCast(r)) + dir[0];
            const nc_signed = @as(i32, @intCast(c)) + dir[1];

            const next_pos = self.isValidCoord(nr_signed, nc_signed) orelse continue;

            if (self.data[next_pos.r][next_pos.c] == current_height + 1) {
                total += self.dfs(next_pos.r, next_pos.c);
            }
        }
        return total;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file_path = "../input.txt";
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const input = try allocator.alloc(u8, file_size);
    defer allocator.free(input);

    _ = try file.readAll(input);

    // Parse grid
    var grid = try Grid.init(allocator, input);
    defer grid.deinit();

    // Find trailheads once and reuse for both parts
    const trailheads = try grid.findTrailheads(allocator);
    defer allocator.free(trailheads);

    // Part 1
    var part1_total: usize = 0;
    for (trailheads) |th| {
        const score = try grid.countReachableNines(allocator, th.r, th.c);
        part1_total += score;
    }

    // Part 2
    var part2_total: u64 = 0;
    for (trailheads) |th| {
        const rating = grid.countDistinctTrails(th.r, th.c);
        part2_total += rating;
    }

    std.debug.print("Part 1: {d}\n", .{part1_total});
    std.debug.print("Part 2: {d}\n", .{part2_total});
}
