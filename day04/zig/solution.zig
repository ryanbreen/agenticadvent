const std = @import("std");

const Direction = struct {
    dr: i32,
    dc: i32,
};

const directions = [_]Direction{
    .{ .dr = -1, .dc = -1 }, .{ .dr = -1, .dc = 0 }, .{ .dr = -1, .dc = 1 },
    .{ .dr = 0, .dc = -1 },                           .{ .dr = 0, .dc = 1 },
    .{ .dr = 1, .dc = -1 },  .{ .dr = 1, .dc = 0 },  .{ .dr = 1, .dc = 1 },
};

const Grid = struct {
    data: [][]u8,
    rows: usize,
    cols: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, input: []const u8) !Grid {
        var line_count: usize = 0;
        var cols: usize = 0;
        var lines = std.mem.splitScalar(u8, input, '\n');
        while (lines.next()) |line| {
            if (line.len > 0) {
                line_count += 1;
                if (cols == 0) cols = line.len;
            }
        }

        const data = try allocator.alloc([]u8, line_count);
        errdefer allocator.free(data);

        var i: usize = 0;
        lines = std.mem.splitScalar(u8, input, '\n');
        while (lines.next()) |line| {
            if (line.len > 0) {
                data[i] = try allocator.dupe(u8, line);
                i += 1;
            }
        }

        return Grid{
            .data = data,
            .rows = line_count,
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

    fn get(self: *const Grid, r: usize, c: usize) u8 {
        return self.data[r][c];
    }

    fn set(self: *Grid, r: usize, c: usize, val: u8) void {
        self.data[r][c] = val;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    // Trim trailing whitespace
    const input = std.mem.trim(u8, buffer, &std.ascii.whitespace);

    // Part 1
    const part1_result = try part1(allocator, input);
    std.debug.print("Part 1: {d}\n", .{part1_result});

    // Part 2
    const part2_result = try part2(allocator, input);
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn part1(allocator: std.mem.Allocator, input: []const u8) !usize {
    var grid = try Grid.init(allocator, input);
    defer grid.deinit();

    var accessible_count: usize = 0;

    for (0..grid.rows) |r| {
        for (0..grid.cols) |c| {
            if (grid.get(r, c) == '@') {
                // Count adjacent rolls
                var adjacent_rolls: usize = 0;
                for (directions) |dir| {
                    const nr = @as(i32, @intCast(r)) + dir.dr;
                    const nc = @as(i32, @intCast(c)) + dir.dc;

                    // Check bounds
                    if (nr >= 0 and nr < @as(i32, @intCast(grid.rows)) and
                        nc >= 0 and nc < @as(i32, @intCast(grid.cols))) {
                        const ur = @as(usize, @intCast(nr));
                        const uc = @as(usize, @intCast(nc));
                        if (grid.get(ur, uc) == '@') {
                            adjacent_rolls += 1;
                        }
                    }
                }

                // Accessible if fewer than 4 adjacent rolls
                if (adjacent_rolls < 4) {
                    accessible_count += 1;
                }
            }
        }
    }

    return accessible_count;
}

fn part2(allocator: std.mem.Allocator, input: []const u8) !usize {
    var grid = try Grid.init(allocator, input);
    defer grid.deinit();

    var total_removed: usize = 0;

    const Position = struct { r: usize, c: usize };
    var removable: std.ArrayList(Position) = .{};
    defer removable.deinit(allocator);

    while (true) {
        // Clear previous removable list
        removable.clearRetainingCapacity();

        // Find all rolls that can be removed in this iteration
        for (0..grid.rows) |r| {
            for (0..grid.cols) |c| {
                if (grid.get(r, c) == '@') {
                    // Count adjacent rolls
                    var adjacent_rolls: usize = 0;
                    for (directions) |dir| {
                        const nr = @as(i32, @intCast(r)) + dir.dr;
                        const nc = @as(i32, @intCast(c)) + dir.dc;

                        // Check bounds
                        if (nr >= 0 and nr < @as(i32, @intCast(grid.rows)) and
                            nc >= 0 and nc < @as(i32, @intCast(grid.cols))) {
                            const ur = @as(usize, @intCast(nr));
                            const uc = @as(usize, @intCast(nc));
                            if (grid.get(ur, uc) == '@') {
                                adjacent_rolls += 1;
                            }
                        }
                    }

                    // Can be removed if fewer than 4 adjacent rolls
                    if (adjacent_rolls < 4) {
                        try removable.append(allocator, .{ .r = r, .c = c });
                    }
                }
            }
        }

        // If no rolls can be removed, we're done
        if (removable.items.len == 0) {
            break;
        }

        // Remove all accessible rolls
        for (removable.items) |pos| {
            grid.set(pos.r, pos.c, '.');
        }

        total_removed += removable.items.len;
    }

    return total_removed;
}
