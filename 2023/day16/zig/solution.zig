const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const Direction = enum(u2) {
    right = 0,
    down = 1,
    left = 2,
    up = 3,
};

const State = struct {
    row: i32,
    col: i32,
    dir: Direction,
};

// Direction deltas: right, down, left, up
const DR = [4]i32{ 0, 1, 0, -1 };
const DC = [4]i32{ 1, 0, -1, 0 };

fn countEnergized(allocator: Allocator, grid: []const []const u8, start_row: i32, start_col: i32, start_dir: Direction) !usize {
    const rows: i32 = @intCast(grid.len);
    const cols: i32 = @intCast(grid[0].len);

    // Visited set: indexed by (row * cols + col) * 4 + dir
    const visited_size: usize = @intCast(rows * cols * 4);
    var visited = try allocator.alloc(bool, visited_size);
    defer allocator.free(visited);
    @memset(visited, false);

    // BFS queue
    var queue = try ArrayList(State).initCapacity(allocator, 1000);
    defer queue.deinit(allocator);

    try queue.append(allocator, State{ .row = start_row, .col = start_col, .dir = start_dir });

    while (queue.items.len > 0) {
        const state = queue.orderedRemove(0);
        const r = state.row;
        const c = state.col;
        const d = state.dir;

        // Bounds check
        if (r < 0 or r >= rows or c < 0 or c >= cols) {
            continue;
        }

        // Visited check
        const idx: usize = @intCast((r * cols + c) * 4 + @as(i32, @intFromEnum(d)));
        if (visited[idx]) {
            continue;
        }
        visited[idx] = true;

        const cell = grid[@intCast(r)][@intCast(c)];

        // Calculate next directions based on cell type
        var next_dirs: [2]?Direction = .{ null, null };
        var dir_count: usize = 0;

        switch (cell) {
            '.' => {
                next_dirs[0] = d;
                dir_count = 1;
            },
            '/' => {
                // right->up, down->left, left->down, up->right
                const mapping = [4]Direction{ .up, .left, .down, .right };
                next_dirs[0] = mapping[@intFromEnum(d)];
                dir_count = 1;
            },
            '\\' => {
                // right->down, down->right, left->up, up->left
                const mapping = [4]Direction{ .down, .right, .up, .left };
                next_dirs[0] = mapping[@intFromEnum(d)];
                dir_count = 1;
            },
            '|' => {
                if (d == .right or d == .left) {
                    // Split: up and down
                    next_dirs[0] = .down;
                    next_dirs[1] = .up;
                    dir_count = 2;
                } else {
                    next_dirs[0] = d;
                    dir_count = 1;
                }
            },
            '-' => {
                if (d == .down or d == .up) {
                    // Split: left and right
                    next_dirs[0] = .right;
                    next_dirs[1] = .left;
                    dir_count = 2;
                } else {
                    next_dirs[0] = d;
                    dir_count = 1;
                }
            },
            else => {
                next_dirs[0] = d;
                dir_count = 1;
            },
        }

        // Add next states to queue
        for (0..dir_count) |i| {
            if (next_dirs[i]) |nd| {
                const nd_idx = @intFromEnum(nd);
                try queue.append(allocator, State{
                    .row = r + DR[nd_idx],
                    .col = c + DC[nd_idx],
                    .dir = nd,
                });
            }
        }
    }

    // Count unique energized tiles
    var energized: usize = 0;
    for (0..@as(usize, @intCast(rows))) |row| {
        for (0..@as(usize, @intCast(cols))) |col| {
            const base_idx: usize = (row * @as(usize, @intCast(cols)) + col) * 4;
            if (visited[base_idx] or visited[base_idx + 1] or visited[base_idx + 2] or visited[base_idx + 3]) {
                energized += 1;
            }
        }
    }

    return energized;
}

fn part1(allocator: Allocator, grid: []const []const u8) !usize {
    return countEnergized(allocator, grid, 0, 0, .right);
}

fn part2(allocator: Allocator, grid: []const []const u8) !usize {
    const rows: i32 = @intCast(grid.len);
    const cols: i32 = @intCast(grid[0].len);
    var max_energized: usize = 0;

    // Top row, heading down
    for (0..@as(usize, @intCast(cols))) |c| {
        const energized = try countEnergized(allocator, grid, 0, @intCast(c), .down);
        max_energized = @max(max_energized, energized);
    }

    // Bottom row, heading up
    for (0..@as(usize, @intCast(cols))) |c| {
        const energized = try countEnergized(allocator, grid, rows - 1, @intCast(c), .up);
        max_energized = @max(max_energized, energized);
    }

    // Left column, heading right
    for (0..@as(usize, @intCast(rows))) |r| {
        const energized = try countEnergized(allocator, grid, @intCast(r), 0, .right);
        max_energized = @max(max_energized, energized);
    }

    // Right column, heading left
    for (0..@as(usize, @intCast(rows))) |r| {
        const energized = try countEnergized(allocator, grid, @intCast(r), cols - 1, .left);
        max_energized = @max(max_energized, energized);
    }

    return max_energized;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse into grid
    var lines = try ArrayList([]const u8).initCapacity(allocator, 200);
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, content, '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            try lines.append(allocator, line);
        }
    }

    const grid = lines.items;

    const result1 = try part1(allocator, grid);
    const result2 = try part2(allocator, grid);

    std.debug.print("Part 1: {d}\n", .{result1});
    std.debug.print("Part 2: {d}\n", .{result2});
}
