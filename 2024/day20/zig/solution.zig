const std = @import("std");
const ArrayList = std.ArrayList;

const Point = struct {
    r: i32,
    c: i32,
};

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

    // Parse grid
    var lines_list: ArrayList([]const u8) = .{};
    defer lines_list.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, input, '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            try lines_list.append(allocator, line);
        }
    }

    const grid = lines_list.items;
    const rows: i32 = @intCast(grid.len);
    const cols: i32 = @intCast(grid[0].len);

    // Find start and end
    var start: Point = undefined;
    var end: Point = undefined;
    for (grid, 0..) |row, r| {
        for (row, 0..) |ch, c| {
            if (ch == 'S') {
                start = .{ .r = @intCast(r), .c = @intCast(c) };
            } else if (ch == 'E') {
                end = .{ .r = @intCast(r), .c = @intCast(c) };
            }
        }
    }

    // BFS to trace path - store distances
    // Using a dense 2D array for fast lookup
    const dist_array = try allocator.alloc(i32, @intCast(rows * cols));
    defer allocator.free(dist_array);
    @memset(dist_array, -1);

    // Also keep track of all track positions for iteration
    var track_positions: ArrayList(Point) = .{};
    defer track_positions.deinit(allocator);

    // BFS queue
    var queue: ArrayList(Point) = .{};
    defer queue.deinit(allocator);

    dist_array[@intCast(start.r * cols + start.c)] = 0;
    try queue.append(allocator, start);
    try track_positions.append(allocator, start);

    const dirs = [_][2]i32{ .{ -1, 0 }, .{ 1, 0 }, .{ 0, -1 }, .{ 0, 1 } };

    var queue_idx: usize = 0;
    while (queue_idx < queue.items.len) : (queue_idx += 1) {
        const cur = queue.items[queue_idx];
        const cur_dist = dist_array[@intCast(cur.r * cols + cur.c)];

        if (cur.r == end.r and cur.c == end.c) {
            continue;
        }

        for (dirs) |d| {
            const nr = cur.r + d[0];
            const nc = cur.c + d[1];

            if (nr >= 0 and nr < rows and nc >= 0 and nc < cols) {
                const idx: usize = @intCast(nr * cols + nc);
                if (grid[@intCast(nr)][@intCast(nc)] != '#' and dist_array[idx] == -1) {
                    dist_array[idx] = cur_dist + 1;
                    try queue.append(allocator, .{ .r = nr, .c = nc });
                    try track_positions.append(allocator, .{ .r = nr, .c = nc });
                }
            }
        }
    }

    // Count cheats
    const part1_result = countCheats(track_positions.items, dist_array, cols, 2, 100);
    const part2_result = countCheats(track_positions.items, dist_array, cols, 20, 100);

    const stdout = std.fs.File.stdout();
    var buf: [128]u8 = undefined;
    const output = std.fmt.bufPrint(&buf, "Part 1: {d}\nPart 2: {d}\n", .{ part1_result, part2_result }) catch unreachable;
    _ = stdout.write(output) catch {};
}

fn countCheats(track_positions: []const Point, dist_array: []const i32, cols: i32, max_cheat_time: i32, min_savings: i32) u64 {
    var count: u64 = 0;

    for (track_positions) |p1| {
        const d1 = dist_array[@intCast(p1.r * cols + p1.c)];

        for (track_positions) |p2| {
            const cheat_cost = absInt(p2.r - p1.r) + absInt(p2.c - p1.c);
            if (cheat_cost <= max_cheat_time) {
                const d2 = dist_array[@intCast(p2.r * cols + p2.c)];
                const savings = d2 - d1 - cheat_cost;
                if (savings >= min_savings) {
                    count += 1;
                }
            }
        }
    }

    return count;
}

fn absInt(x: i32) i32 {
    return if (x < 0) -x else x;
}
