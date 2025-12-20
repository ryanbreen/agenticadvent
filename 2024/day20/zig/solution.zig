const std = @import("std");
const ArrayList = std.ArrayList;

const Point = struct {
    r: i32,
    c: i32,
};

const TraceResult = struct {
    track_positions: ArrayList(Point),
    dist_array: []i32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const input = std.mem.trim(u8, content, "\n\r ");

    var lines: ArrayList([]const u8) = .{};
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, input, '\n');
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            try lines.append(allocator, line);
        }
    }

    const grid = lines.items;
    const rows: i32 = @intCast(grid.len);
    const cols: i32 = @intCast(grid[0].len);

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

    var result = try tracePath(allocator, grid, rows, cols, start, end);
    defer result.track_positions.deinit(allocator);
    defer allocator.free(result.dist_array);

    const part1 = countCheats(result.track_positions.items, result.dist_array, cols, 2, 100);
    const part2 = countCheats(result.track_positions.items, result.dist_array, cols, 20, 100);

    const stdout = std.fs.File.stdout();
    var buf: [64]u8 = undefined;
    const output = std.fmt.bufPrint(&buf, "Part 1: {d}\nPart 2: {d}\n", .{ part1, part2 }) catch unreachable;
    _ = try stdout.write(output);
}

fn tracePath(
    allocator: std.mem.Allocator,
    grid: []const []const u8,
    rows: i32,
    cols: i32,
    start: Point,
    end: Point,
) !TraceResult {
    const dist_array = try allocator.alloc(i32, @intCast(rows * cols));
    @memset(dist_array, -1);

    var track_positions: ArrayList(Point) = .{};
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

    return .{
        .track_positions = track_positions,
        .dist_array = dist_array,
    };
}

fn countCheats(
    track_positions: []const Point,
    dist_array: []const i32,
    cols: i32,
    max_cheat_time: i32,
    min_savings: i32,
) u64 {
    var count: u64 = 0;

    for (track_positions) |p1| {
        const d1 = dist_array[@intCast(p1.r * cols + p1.c)];

        for (track_positions) |p2| {
            const cheat_cost: i32 = @intCast(@abs(p2.r - p1.r) + @abs(p2.c - p1.c));
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
