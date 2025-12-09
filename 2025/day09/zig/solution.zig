const std = @import("std");
const Allocator = std.mem.Allocator;

const Point = struct {
    x: i32,
    y: i32,
};

const HorizontalEdge = struct {
    y: i32,
    x_min: i32,
    x_max: i32,
};

const VerticalEdge = struct {
    x: i32,
    y_min: i32,
    y_max: i32,
};

fn part1(points: []const Point) i64 {
    var max_area: i64 = 0;

    var i: usize = 0;
    while (i < points.len) : (i += 1) {
        const x1 = points[i].x;
        const y1 = points[i].y;

        var j: usize = i + 1;
        while (j < points.len) : (j += 1) {
            const x2 = points[j].x;
            const y2 = points[j].y;

            const width = @abs(x2 - x1) + 1;
            const height = @abs(y2 - y1) + 1;
            const area = @as(i64, width) * @as(i64, height);

            if (area > max_area) {
                max_area = area;
            }
        }
    }

    return max_area;
}

fn isInsidePolygon(x: f64, y: f64, vert_edges: []const VerticalEdge) bool {
    var crossings: f64 = 0.0;

    for (vert_edges) |edge| {
        if (@as(f64, @floatFromInt(edge.x)) <= x) continue;

        const y_min_f = @as(f64, @floatFromInt(edge.y_min));
        const y_max_f = @as(f64, @floatFromInt(edge.y_max));

        if (y_min_f < y and y < y_max_f) {
            crossings += 1.0;
        } else if (y == y_min_f or y == y_max_f) {
            crossings += 0.5;
        }
    }

    const remainder = @mod(crossings, 2.0);
    return remainder >= 0.5; // Close enough to 1
}

fn rectangleValid(
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
    vert_edges: []const VerticalEdge,
    horiz_edges: []const HorizontalEdge,
) bool {
    const min_x = @min(x1, x2);
    const max_x = @max(x1, x2);
    const min_y = @min(y1, y2);
    const max_y = @max(y1, y2);

    // Check if any vertical edge crosses through the rectangle interior
    for (vert_edges) |edge| {
        if (min_x < edge.x and edge.x < max_x) {
            // Check if this edge segment overlaps with rectangle's y range
            if (!(edge.y_max <= min_y or edge.y_min >= max_y)) {
                return false;
            }
        }
    }

    // Check if any horizontal edge crosses through the rectangle interior
    for (horiz_edges) |edge| {
        if (min_y < edge.y and edge.y < max_y) {
            // Check if this edge segment overlaps with rectangle's x range
            if (!(edge.x_max <= min_x or edge.x_min >= max_x)) {
                return false;
            }
        }
    }

    // Check that center point is inside the polygon
    const center_x = @as(f64, @floatFromInt(min_x + max_x)) / 2.0;
    const center_y = @as(f64, @floatFromInt(min_y + max_y)) / 2.0;
    return isInsidePolygon(center_x, center_y, vert_edges);
}

fn part2(allocator: Allocator, points: []const Point) !i64 {
    // Pre-allocate arrays since we know the size (n edges)
    const n = points.len;
    const horizontal_edges = try allocator.alloc(HorizontalEdge, n);
    defer allocator.free(horizontal_edges);
    const vertical_edges = try allocator.alloc(VerticalEdge, n);
    defer allocator.free(vertical_edges);

    var h_count: usize = 0;
    var v_count: usize = 0;

    // Build edges connecting consecutive points
    var i: usize = 0;
    while (i < n) : (i += 1) {
        const x1 = points[i].x;
        const y1 = points[i].y;
        const next = (i + 1) % n;
        const x2 = points[next].x;
        const y2 = points[next].y;

        if (y1 == y2) {
            // Horizontal edge
            horizontal_edges[h_count] = .{
                .y = y1,
                .x_min = @min(x1, x2),
                .x_max = @max(x1, x2),
            };
            h_count += 1;
        } else {
            // Vertical edge
            vertical_edges[v_count] = .{
                .x = x1,
                .y_min = @min(y1, y2),
                .y_max = @max(y1, y2),
            };
            v_count += 1;
        }
    }

    // Sort vertical edges by x coordinate for ray casting
    const vert_items = vertical_edges[0..v_count];
    std.mem.sort(VerticalEdge, vert_items, {}, struct {
        fn lessThan(_: void, a: VerticalEdge, b: VerticalEdge) bool {
            return a.x < b.x;
        }
    }.lessThan);

    const horiz_items = horizontal_edges[0..h_count];

    // Find largest valid rectangle
    var max_area: i64 = 0;

    i = 0;
    while (i < points.len) : (i += 1) {
        const x1 = points[i].x;
        const y1 = points[i].y;

        var j: usize = i + 1;
        while (j < points.len) : (j += 1) {
            const x2 = points[j].x;
            const y2 = points[j].y;

            if (rectangleValid(x1, y1, x2, y2, vert_items, horiz_items)) {
                const width = @abs(x2 - x1) + 1;
                const height = @abs(y2 - y1) + 1;
                const area = @as(i64, width) * @as(i64, height);

                if (area > max_area) {
                    max_area = area;
                }
            }
        }
    }

    return max_area;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    // Count lines
    var line_count: usize = 0;
    var line_iter1 = std.mem.tokenizeScalar(u8, buffer, '\n');
    while (line_iter1.next()) |_| {
        line_count += 1;
    }

    // Parse input
    const points = try allocator.alloc(Point, line_count);
    defer allocator.free(points);

    var idx: usize = 0;
    var lines = std.mem.tokenizeScalar(u8, buffer, '\n');
    while (lines.next()) |line| {
        var parts = std.mem.tokenizeScalar(u8, line, ',');
        const x_str = parts.next() orelse continue;
        const y_str = parts.next() orelse continue;

        const x = try std.fmt.parseInt(i32, x_str, 10);
        const y = try std.fmt.parseInt(i32, y_str, 10);

        points[idx] = .{ .x = x, .y = y };
        idx += 1;
    }

    const p1 = part1(points);
    const p2 = try part2(allocator, points);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
