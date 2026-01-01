const std = @import("std");

const Instruction = struct {
    direction: u8,
    distance: i64,
    color: [6]u8,
};

const Direction = struct {
    dr: i64,
    dc: i64,
};

fn getDirection(dir: u8) Direction {
    return switch (dir) {
        'R' => .{ .dr = 0, .dc = 1 },
        'D' => .{ .dr = 1, .dc = 0 },
        'L' => .{ .dr = 0, .dc = -1 },
        'U' => .{ .dr = -1, .dc = 0 },
        else => unreachable,
    };
}

fn getDirectionFromHex(digit: u8) Direction {
    return switch (digit) {
        '0' => .{ .dr = 0, .dc = 1 },   // R
        '1' => .{ .dr = 1, .dc = 0 },   // D
        '2' => .{ .dr = 0, .dc = -1 },  // L
        '3' => .{ .dr = -1, .dc = 0 },  // U
        else => unreachable,
    };
}

fn hexToInt(hex: []const u8) i64 {
    var result: i64 = 0;
    for (hex) |c| {
        result *= 16;
        if (c >= '0' and c <= '9') {
            result += @as(i64, c - '0');
        } else if (c >= 'a' and c <= 'f') {
            result += @as(i64, c - 'a' + 10);
        } else if (c >= 'A' and c <= 'F') {
            result += @as(i64, c - 'A' + 10);
        }
    }
    return result;
}

fn calculateArea(vertices: []const [2]i64, perimeter: i64) i64 {
    // Shoelace formula for polygon area
    var area: i128 = 0;
    const n = vertices.len;
    for (0..n) |i| {
        const j = (i + 1) % n;
        area += @as(i128, vertices[i][0]) * @as(i128, vertices[j][1]);
        area -= @as(i128, vertices[j][0]) * @as(i128, vertices[i][1]);
    }
    if (area < 0) area = -area;
    const half_area: i64 = @intCast(@divTrunc(area, 2));

    // Total points = interior + boundary
    // From Pick's theorem: interior = area - boundary/2 + 1
    // Total = interior + boundary = area + boundary/2 + 1
    return half_area + @divTrunc(perimeter, 2) + 1;
}

fn part1(instructions: []const Instruction) i64 {
    var vertices_list: [1024][2]i64 = undefined;
    var vertex_count: usize = 0;
    var perimeter: i64 = 0;
    var r: i64 = 0;
    var c: i64 = 0;

    vertices_list[vertex_count] = .{ r, c };
    vertex_count += 1;

    for (instructions) |instr| {
        const dir = getDirection(instr.direction);
        r += dir.dr * instr.distance;
        c += dir.dc * instr.distance;
        vertices_list[vertex_count] = .{ r, c };
        vertex_count += 1;
        perimeter += instr.distance;
    }

    return calculateArea(vertices_list[0..vertex_count], perimeter);
}

fn part2(instructions: []const Instruction) i64 {
    var vertices_list: [1024][2]i64 = undefined;
    var vertex_count: usize = 0;
    var perimeter: i64 = 0;
    var r: i64 = 0;
    var c: i64 = 0;

    vertices_list[vertex_count] = .{ r, c };
    vertex_count += 1;

    for (instructions) |instr| {
        // First 5 hex digits = distance, last digit = direction
        const distance = hexToInt(instr.color[0..5]);
        const dir = getDirectionFromHex(instr.color[5]);
        r += dir.dr * distance;
        c += dir.dc * distance;
        vertices_list[vertex_count] = .{ r, c };
        vertex_count += 1;
        perimeter += distance;
    }

    return calculateArea(vertices_list[0..vertex_count], perimeter);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse instructions using fixed-size array (input has ~750 lines max)
    var instructions: [1024]Instruction = undefined;
    var instr_count: usize = 0;

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse: "R 6 (#70c710)"
        var parts = std.mem.splitScalar(u8, line, ' ');
        const dir_str = parts.next() orelse continue;
        const dist_str = parts.next() orelse continue;
        const color_str = parts.next() orelse continue;

        const direction = dir_str[0];
        const distance = std.fmt.parseInt(i64, dist_str, 10) catch continue;

        // Extract hex color from "(#XXXXXX)"
        var color: [6]u8 = undefined;
        if (color_str.len >= 9) {
            @memcpy(&color, color_str[2..8]);
        }

        instructions[instr_count] = .{
            .direction = direction,
            .distance = distance,
            .color = color,
        };
        instr_count += 1;
    }

    std.debug.print("Part 1: {d}\n", .{part1(instructions[0..instr_count])});
    std.debug.print("Part 2: {d}\n", .{part2(instructions[0..instr_count])});
}
