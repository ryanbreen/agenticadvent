const std = @import("std");

const Position = struct {
    row: i32,
    col: i32,
};

const PositionList = std.ArrayList(Position);
const PositionSet = std.AutoHashMap(Position, void);
const AntennaMap = std.AutoHashMap(u8, PositionList);

fn inBounds(row: i32, col: i32, rows: i32, cols: i32) bool {
    return row >= 0 and row < rows and col >= 0 and col < cols;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const part1_result = try part1(allocator);
    const part2_result = try part2(allocator);

    std.debug.print("Part 1: {d}\n", .{part1_result});
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn parseInput(allocator: std.mem.Allocator) !struct { rows: i32, cols: i32, antennas: AntennaMap } {
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Count rows and cols, and collect antennas
    var rows: i32 = 0;
    var cols: i32 = 0;
    var antennas = AntennaMap.init(allocator);

    var line_iter = std.mem.splitScalar(u8, content, '\n');
    var row: i32 = 0;
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            if (cols == 0) {
                cols = @intCast(line.len);
            }

            for (line, 0..) |ch, c| {
                if (ch != '.') {
                    const entry = try antennas.getOrPut(ch);
                    if (!entry.found_existing) {
                        entry.value_ptr.* = .{};
                    }
                    try entry.value_ptr.append(allocator, Position{
                        .row = row,
                        .col = @intCast(c),
                    });
                }
            }

            row += 1;
        }
    }
    rows = row;

    return .{ .rows = rows, .cols = cols, .antennas = antennas };
}

fn part1(allocator: std.mem.Allocator) !usize {
    const input = try parseInput(allocator);
    const rows = input.rows;
    const cols = input.cols;
    var antennas = input.antennas;
    defer {
        var iter = antennas.valueIterator();
        while (iter.next()) |list| {
            list.deinit(allocator);
        }
        antennas.deinit();
    }

    var antinodes = PositionSet.init(allocator);
    defer antinodes.deinit();

    var freq_iter = antennas.iterator();
    while (freq_iter.next()) |entry| {
        const positions = entry.value_ptr.items;

        // For each pair of antennas with same frequency
        for (positions, 0..) |pos1, i| {
            for (positions[i + 1 ..]) |pos2| {
                // Calculate the two antinodes
                // Antinode beyond antenna 1 (away from antenna 2)
                const ar1 = 2 * pos1.row - pos2.row;
                const ac1 = 2 * pos1.col - pos2.col;
                // Antinode beyond antenna 2 (away from antenna 1)
                const ar2 = 2 * pos2.row - pos1.row;
                const ac2 = 2 * pos2.col - pos1.col;

                // Add if within bounds
                if (inBounds(ar1, ac1, rows, cols)) {
                    try antinodes.put(Position{ .row = ar1, .col = ac1 }, {});
                }
                if (inBounds(ar2, ac2, rows, cols)) {
                    try antinodes.put(Position{ .row = ar2, .col = ac2 }, {});
                }
            }
        }
    }

    return antinodes.count();
}

fn part2(allocator: std.mem.Allocator) !usize {
    const input = try parseInput(allocator);
    const rows = input.rows;
    const cols = input.cols;
    var antennas = input.antennas;
    defer {
        var iter = antennas.valueIterator();
        while (iter.next()) |list| {
            list.deinit(allocator);
        }
        antennas.deinit();
    }

    var antinodes = PositionSet.init(allocator);
    defer antinodes.deinit();

    var freq_iter = antennas.iterator();
    while (freq_iter.next()) |entry| {
        const positions = entry.value_ptr.items;

        // For each pair of antennas with same frequency
        for (positions, 0..) |pos1, i| {
            for (positions[i + 1 ..]) |pos2| {
                const dr = pos2.row - pos1.row;
                const dc = pos2.col - pos1.col;

                // Extend in both directions along the line
                // Direction 1: from antenna 1 towards and beyond antenna 2
                var r = pos1.row;
                var c = pos1.col;
                while (inBounds(r, c, rows, cols)) {
                    try antinodes.put(Position{ .row = r, .col = c }, {});
                    r += dr;
                    c += dc;
                }

                // Direction 2: from antenna 1 away from antenna 2
                r = pos1.row - dr;
                c = pos1.col - dc;
                while (inBounds(r, c, rows, cols)) {
                    try antinodes.put(Position{ .row = r, .col = c }, {});
                    r -= dr;
                    c -= dc;
                }
            }
        }
    }

    return antinodes.count();
}
