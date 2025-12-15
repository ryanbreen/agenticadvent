const std = @import("std");

const Direction = enum {
    up,
    down,
    left,
    right,

    fn fromChar(c: u8) ?Direction {
        return switch (c) {
            '^' => .up,
            'v' => .down,
            '<' => .left,
            '>' => .right,
            else => null,
        };
    }

    fn delta(self: Direction) struct { dr: i32, dc: i32 } {
        return switch (self) {
            .up => .{ .dr = -1, .dc = 0 },
            .down => .{ .dr = 1, .dc = 0 },
            .left => .{ .dr = 0, .dc = -1 },
            .right => .{ .dr = 0, .dc = 1 },
        };
    }
};

const Position = struct {
    r: i32,
    c: i32,
};

const Grid = struct {
    cells: [][]u8,
    height: usize,
    width: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, height: usize, width: usize) !Grid {
        const cells = try allocator.alloc([]u8, height);
        for (cells) |*row| {
            row.* = try allocator.alloc(u8, width);
        }
        return Grid{
            .cells = cells,
            .height = height,
            .width = width,
            .allocator = allocator,
        };
    }

    fn deinit(self: *Grid) void {
        for (self.cells) |row| {
            self.allocator.free(row);
        }
        self.allocator.free(self.cells);
    }

    fn clone(self: *const Grid, allocator: std.mem.Allocator) !Grid {
        const new_grid = try Grid.init(allocator, self.height, self.width);
        for (self.cells, 0..) |row, r| {
            @memcpy(new_grid.cells[r], row);
        }
        return new_grid;
    }

    fn get(self: *const Grid, r: i32, c: i32) u8 {
        return self.cells[@intCast(r)][@intCast(c)];
    }

    fn set(self: *Grid, r: i32, c: i32, val: u8) void {
        self.cells[@intCast(r)][@intCast(c)] = val;
    }

    fn findRobot(self: *const Grid) ?Position {
        for (self.cells, 0..) |row, r| {
            for (row, 0..) |cell, c| {
                if (cell == '@') {
                    return Position{ .r = @intCast(r), .c = @intCast(c) };
                }
            }
        }
        return null;
    }

    fn calculateGPS(self: *const Grid, box_char: u8) i64 {
        var total: i64 = 0;
        for (self.cells, 0..) |row, r| {
            for (row, 0..) |cell, c| {
                if (cell == box_char) {
                    total += @as(i64, @intCast(r)) * 100 + @as(i64, @intCast(c));
                }
            }
        }
        return total;
    }
};

fn parseInput(allocator: std.mem.Allocator, text: []const u8) !struct { grid: Grid, moves: []u8 } {
    var parts = std.mem.splitSequence(u8, text, "\n\n");
    const grid_text = parts.next() orelse return error.InvalidInput;
    const moves_text = parts.next() orelse return error.InvalidInput;

    // Parse grid
    var lines: std.ArrayList([]const u8) = .{};
    defer lines.deinit(allocator);

    var line_iter = std.mem.splitSequence(u8, grid_text, "\n");
    while (line_iter.next()) |line| {
        if (line.len > 0) {
            try lines.append(allocator, line);
        }
    }

    const height = lines.items.len;
    const width = if (height > 0) lines.items[0].len else 0;

    const grid = try Grid.init(allocator, height, width);
    for (lines.items, 0..) |line, r| {
        @memcpy(grid.cells[r], line);
    }

    // Parse moves
    var moves_list: std.ArrayList(u8) = .{};
    defer moves_list.deinit(allocator);

    var move_iter = std.mem.splitSequence(u8, moves_text, "\n");
    while (move_iter.next()) |line| {
        for (line) |c| {
            if (Direction.fromChar(c)) |_| {
                try moves_list.append(allocator, c);
            }
        }
    }

    return .{
        .grid = grid,
        .moves = try moves_list.toOwnedSlice(allocator),
    };
}

fn moveRobot(grid: *Grid, robot_pos: Position, dir: Direction) Position {
    const d = dir.delta();
    const nr = robot_pos.r + d.dr;
    const nc = robot_pos.c + d.dc;

    const target = grid.get(nr, nc);

    if (target == '#') {
        return robot_pos;
    }

    if (target == '.') {
        grid.set(robot_pos.r, robot_pos.c, '.');
        grid.set(nr, nc, '@');
        return Position{ .r = nr, .c = nc };
    }

    if (target == 'O') {
        var check_r = nr;
        var check_c = nc;

        while (grid.get(check_r, check_c) == 'O') {
            check_r += d.dr;
            check_c += d.dc;
        }

        if (grid.get(check_r, check_c) == '#') {
            return robot_pos;
        }

        grid.set(check_r, check_c, 'O');
        grid.set(robot_pos.r, robot_pos.c, '.');
        grid.set(nr, nc, '@');
        return Position{ .r = nr, .c = nc };
    }

    return robot_pos;
}

fn scaleGrid(allocator: std.mem.Allocator, grid: *const Grid) !Grid {
    var new_grid = try Grid.init(allocator, grid.height, grid.width * 2);

    for (grid.cells, 0..) |row, r| {
        var new_c: usize = 0;
        for (row) |cell| {
            if (cell == '#') {
                new_grid.cells[r][new_c] = '#';
                new_grid.cells[r][new_c + 1] = '#';
            } else if (cell == 'O') {
                new_grid.cells[r][new_c] = '[';
                new_grid.cells[r][new_c + 1] = ']';
            } else if (cell == '.') {
                new_grid.cells[r][new_c] = '.';
                new_grid.cells[r][new_c + 1] = '.';
            } else if (cell == '@') {
                new_grid.cells[r][new_c] = '@';
                new_grid.cells[r][new_c + 1] = '.';
            }
            new_c += 2;
        }
    }

    return new_grid;
}

const BoxPos = struct {
    r: i32,
    c: i32,

    pub fn eql(self: BoxPos, other: BoxPos) bool {
        return self.r == other.r and self.c == other.c;
    }
};

fn canMoveBoxVertical(grid: *const Grid, box_left_c: i32, r: i32, dr: i32) bool {
    const nr = r + dr;
    const left_c = box_left_c;
    const right_c = box_left_c + 1;

    const left_target = grid.get(nr, left_c);
    const right_target = grid.get(nr, right_c);

    if (left_target == '#' or right_target == '#') {
        return false;
    }

    var has_blocking_box = false;

    if (left_target == '[') {
        if (!canMoveBoxVertical(grid, left_c, nr, dr)) {
            return false;
        }
        has_blocking_box = true;
    } else if (left_target == ']') {
        if (!canMoveBoxVertical(grid, left_c - 1, nr, dr)) {
            return false;
        }
        has_blocking_box = true;
    }

    if (right_target == '[') {
        if (!canMoveBoxVertical(grid, right_c, nr, dr)) {
            return false;
        }
        has_blocking_box = true;
    } else if (right_target == ']' and left_target != '[') {
        // Only check if we haven't already checked this box
        if (!canMoveBoxVertical(grid, right_c - 1, nr, dr)) {
            return false;
        }
        has_blocking_box = true;
    }

    return true;
}

fn collectBoxesVertical(
    grid: *const Grid,
    box_left_c: i32,
    r: i32,
    dr: i32,
    collected: *std.AutoHashMap(BoxPos, void),
) !void {
    const box = BoxPos{ .r = r, .c = box_left_c };
    try collected.put(box, {});

    const nr = r + dr;
    const left_c = box_left_c;
    const right_c = box_left_c + 1;

    const left_target = grid.get(nr, left_c);
    const right_target = grid.get(nr, right_c);

    if (left_target == '[') {
        const next_box = BoxPos{ .r = nr, .c = left_c };
        if (!collected.contains(next_box)) {
            try collectBoxesVertical(grid, left_c, nr, dr, collected);
        }
    } else if (left_target == ']') {
        const next_box = BoxPos{ .r = nr, .c = left_c - 1 };
        if (!collected.contains(next_box)) {
            try collectBoxesVertical(grid, left_c - 1, nr, dr, collected);
        }
    }

    if (right_target == '[') {
        const next_box = BoxPos{ .r = nr, .c = right_c };
        if (!collected.contains(next_box)) {
            try collectBoxesVertical(grid, right_c, nr, dr, collected);
        }
    } else if (right_target == ']' and left_target != '[') {
        const next_box = BoxPos{ .r = nr, .c = right_c - 1 };
        if (!collected.contains(next_box)) {
            try collectBoxesVertical(grid, right_c - 1, nr, dr, collected);
        }
    }
}

fn moveRobotWide(allocator: std.mem.Allocator, grid: *Grid, robot_pos: Position, dir: Direction) !Position {
    const d = dir.delta();
    const nr = robot_pos.r + d.dr;
    const nc = robot_pos.c + d.dc;

    const target = grid.get(nr, nc);

    if (target == '#') {
        return robot_pos;
    }

    if (target == '.') {
        grid.set(robot_pos.r, robot_pos.c, '.');
        grid.set(nr, nc, '@');
        return Position{ .r = nr, .c = nc };
    }

    if (target == '[' or target == ']') {
        if (d.dc != 0) { // Horizontal movement
            var check_c = nc;
            while (grid.get(robot_pos.r, check_c) == '[' or grid.get(robot_pos.r, check_c) == ']') {
                check_c += d.dc;
            }

            if (grid.get(robot_pos.r, check_c) == '#') {
                return robot_pos;
            }

            // Shift all boxes
            if (d.dc > 0) { // Moving right
                var col = check_c;
                while (col > nc) : (col -= 1) {
                    grid.set(robot_pos.r, col, grid.get(robot_pos.r, col - 1));
                }
            } else { // Moving left
                var col = check_c;
                while (col < nc) : (col += 1) {
                    grid.set(robot_pos.r, col, grid.get(robot_pos.r, col + 1));
                }
            }

            grid.set(robot_pos.r, robot_pos.c, '.');
            grid.set(nr, nc, '@');
            return Position{ .r = nr, .c = nc };
        } else { // Vertical movement
            const box_left_c = if (target == '[') nc else nc - 1;

            if (!canMoveBoxVertical(grid, box_left_c, nr, d.dr)) {
                return robot_pos;
            }

            var boxes_to_move = std.AutoHashMap(BoxPos, void).init(allocator);
            defer boxes_to_move.deinit();

            try collectBoxesVertical(grid, box_left_c, nr, d.dr, &boxes_to_move);

            // Sort boxes by row
            var box_list: std.ArrayList(BoxPos) = .{};
            defer box_list.deinit(allocator);

            var iter = boxes_to_move.keyIterator();
            while (iter.next()) |box| {
                try box_list.append(allocator, box.*);
            }

            const boxes = box_list.items;
            if (d.dr > 0) {
                std.sort.pdq(BoxPos, boxes, {}, struct {
                    fn lessThan(_: void, a: BoxPos, b: BoxPos) bool {
                        return a.r > b.r;
                    }
                }.lessThan);
            } else {
                std.sort.pdq(BoxPos, boxes, {}, struct {
                    fn lessThan(_: void, a: BoxPos, b: BoxPos) bool {
                        return a.r < b.r;
                    }
                }.lessThan);
            }

            // Move all boxes
            for (boxes) |box| {
                grid.set(box.r, box.c, '.');
                grid.set(box.r, box.c + 1, '.');
                grid.set(box.r + d.dr, box.c, '[');
                grid.set(box.r + d.dr, box.c + 1, ']');
            }

            // Move robot
            grid.set(robot_pos.r, robot_pos.c, '.');
            grid.set(nr, nc, '@');
            return Position{ .r = nr, .c = nc };
        }
    }

    return robot_pos;
}

fn part1(allocator: std.mem.Allocator, text: []const u8) !i64 {
    var parsed = try parseInput(allocator, text);
    defer parsed.grid.deinit();
    defer allocator.free(parsed.moves);

    var robot_pos = parsed.grid.findRobot() orelse return error.NoRobot;

    for (parsed.moves) |move_char| {
        const dir = Direction.fromChar(move_char) orelse continue;
        robot_pos = moveRobot(&parsed.grid, robot_pos, dir);
    }

    return parsed.grid.calculateGPS('O');
}

fn part2(allocator: std.mem.Allocator, text: []const u8) !i64 {
    var parsed = try parseInput(allocator, text);
    defer parsed.grid.deinit();
    defer allocator.free(parsed.moves);

    var wide_grid = try scaleGrid(allocator, &parsed.grid);
    defer wide_grid.deinit();

    var robot_pos = wide_grid.findRobot() orelse return error.NoRobot;

    for (parsed.moves) |move_char| {
        const dir = Direction.fromChar(move_char) orelse continue;
        robot_pos = try moveRobotWide(allocator, &wide_grid, robot_pos, dir);
    }

    return wide_grid.calculateGPS('[');
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "../input.txt";
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const text = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(text);

    const trimmed = std.mem.trim(u8, text, &std.ascii.whitespace);

    const p1 = try part1(allocator, trimmed);
    const p2 = try part2(allocator, trimmed);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
