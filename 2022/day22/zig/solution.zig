const std = @import("std");

const Direction = enum(u8) {
    right = 0,
    down = 1,
    left = 2,
    up = 3,

    fn turn_right(self: Direction) Direction {
        return @enumFromInt((@intFromEnum(self) + 1) % 4);
    }

    fn turn_left(self: Direction) Direction {
        return @enumFromInt((@intFromEnum(self) + 3) % 4);
    }

    fn dr(self: Direction) i32 {
        const deltas = [_]i32{ 0, 1, 0, -1 };
        return deltas[@intFromEnum(self)];
    }

    fn dc(self: Direction) i32 {
        const deltas = [_]i32{ 1, 0, -1, 0 };
        return deltas[@intFromEnum(self)];
    }
};

const Instruction = union(enum) {
    move: u32,
    turn_left: void,
    turn_right: void,
};

const MAX_INSTRUCTIONS = 10000;
const MAX_ROWS = 300;

fn parse_instructions(path: []const u8, out: *[MAX_INSTRUCTIONS]Instruction) usize {
    var count: usize = 0;
    var i: usize = 0;

    while (i < path.len and count < MAX_INSTRUCTIONS) {
        if (path[i] >= '0' and path[i] <= '9') {
            var j = i;
            while (j < path.len and path[j] >= '0' and path[j] <= '9') {
                j += 1;
            }
            const num = std.fmt.parseInt(u32, path[i..j], 10) catch 0;
            out[count] = .{ .move = num };
            count += 1;
            i = j;
        } else if (path[i] == 'L') {
            out[count] = .turn_left;
            count += 1;
            i += 1;
        } else if (path[i] == 'R') {
            out[count] = .turn_right;
            count += 1;
            i += 1;
        } else {
            i += 1;
        }
    }

    return count;
}

const Grid = struct {
    rows: [MAX_ROWS][]const u8,
    row_count: usize,
    width: usize,

    fn get_cell(self: *const Grid, row: i32, col: i32) u8 {
        if (row < 0 or col < 0) return ' ';
        const r: usize = @intCast(row);
        const c: usize = @intCast(col);
        if (r >= self.row_count) return ' ';
        if (c >= self.rows[r].len) return ' ';
        return self.rows[r][c];
    }
};

fn part1(grid: *const Grid, instructions: []const Instruction) i64 {
    const height: i32 = @intCast(grid.row_count);
    const width: i32 = @intCast(grid.width);

    // Find starting position
    var col: i32 = 0;
    for (grid.rows[0], 0..) |c, idx| {
        if (c == '.') {
            col = @intCast(idx);
            break;
        }
    }
    var row: i32 = 0;
    var facing = Direction.right;

    for (instructions) |instr| {
        switch (instr) {
            .move => |steps| {
                for (0..steps) |_| {
                    const dr = facing.dr();
                    const dc = facing.dc();
                    var nr = row + dr;
                    var nc = col + dc;

                    // Wrap around if needed
                    if (facing == .right) {
                        if (nc >= width or grid.get_cell(nr, nc) == ' ') {
                            nc = 0;
                            while (grid.get_cell(nr, nc) == ' ') {
                                nc += 1;
                            }
                        }
                    } else if (facing == .left) {
                        if (nc < 0 or grid.get_cell(nr, nc) == ' ') {
                            nc = width - 1;
                            while (grid.get_cell(nr, nc) == ' ') {
                                nc -= 1;
                            }
                        }
                    } else if (facing == .down) {
                        if (nr >= height or grid.get_cell(nr, nc) == ' ') {
                            nr = 0;
                            while (grid.get_cell(nr, nc) == ' ') {
                                nr += 1;
                            }
                        }
                    } else if (facing == .up) {
                        if (nr < 0 or grid.get_cell(nr, nc) == ' ') {
                            nr = height - 1;
                            while (grid.get_cell(nr, nc) == ' ') {
                                nr -= 1;
                            }
                        }
                    }

                    // Check if we hit a wall
                    if (grid.get_cell(nr, nc) == '#') {
                        break;
                    }

                    row = nr;
                    col = nc;
                }
            },
            .turn_left => {
                facing = facing.turn_left();
            },
            .turn_right => {
                facing = facing.turn_right();
            },
        }
    }

    return 1000 * (row + 1) + 4 * (col + 1) + @as(i64, @intFromEnum(facing));
}

fn get_cube_face(row: i32, col: i32, face_size: i32) struct { face: i32, lr: i32, lc: i32 } {
    const face_row = @divTrunc(row, face_size);
    const face_col = @divTrunc(col, face_size);
    const lr = @mod(row, face_size);
    const lc = @mod(col, face_size);

    // Cube layout:
    //   12
    //   3
    //  45
    //  6
    const face: i32 = if (face_row == 0 and face_col == 1) 1
    else if (face_row == 0 and face_col == 2) 2
    else if (face_row == 1 and face_col == 1) 3
    else if (face_row == 2 and face_col == 0) 4
    else if (face_row == 2 and face_col == 1) 5
    else if (face_row == 3 and face_col == 0) 6
    else -1;

    return .{ .face = face, .lr = lr, .lc = lc };
}

fn wrap_cube(row: i32, col: i32, facing: Direction, face_size: i32) struct { nr: i32, nc: i32, nf: Direction } {
    const S = face_size;
    const cube_info = get_cube_face(row, col, S);
    const face = cube_info.face;
    const lr = cube_info.lr;
    const lc = cube_info.lc;

    // Wrapping rules for the actual input layout
    if (face == 1) {
        if (facing == .up) {
            // Goes to face 6, from left, facing right
            return .{ .nr = 3 * S + lc, .nc = 0, .nf = .right };
        } else if (facing == .left) {
            // Goes to face 4, from left, facing right (inverted)
            return .{ .nr = 3 * S - 1 - lr, .nc = 0, .nf = .right };
        }
    } else if (face == 2) {
        if (facing == .right) {
            // Goes to face 5, from right, facing left (inverted)
            return .{ .nr = 3 * S - 1 - lr, .nc = 2 * S - 1, .nf = .left };
        } else if (facing == .down) {
            // Goes to face 3, from right, facing left
            return .{ .nr = S + lc, .nc = 2 * S - 1, .nf = .left };
        } else if (facing == .up) {
            // Goes to face 6, from bottom, facing up
            return .{ .nr = 4 * S - 1, .nc = lc, .nf = .up };
        }
    } else if (face == 3) {
        if (facing == .right) {
            // Goes to face 2, from bottom, facing up
            return .{ .nr = S - 1, .nc = 2 * S + lr, .nf = .up };
        } else if (facing == .left) {
            // Goes to face 4, from top, facing down
            return .{ .nr = 2 * S, .nc = lr, .nf = .down };
        }
    } else if (face == 4) {
        if (facing == .up) {
            // Goes to face 3, from left, facing right
            return .{ .nr = S + lc, .nc = S, .nf = .right };
        } else if (facing == .left) {
            // Goes to face 1, from left, facing right (inverted)
            return .{ .nr = S - 1 - lr, .nc = S, .nf = .right };
        }
    } else if (face == 5) {
        if (facing == .right) {
            // Goes to face 2, from right, facing left (inverted)
            return .{ .nr = S - 1 - lr, .nc = 3 * S - 1, .nf = .left };
        } else if (facing == .down) {
            // Goes to face 6, from right, facing left
            return .{ .nr = 3 * S + lc, .nc = S - 1, .nf = .left };
        }
    } else if (face == 6) {
        if (facing == .right) {
            // Goes to face 5, from bottom, facing up
            return .{ .nr = 3 * S - 1, .nc = S + lr, .nf = .up };
        } else if (facing == .down) {
            // Goes to face 2, from top, facing down
            return .{ .nr = 0, .nc = 2 * S + lc, .nf = .down };
        } else if (facing == .left) {
            // Goes to face 1, from top, facing down
            return .{ .nr = 0, .nc = S + lr, .nf = .down };
        }
    }

    // Shouldn't reach here
    return .{ .nr = row, .nc = col, .nf = facing };
}

fn part2(grid: *const Grid, instructions: []const Instruction) i64 {
    const height: i32 = @intCast(grid.row_count);
    const width: i32 = @intCast(grid.width);

    // Determine face size
    const face_size: i32 = if (height > 50) 50 else 4;

    // Find starting position
    var col: i32 = 0;
    for (grid.rows[0], 0..) |c, idx| {
        if (c == '.') {
            col = @intCast(idx);
            break;
        }
    }
    var row: i32 = 0;
    var facing = Direction.right;

    for (instructions) |instr| {
        switch (instr) {
            .move => |steps| {
                for (0..steps) |_| {
                    const dr = facing.dr();
                    const dc = facing.dc();
                    var nr = row + dr;
                    var nc = col + dc;
                    var nf = facing;

                    // Check if we need to wrap
                    var need_wrap = false;
                    if (nr < 0 or nr >= height or nc < 0 or nc >= width) {
                        need_wrap = true;
                    } else if (grid.get_cell(nr, nc) == ' ') {
                        need_wrap = true;
                    }

                    if (need_wrap) {
                        const wrap_result = wrap_cube(row, col, facing, face_size);
                        nr = wrap_result.nr;
                        nc = wrap_result.nc;
                        nf = wrap_result.nf;
                    }

                    // Check if we hit a wall
                    if (grid.get_cell(nr, nc) == '#') {
                        break;
                    }

                    row = nr;
                    col = nc;
                    facing = nf;
                }
            },
            .turn_left => {
                facing = facing.turn_left();
            },
            .turn_right => {
                facing = facing.turn_right();
            },
        }
    }

    return 1000 * (row + 1) + 4 * (col + 1) + @as(i64, @intFromEnum(facing));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = std.fs.cwd().openFile("../input.txt", .{}) catch |err| {
        std.debug.print("Failed to open ../input.txt: {}\n", .{err});
        return err;
    };
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Split into grid and path
    var parts = std.mem.splitSequence(u8, content, "\n\n");
    const grid_text = parts.first();
    const path_text = std.mem.trim(u8, parts.rest(), "\n\r ");

    // Parse grid
    var grid: Grid = undefined;
    grid.row_count = 0;
    grid.width = 0;

    var lines = std.mem.splitScalar(u8, grid_text, '\n');
    while (lines.next()) |line| {
        if (grid.row_count < MAX_ROWS) {
            grid.rows[grid.row_count] = line;
            if (line.len > grid.width) grid.width = line.len;
            grid.row_count += 1;
        }
    }

    // Parse instructions
    var instructions: [MAX_INSTRUCTIONS]Instruction = undefined;
    const instr_count = parse_instructions(path_text, &instructions);

    const p1 = part1(&grid, instructions[0..instr_count]);
    const p2 = part2(&grid, instructions[0..instr_count]);

    // Use posix write to stdout
    const stdout_fd = std.posix.STDOUT_FILENO;
    var buf1: [64]u8 = undefined;
    const msg1 = std.fmt.bufPrint(&buf1, "Part 1: {d}\n", .{p1}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg1) catch {};

    var buf2: [64]u8 = undefined;
    const msg2 = std.fmt.bufPrint(&buf2, "Part 2: {d}\n", .{p2}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg2) catch {};
}
