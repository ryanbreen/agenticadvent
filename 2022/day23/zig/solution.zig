const std = @import("std");

const Coord = struct {
    r: i32,
    c: i32,

    fn hash(self: Coord) u64 {
        const r_u: u64 = @bitCast(@as(i64, self.r));
        const c_u: u64 = @bitCast(@as(i64, self.c));
        return r_u *% 0x9e3779b97f4a7c15 +% c_u;
    }

    fn eql(a: Coord, b: Coord) bool {
        return a.r == b.r and a.c == b.c;
    }
};

const CoordSet = std.HashMap(Coord, void, struct {
    pub fn hash(_: @This(), key: Coord) u64 {
        return key.hash();
    }
    pub fn eql(_: @This(), a: Coord, b: Coord) bool {
        return Coord.eql(a, b);
    }
}, std.hash_map.default_max_load_percentage);

const CoordCountMap = std.HashMap(Coord, u32, struct {
    pub fn hash(_: @This(), key: Coord) u64 {
        return key.hash();
    }
    pub fn eql(_: @This(), a: Coord, b: Coord) bool {
        return Coord.eql(a, b);
    }
}, std.hash_map.default_max_load_percentage);

const Direction = enum { N, S, W, E };

// Direction checks: for each direction, the 3 positions to check and the move delta
fn getDirectionChecks(dir: Direction) struct { checks: [3][2]i32, delta: [2]i32 } {
    return switch (dir) {
        .N => .{ .checks = .{ .{ -1, -1 }, .{ -1, 0 }, .{ -1, 1 } }, .delta = .{ -1, 0 } },
        .S => .{ .checks = .{ .{ 1, -1 }, .{ 1, 0 }, .{ 1, 1 } }, .delta = .{ 1, 0 } },
        .W => .{ .checks = .{ .{ -1, -1 }, .{ 0, -1 }, .{ 1, -1 } }, .delta = .{ 0, -1 } },
        .E => .{ .checks = .{ .{ -1, 1 }, .{ 0, 1 }, .{ 1, 1 } }, .delta = .{ 0, 1 } },
    };
}

// All 8 neighbor offsets
const all_neighbors: [8][2]i32 = .{
    .{ -1, -1 }, .{ -1, 0 }, .{ -1, 1 },
    .{ 0, -1 },  .{ 0, 1 },
    .{ 1, -1 },  .{ 1, 0 },  .{ 1, 1 },
};

fn parseInput(allocator: std.mem.Allocator, text: []const u8) !CoordSet {
    var elves = CoordSet.init(allocator);

    var r: i32 = 0;
    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        for (line, 0..) |ch, col| {
            if (ch == '#') {
                try elves.put(.{ .r = r, .c = @intCast(col) }, {});
            }
        }
        r += 1;
    }

    return elves;
}

fn hasNeighbor(elves: *const CoordSet, r: i32, c: i32) bool {
    for (all_neighbors) |offset| {
        const nr = r + offset[0];
        const nc = c + offset[1];
        if (elves.contains(.{ .r = nr, .c = nc })) {
            return true;
        }
    }
    return false;
}

fn canMove(elves: *const CoordSet, r: i32, c: i32, dir: Direction) bool {
    const info = getDirectionChecks(dir);
    for (info.checks) |check| {
        if (elves.contains(.{ .r = r + check[0], .c = c + check[1] })) {
            return false;
        }
    }
    return true;
}

fn simulateRound(allocator: std.mem.Allocator, elves: *CoordSet, directions: [4]Direction) !struct { moved: bool } {
    // Phase 1: Each elf proposes a move
    var proposals = CoordSet.init(allocator);
    defer proposals.deinit();

    var proposal_counts = CoordCountMap.init(allocator);
    defer proposal_counts.deinit();

    // Store proposals: elf coord -> proposed coord
    var elf_proposals = std.HashMap(Coord, Coord, struct {
        pub fn hash(_: @This(), key: Coord) u64 {
            return key.hash();
        }
        pub fn eql(_: @This(), a: Coord, b: Coord) bool {
            return Coord.eql(a, b);
        }
    }, std.hash_map.default_max_load_percentage).init(allocator);
    defer elf_proposals.deinit();

    var iter = elves.iterator();
    while (iter.next()) |entry| {
        const elf = entry.key_ptr.*;
        const r = elf.r;
        const c = elf.c;

        // Check if any neighbors
        if (!hasNeighbor(elves, r, c)) {
            continue; // Don't move
        }

        // Try each direction
        for (directions) |dir| {
            if (canMove(elves, r, c, dir)) {
                const info = getDirectionChecks(dir);
                const new_pos = Coord{ .r = r + info.delta[0], .c = c + info.delta[1] };
                try elf_proposals.put(elf, new_pos);
                const gop = try proposal_counts.getOrPut(new_pos);
                if (!gop.found_existing) {
                    gop.value_ptr.* = 0;
                }
                gop.value_ptr.* += 1;
                break;
            }
        }
    }

    // Phase 2: Execute moves (only if unique proposal)
    var new_elves = CoordSet.init(allocator);
    var moved = false;

    iter = elves.iterator();
    while (iter.next()) |entry| {
        const elf = entry.key_ptr.*;

        if (elf_proposals.get(elf)) |new_pos| {
            if (proposal_counts.get(new_pos).? == 1) {
                try new_elves.put(new_pos, {});
                moved = true;
            } else {
                try new_elves.put(elf, {});
            }
        } else {
            try new_elves.put(elf, {});
        }
    }

    // Swap the sets
    elves.deinit();
    elves.* = new_elves;

    return .{ .moved = moved };
}

fn boundingRectEmpty(elves: *const CoordSet) i64 {
    var min_r: i32 = std.math.maxInt(i32);
    var max_r: i32 = std.math.minInt(i32);
    var min_c: i32 = std.math.maxInt(i32);
    var max_c: i32 = std.math.minInt(i32);

    var iter = elves.iterator();
    while (iter.next()) |entry| {
        const coord = entry.key_ptr.*;
        min_r = @min(min_r, coord.r);
        max_r = @max(max_r, coord.r);
        min_c = @min(min_c, coord.c);
        max_c = @max(max_c, coord.c);
    }

    const area: i64 = @as(i64, max_r - min_r + 1) * @as(i64, max_c - min_c + 1);
    return area - @as(i64, @intCast(elves.count()));
}

fn rotateDirections(directions: [4]Direction) [4]Direction {
    return .{ directions[1], directions[2], directions[3], directions[0] };
}

fn part1(allocator: std.mem.Allocator, text: []const u8) !i64 {
    var elves = try parseInput(allocator, text);
    defer elves.deinit();

    var directions: [4]Direction = .{ .N, .S, .W, .E };

    for (0..10) |_| {
        _ = try simulateRound(allocator, &elves, directions);
        directions = rotateDirections(directions);
    }

    return boundingRectEmpty(&elves);
}

fn part2(allocator: std.mem.Allocator, text: []const u8) !u32 {
    var elves = try parseInput(allocator, text);
    defer elves.deinit();

    var directions: [4]Direction = .{ .N, .S, .W, .E };

    var round_num: u32 = 0;
    while (true) {
        round_num += 1;
        const result = try simulateRound(allocator, &elves, directions);
        if (!result.moved) {
            return round_num;
        }
        directions = rotateDirections(directions);
    }
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

    const text = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(text);

    const p1 = try part1(allocator, text);
    const p2 = try part2(allocator, text);

    // Use posix write to stdout
    const stdout_fd = std.posix.STDOUT_FILENO;
    var buf1: [64]u8 = undefined;
    const msg1 = std.fmt.bufPrint(&buf1, "Part 1: {d}\n", .{p1}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg1) catch {};

    var buf2: [64]u8 = undefined;
    const msg2 = std.fmt.bufPrint(&buf2, "Part 2: {d}\n", .{p2}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg2) catch {};
}
