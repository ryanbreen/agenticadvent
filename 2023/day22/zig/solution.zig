const std = @import("std");
const Allocator = std.mem.Allocator;

const Brick = struct {
    x1: i32,
    y1: i32,
    z1: i32,
    x2: i32,
    y2: i32,
    z2: i32,
    orig_idx: usize,
};

const Cell = struct {
    x: i32,
    y: i32,
    z: i32,
};

fn cellHash(cell: Cell) u64 {
    const x: u64 = @bitCast(@as(i64, cell.x));
    const y: u64 = @bitCast(@as(i64, cell.y));
    const z: u64 = @bitCast(@as(i64, cell.z));
    return x *% 73856093 +% y *% 19349663 +% z *% 83492791;
}

fn cellEql(a: Cell, b: Cell) bool {
    return a.x == b.x and a.y == b.y and a.z == b.z;
}

const CellContext = struct {
    pub fn hash(self: @This(), key: Cell) u64 {
        _ = self;
        return cellHash(key);
    }
    pub fn eql(self: @This(), a: Cell, b: Cell) bool {
        _ = self;
        return cellEql(a, b);
    }
};

const CellMap = std.HashMapUnmanaged(Cell, usize, CellContext, std.hash_map.default_max_load_percentage);

fn parseInt(s: []const u8) i32 {
    return std.fmt.parseInt(i32, s, 10) catch 0;
}

fn parseBricks(allocator: Allocator, content: []const u8) ![]Brick {
    var bricks = std.ArrayListUnmanaged(Brick){};
    var lines = std.mem.splitScalar(u8, content, '\n');
    var idx: usize = 0;

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Find tilde separator
        var tilde_idx: usize = 0;
        for (line, 0..) |c, i| {
            if (c == '~') {
                tilde_idx = i;
                break;
            }
        }

        const left = line[0..tilde_idx];
        const right = line[tilde_idx + 1 ..];

        // Parse left: x1,y1,z1
        var left_parts = std.mem.splitScalar(u8, left, ',');
        const x1 = parseInt(left_parts.next().?);
        const y1 = parseInt(left_parts.next().?);
        const z1 = parseInt(left_parts.next().?);

        // Parse right: x2,y2,z2
        var right_parts = std.mem.splitScalar(u8, right, ',');
        const x2 = parseInt(right_parts.next().?);
        const y2 = parseInt(right_parts.next().?);
        const z2 = parseInt(right_parts.next().?);

        // Ensure z1 <= z2
        if (z1 > z2) {
            try bricks.append(allocator, .{
                .x1 = x2,
                .y1 = y2,
                .z1 = z2,
                .x2 = x1,
                .y2 = y1,
                .z2 = z1,
                .orig_idx = idx,
            });
        } else {
            try bricks.append(allocator, .{
                .x1 = x1,
                .y1 = y1,
                .z1 = z1,
                .x2 = x2,
                .y2 = y2,
                .z2 = z2,
                .orig_idx = idx,
            });
        }
        idx += 1;
    }

    return try bricks.toOwnedSlice(allocator);
}

const SupportGraph = struct {
    // supports[i] contains bricks that brick i supports (bricks above i)
    supports: []std.AutoHashMapUnmanaged(usize, void),
    // supporters[i] contains bricks that support brick i (bricks below i)
    supporters: []std.AutoHashMapUnmanaged(usize, void),
    allocator: Allocator,
    n: usize,

    fn init(allocator: Allocator, n: usize) !SupportGraph {
        const supports = try allocator.alloc(std.AutoHashMapUnmanaged(usize, void), n);
        const supporters = try allocator.alloc(std.AutoHashMapUnmanaged(usize, void), n);

        for (0..n) |i| {
            supports[i] = std.AutoHashMapUnmanaged(usize, void){};
            supporters[i] = std.AutoHashMapUnmanaged(usize, void){};
        }

        return .{
            .supports = supports,
            .supporters = supporters,
            .allocator = allocator,
            .n = n,
        };
    }

    fn deinit(self: *SupportGraph) void {
        for (0..self.n) |i| {
            self.supports[i].deinit(self.allocator);
            self.supporters[i].deinit(self.allocator);
        }
        self.allocator.free(self.supports);
        self.allocator.free(self.supporters);
    }

    fn addSupport(self: *SupportGraph, supporter: usize, supported: usize) !void {
        try self.supports[supporter].put(self.allocator, supported, {});
        try self.supporters[supported].put(self.allocator, supporter, {});
    }
};

fn compareBrickZ(context: void, a: Brick, b: Brick) bool {
    _ = context;
    return a.z1 < b.z1;
}

fn settleBricks(allocator: Allocator, bricks: []Brick) !SupportGraph {
    const n = bricks.len;

    // Sort by minimum z
    std.mem.sort(Brick, bricks, {}, compareBrickZ);

    // Occupied cells: (x,y,z) -> brick original index
    var occupied = CellMap{};
    defer occupied.deinit(allocator);

    var graph = try SupportGraph.init(allocator, n);

    for (bricks) |*brick| {
        const x1 = brick.x1;
        const y1 = brick.y1;
        const z1 = brick.z1;
        const x2 = brick.x2;
        const y2 = brick.y2;
        const z2 = brick.z2;

        const min_x = @min(x1, x2);
        const max_x = @max(x1, x2);
        const min_y = @min(y1, y2);
        const max_y = @max(y1, y2);

        // Find maximum drop
        var drop: i32 = z1 - 1; // drop to z=1 at most

        var x = min_x;
        while (x <= max_x) : (x += 1) {
            var y = min_y;
            while (y <= max_y) : (y += 1) {
                // Check cells below
                var z = z1 - 1;
                while (z > 0) : (z -= 1) {
                    if (occupied.get(.{ .x = x, .y = y, .z = z })) |_| {
                        drop = @min(drop, z1 - z - 1);
                        break;
                    }
                }
            }
        }

        // Apply drop
        const new_z1 = z1 - drop;
        const new_z2 = z2 - drop;
        brick.z1 = new_z1;
        brick.z2 = new_z2;

        // Mark cells as occupied and find supporters
        x = min_x;
        while (x <= max_x) : (x += 1) {
            var y = min_y;
            while (y <= max_y) : (y += 1) {
                // Check for supporter directly below
                if (occupied.get(.{ .x = x, .y = y, .z = new_z1 - 1 })) |supporter_idx| {
                    try graph.addSupport(supporter_idx, brick.orig_idx);
                }

                // Mark all cells of this brick
                var z = new_z1;
                while (z <= new_z2) : (z += 1) {
                    try occupied.put(allocator, .{ .x = x, .y = y, .z = z }, brick.orig_idx);
                }
            }
        }
    }

    return graph;
}

fn part1(graph: *SupportGraph) usize {
    var safe_count: usize = 0;

    for (0..graph.n) |i| {
        var can_remove = true;

        var it = graph.supports[i].keyIterator();
        while (it.next()) |supported_ptr| {
            const supported = supported_ptr.*;
            if (graph.supporters[supported].count() == 1) {
                can_remove = false;
                break;
            }
        }

        if (can_remove) {
            safe_count += 1;
        }
    }

    return safe_count;
}

fn part2(allocator: Allocator, graph: *SupportGraph) !usize {
    var total_falls: usize = 0;

    // For each brick, simulate removing it
    for (0..graph.n) |i| {
        // BFS to find all falling bricks
        var falling = std.AutoHashMapUnmanaged(usize, void){};
        defer falling.deinit(allocator);

        var queue = std.ArrayListUnmanaged(usize){};
        defer queue.deinit(allocator);

        try falling.put(allocator, i, {});
        try queue.append(allocator, i);

        var head: usize = 0;
        while (head < queue.items.len) {
            const brick = queue.items[head];
            head += 1;

            // Check all bricks that this brick supports
            var it = graph.supports[brick].keyIterator();
            while (it.next()) |supported_ptr| {
                const supported = supported_ptr.*;

                if (falling.contains(supported)) continue;

                // Check if all supporters have fallen
                var all_fallen = true;
                var sup_it = graph.supporters[supported].keyIterator();
                while (sup_it.next()) |sup_ptr| {
                    if (!falling.contains(sup_ptr.*)) {
                        all_fallen = false;
                        break;
                    }
                }

                if (all_fallen) {
                    try falling.put(allocator, supported, {});
                    try queue.append(allocator, supported);
                }
            }
        }

        // Count falls (excluding initial brick)
        total_falls += falling.count() - 1;
    }

    return total_falls;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const bricks = try parseBricks(allocator, std.mem.trim(u8, content, "\n\r "));
    defer allocator.free(bricks);

    var graph = try settleBricks(allocator, bricks);
    defer graph.deinit();

    const p1 = part1(&graph);
    const p2 = try part2(allocator, &graph);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
