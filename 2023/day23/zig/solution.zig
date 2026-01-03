const std = @import("std");

fn coordToIndex(r: i32, c: i32, cols: usize) u64 {
    return @as(u64, @intCast(r)) * cols + @as(u64, @intCast(c));
}

const Edge = struct {
    target_id: usize,
    weight: i32,
};

const Graph = struct {
    edges: std.ArrayListUnmanaged(std.ArrayListUnmanaged(Edge)),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) Graph {
        return Graph{
            .edges = std.ArrayListUnmanaged(std.ArrayListUnmanaged(Edge)){},
            .allocator = allocator,
        };
    }

    fn deinit(self: *Graph) void {
        for (self.edges.items) |*list| {
            list.deinit(self.allocator);
        }
        self.edges.deinit(self.allocator);
    }

    fn ensureNode(self: *Graph, node: usize) !void {
        while (self.edges.items.len <= node) {
            try self.edges.append(self.allocator, std.ArrayListUnmanaged(Edge){});
        }
    }

    fn addEdge(self: *Graph, from: usize, to_id: usize, weight: i32) !void {
        try self.ensureNode(from);
        try self.edges.items[from].append(self.allocator, Edge{ .target_id = to_id, .weight = weight });
    }

    fn getEdges(self: *Graph, node: usize) []Edge {
        if (node >= self.edges.items.len) return &[_]Edge{};
        return self.edges.items[node].items;
    }
};

fn parseInput(allocator: std.mem.Allocator) !struct { grid: [][]const u8, rows: usize, cols: usize } {
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.ArrayListUnmanaged([]const u8){};

    var it = std.mem.splitScalar(u8, content, '\n');
    while (it.next()) |line| {
        if (line.len > 0) {
            const line_copy = try allocator.dupe(u8, line);
            try lines.append(allocator, line_copy);
        }
    }

    const grid = try lines.toOwnedSlice(allocator);
    const rows = grid.len;
    const cols = if (rows > 0) grid[0].len else 0;

    return .{ .grid = grid, .rows = rows, .cols = cols };
}

fn findJunctions(allocator: std.mem.Allocator, grid: [][]const u8, rows: usize, cols: usize) !std.AutoHashMapUnmanaged(u64, void) {
    var junctions = std.AutoHashMapUnmanaged(u64, void){};

    // Find start
    for (grid[0], 0..) |cell, c| {
        if (cell == '.') {
            try junctions.put(allocator, coordToIndex(0, @intCast(c), cols), {});
            break;
        }
    }

    // Find end
    for (grid[rows - 1], 0..) |cell, c| {
        if (cell == '.') {
            try junctions.put(allocator, coordToIndex(@intCast(rows - 1), @intCast(c), cols), {});
            break;
        }
    }

    // Find intersections (3+ walkable neighbors)
    const dirs = [_][2]i32{ .{ -1, 0 }, .{ 1, 0 }, .{ 0, -1 }, .{ 0, 1 } };

    for (0..rows) |r| {
        for (0..cols) |c| {
            if (grid[r][c] == '#') continue;

            var neighbors: u32 = 0;
            for (dirs) |dir| {
                const nr = @as(i32, @intCast(r)) + dir[0];
                const nc = @as(i32, @intCast(c)) + dir[1];
                if (nr >= 0 and nr < rows and nc >= 0 and nc < cols) {
                    if (grid[@intCast(nr)][@intCast(nc)] != '#') {
                        neighbors += 1;
                    }
                }
            }
            if (neighbors >= 3) {
                try junctions.put(allocator, coordToIndex(@intCast(r), @intCast(c), cols), {});
            }
        }
    }

    return junctions;
}

fn buildGraph(
    allocator: std.mem.Allocator,
    grid: [][]const u8,
    rows: usize,
    cols: usize,
    junctions: *std.AutoHashMapUnmanaged(u64, void),
    junction_ids: *std.AutoHashMapUnmanaged(u64, usize),
    respect_slopes: bool,
) !Graph {
    var graph = Graph.init(allocator);

    const dirs = [_][2]i32{ .{ -1, 0 }, .{ 1, 0 }, .{ 0, -1 }, .{ 0, 1 } };

    // Map junctions to compact IDs
    var id: usize = 0;
    var junction_iter = junctions.keyIterator();
    while (junction_iter.next()) |key| {
        try junction_ids.put(allocator, key.*, id);
        id += 1;
    }

    // Ensure graph has enough nodes
    try graph.ensureNode(id);

    // BFS from each junction
    var visited = std.AutoHashMapUnmanaged(u64, void){};
    defer visited.deinit(allocator);

    var stack = std.ArrayListUnmanaged(struct { r: i32, c: i32, dist: i32 }){};
    defer stack.deinit(allocator);

    var junction_iter2 = junctions.keyIterator();
    while (junction_iter2.next()) |start_key| {
        const start_junction = start_key.*;
        const start_r: i32 = @intCast(start_junction / cols);
        const start_c: i32 = @intCast(start_junction % cols);

        visited.clearRetainingCapacity();
        stack.clearRetainingCapacity();

        try stack.append(allocator, .{ .r = start_r, .c = start_c, .dist = 0 });
        try visited.put(allocator, start_junction, {});

        while (stack.items.len > 0) {
            const item = stack.items[stack.items.len - 1];
            stack.items.len -= 1;
            const r = item.r;
            const c = item.c;
            const dist = item.dist;
            const current_coord = coordToIndex(r, c, cols);

            if (dist > 0 and junctions.contains(current_coord)) {
                // Found another junction
                const from_id = junction_ids.get(start_junction).?;
                const to_id = junction_ids.get(current_coord).?;
                try graph.addEdge(from_id, to_id, dist);
                continue;
            }

            // Explore neighbors
            for (dirs) |dir| {
                const dr = dir[0];
                const dc = dir[1];
                const nr = r + dr;
                const nc = c + dc;

                if (nr < 0 or nr >= rows or nc < 0 or nc >= cols) continue;

                const cell = grid[@intCast(nr)][@intCast(nc)];
                if (cell == '#') continue;

                const next_coord = coordToIndex(nr, nc, cols);
                if (visited.contains(next_coord)) continue;

                // Check slope constraints for Part 1
                if (respect_slopes) {
                    const cur_cell = grid[@intCast(r)][@intCast(c)];
                    if (cur_cell == '^' and !(dr == -1 and dc == 0)) continue;
                    if (cur_cell == 'v' and !(dr == 1 and dc == 0)) continue;
                    if (cur_cell == '<' and !(dr == 0 and dc == -1)) continue;
                    if (cur_cell == '>' and !(dr == 0 and dc == 1)) continue;
                }

                try visited.put(allocator, next_coord, {});
                try stack.append(allocator, .{ .r = nr, .c = nc, .dist = dist + 1 });
            }
        }
    }

    return graph;
}

fn dfs(graph: *Graph, node: usize, end_id: usize, visited: *u64) i32 {
    if (node == end_id) return 0;

    const mask = @as(u64, 1) << @intCast(node);
    visited.* |= mask;

    var max_dist: i32 = -1000000;

    for (graph.getEdges(node)) |edge| {
        const target_mask = @as(u64, 1) << @intCast(edge.target_id);

        if ((visited.* & target_mask) == 0) {
            const result = dfs(graph, edge.target_id, end_id, visited);
            if (result != -1000000) {
                max_dist = @max(max_dist, edge.weight + result);
            }
        }
    }

    visited.* &= ~mask;
    return max_dist;
}

fn solve(
    allocator: std.mem.Allocator,
    grid: [][]const u8,
    rows: usize,
    cols: usize,
    respect_slopes: bool,
) !i32 {
    var junctions = try findJunctions(allocator, grid, rows, cols);
    defer junctions.deinit(allocator);

    var junction_ids = std.AutoHashMapUnmanaged(u64, usize){};
    defer junction_ids.deinit(allocator);

    var graph = try buildGraph(allocator, grid, rows, cols, &junctions, &junction_ids, respect_slopes);
    defer graph.deinit();

    // Find start and end
    var start: u64 = 0;
    var end: u64 = 0;

    for (grid[0], 0..) |cell, c| {
        if (cell == '.') {
            start = coordToIndex(0, @intCast(c), cols);
            break;
        }
    }

    for (grid[rows - 1], 0..) |cell, c| {
        if (cell == '.') {
            end = coordToIndex(@intCast(rows - 1), @intCast(c), cols);
            break;
        }
    }

    const start_id = junction_ids.get(start).?;
    const end_id = junction_ids.get(end).?;

    var visited: u64 = 0;
    return dfs(&graph, start_id, end_id, &visited);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const parsed = try parseInput(allocator);
    defer {
        for (parsed.grid) |line| {
            allocator.free(line);
        }
        allocator.free(parsed.grid);
    }

    const part1 = try solve(allocator, parsed.grid, parsed.rows, parsed.cols, true);
    const part2 = try solve(allocator, parsed.grid, parsed.rows, parsed.cols, false);

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}
