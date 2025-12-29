const std = @import("std");

const Node = struct {
    left: [3]u8,
    right: [3]u8,
};

fn parseNodeName(slice: []const u8) [3]u8 {
    return .{ slice[0], slice[1], slice[2] };
}

fn gcd(a: u64, b: u64) u64 {
    var x = a;
    var y = b;
    while (y != 0) {
        const temp = y;
        y = x % y;
        x = temp;
    }
    return x;
}

fn lcm(a: u64, b: u64) u64 {
    return a / gcd(a, b) * b;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    // Parse input
    var lines = std.mem.splitScalar(u8, input, '\n');
    const instructions = lines.next() orelse return error.InvalidInput;

    // Skip empty line
    _ = lines.next();

    // Parse network
    var network = std.AutoHashMap([3]u8, Node).init(allocator);
    defer network.deinit();

    var starting_nodes: std.ArrayListUnmanaged([3]u8) = .empty;
    defer starting_nodes.deinit(allocator);

    while (lines.next()) |line| {
        if (line.len < 16) continue;

        // Parse: AAA = (BBB, CCC)
        const node_name = parseNodeName(line[0..3]);
        const left = parseNodeName(line[7..10]);
        const right = parseNodeName(line[12..15]);

        try network.put(node_name, .{ .left = left, .right = right });

        // Check if it's a starting node (ends with 'A')
        if (node_name[2] == 'A') {
            try starting_nodes.append(allocator, node_name);
        }
    }

    // Part 1: Navigate from AAA to ZZZ
    var current: [3]u8 = .{ 'A', 'A', 'A' };
    const target: [3]u8 = .{ 'Z', 'Z', 'Z' };
    var steps: u64 = 0;

    while (!std.mem.eql(u8, &current, &target)) {
        const instruction = instructions[steps % instructions.len];
        const node = network.get(current) orelse return error.NodeNotFound;
        current = if (instruction == 'L') node.left else node.right;
        steps += 1;
    }

    std.debug.print("Part 1: {}\n", .{steps});

    // Part 2: Find LCM of all cycle lengths
    var result: u64 = 1;

    for (starting_nodes.items) |start_node| {
        var node = start_node;
        var cycle_steps: u64 = 0;

        while (node[2] != 'Z') {
            const instruction = instructions[cycle_steps % instructions.len];
            const network_node = network.get(node) orelse return error.NodeNotFound;
            node = if (instruction == 'L') network_node.left else network_node.right;
            cycle_steps += 1;
        }

        result = lcm(result, cycle_steps);
    }

    std.debug.print("Part 2: {}\n", .{result});
}
