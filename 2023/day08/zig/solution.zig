const std = @import("std");

const Node = struct {
    left: [3]u8,
    right: [3]u8,
};

fn parseNodeName(slice: []const u8) [3]u8 {
    return .{ slice[0], slice[1], slice[2] };
}

fn lcm(a: u64, b: u64) u64 {
    return a / std.math.gcd(a, b) * b;
}

/// Navigate from a starting node until the predicate returns true.
/// Returns the number of steps taken.
fn navigate(
    network: std.AutoHashMap([3]u8, Node),
    instructions: []const u8,
    start: [3]u8,
    comptime isEnd: fn ([3]u8) bool,
) !u64 {
    var current = start;
    var steps: u64 = 0;

    while (!isEnd(current)) {
        const instruction = instructions[steps % instructions.len];
        const node = network.get(current) orelse return error.NodeNotFound;
        current = if (instruction == 'L') node.left else node.right;
        steps += 1;
    }

    return steps;
}

fn isZZZ(node: [3]u8) bool {
    return std.mem.eql(u8, &node, &[3]u8{ 'Z', 'Z', 'Z' });
}

fn endsWithZ(node: [3]u8) bool {
    return node[2] == 'Z';
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
        // Minimum valid line: "AAA = (BBB, CCC)" is 16 characters
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
    const part1 = try navigate(network, instructions, .{ 'A', 'A', 'A' }, isZZZ);
    std.debug.print("Part 1: {}\n", .{part1});

    // Part 2: Find LCM of all cycle lengths
    var result: u64 = 1;
    for (starting_nodes.items) |start_node| {
        const cycle_steps = try navigate(network, instructions, start_node, endsWithZ);
        result = lcm(result, cycle_steps);
    }
    std.debug.print("Part 2: {}\n", .{result});
}
