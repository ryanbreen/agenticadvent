const std = @import("std");

const Equation = struct {
    target: i64,
    nums: []i64,
};

const Operator = enum {
    add,
    mul,
    concat,
};

fn countDigits(n: i64) i64 {
    if (n == 0) return 1;
    var count: i64 = 0;
    var num = if (n < 0) -n else n;
    while (num > 0) {
        count += 1;
        num = @divFloor(num, 10);
    }
    return count;
}

fn concat(a: i64, b: i64) i64 {
    const digits = countDigits(b);
    var multiplier: i64 = 1;
    var i: i64 = 0;
    while (i < digits) : (i += 1) {
        multiplier *= 10;
    }
    return a * multiplier + b;
}

fn evaluate(nums: []i64, ops: []Operator) i64 {
    var result = nums[0];
    for (ops, 0..) |op, i| {
        switch (op) {
            .add => result += nums[i + 1],
            .mul => result *= nums[i + 1],
            .concat => result = concat(result, nums[i + 1]),
        }
    }
    return result;
}

fn canMakeTarget(target: i64, nums: []i64, operators: []const Operator) bool {
    const n_ops = nums.len - 1;
    if (n_ops == 0) return nums[0] == target;

    // Use stack allocation for operators array (max 20 operators should be fine)
    var ops: [20]Operator = undefined;
    const ops_slice = ops[0..n_ops];

    // Try all combinations
    var total_combinations: usize = 1;
    for (0..n_ops) |_| {
        total_combinations *= operators.len;
    }

    for (0..total_combinations) |combo_idx| {
        // Convert combo_idx to operator combination
        var idx = combo_idx;
        for (0..n_ops) |i| {
            ops_slice[i] = operators[idx % operators.len];
            idx /= operators.len;
        }

        const result = evaluate(nums, ops_slice);
        if (result == target) {
            return true;
        }
    }

    return false;
}

fn part1(equations: []Equation) i64 {
    const operators = [_]Operator{ .add, .mul };
    var total: i64 = 0;

    for (equations) |eq| {
        if (canMakeTarget(eq.target, eq.nums, &operators)) {
            total += eq.target;
        }
    }

    return total;
}

fn part2(equations: []Equation) i64 {
    const operators = [_]Operator{ .add, .mul, .concat };
    var total: i64 = 0;

    for (equations) |eq| {
        if (canMakeTarget(eq.target, eq.nums, &operators)) {
            total += eq.target;
        }
    }

    return total;
}

fn parseInput(text: []const u8, allocator: std.mem.Allocator) ![]Equation {
    var equations: std.ArrayList(Equation) = .{};
    defer equations.deinit(allocator);

    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.splitSequence(u8, line, ": ");
        const target_str = parts.next() orelse continue;
        const nums_str = parts.next() orelse continue;

        const target = try std.fmt.parseInt(i64, target_str, 10);

        var nums: std.ArrayList(i64) = .{};
        defer nums.deinit(allocator);

        var num_tokens = std.mem.splitScalar(u8, nums_str, ' ');
        while (num_tokens.next()) |num_str| {
            if (num_str.len == 0) continue;
            const num = try std.fmt.parseInt(i64, num_str, 10);
            try nums.append(allocator, num);
        }

        try equations.append(allocator, .{
            .target = target,
            .nums = try nums.toOwnedSlice(allocator),
        });
    }

    return try equations.toOwnedSlice(allocator);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const text = try allocator.alloc(u8, file_size);
    defer allocator.free(text);

    _ = try file.readAll(text);

    const equations = try parseInput(text, allocator);
    defer {
        for (equations) |eq| {
            allocator.free(eq.nums);
        }
        allocator.free(equations);
    }

    const p1 = part1(equations);
    const p2 = part2(equations);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
