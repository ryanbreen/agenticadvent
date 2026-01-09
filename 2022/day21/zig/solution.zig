const std = @import("std");

const MonkeyId = [4]u8;

const Op = enum { add, sub, mul, div };

const Job = union(enum) {
    number: i64,
    expr: struct {
        left: MonkeyId,
        op: Op,
        right: MonkeyId,
    },
};

fn parseMonkeyId(s: []const u8) MonkeyId {
    var id: MonkeyId = undefined;
    @memcpy(&id, s[0..4]);
    return id;
}

fn parseOp(c: u8) Op {
    return switch (c) {
        '+' => .add,
        '-' => .sub,
        '*' => .mul,
        '/' => .div,
        else => unreachable,
    };
}

fn parse(allocator: std.mem.Allocator, input: []const u8) !std.AutoHashMap(MonkeyId, Job) {
    var monkeys = std.AutoHashMap(MonkeyId, Job).init(allocator);

    var lines = std.mem.tokenizeScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len < 6) continue;

        const name = parseMonkeyId(line[0..4]);
        const job_str = line[6..];

        if (job_str.len <= 11) {
            // Try to parse as number
            const trimmed = std.mem.trim(u8, job_str, " \t\r");
            if (std.fmt.parseInt(i64, trimmed, 10)) |num| {
                try monkeys.put(name, .{ .number = num });
                continue;
            } else |_| {}
        }

        // Parse as expression: "aaaa + bbbb"
        if (job_str.len >= 11) {
            const left = parseMonkeyId(job_str[0..4]);
            const op = parseOp(job_str[5]);
            const right = parseMonkeyId(job_str[7..11]);
            try monkeys.put(name, .{ .expr = .{ .left = left, .op = op, .right = right } });
        }
    }

    return monkeys;
}

fn evaluate(monkeys: *const std.AutoHashMap(MonkeyId, Job), name: MonkeyId, memo: *std.AutoHashMap(MonkeyId, i64)) i64 {
    if (memo.get(name)) |val| {
        return val;
    }

    const job = monkeys.get(name) orelse unreachable;
    const result: i64 = switch (job) {
        .number => |n| n,
        .expr => |e| blk: {
            const left_val = evaluate(monkeys, e.left, memo);
            const right_val = evaluate(monkeys, e.right, memo);
            break :blk switch (e.op) {
                .add => left_val + right_val,
                .sub => left_val - right_val,
                .mul => left_val * right_val,
                .div => @divTrunc(left_val, right_val),
            };
        },
    };

    memo.put(name, result) catch unreachable;
    return result;
}

fn containsHumn(monkeys: *const std.AutoHashMap(MonkeyId, Job), name: MonkeyId, memo: *std.AutoHashMap(MonkeyId, bool)) bool {
    const humn_id = parseMonkeyId("humn");
    if (std.mem.eql(u8, &name, &humn_id)) {
        return true;
    }

    if (memo.get(name)) |val| {
        return val;
    }

    const job = monkeys.get(name) orelse unreachable;
    const result: bool = switch (job) {
        .number => false,
        .expr => |e| containsHumn(monkeys, e.left, memo) or containsHumn(monkeys, e.right, memo),
    };

    memo.put(name, result) catch unreachable;
    return result;
}

fn solveForHumn(monkeys: *const std.AutoHashMap(MonkeyId, Job), name: MonkeyId, target: i64, humn_memo: *std.AutoHashMap(MonkeyId, bool), eval_memo: *std.AutoHashMap(MonkeyId, i64)) i64 {
    const humn_id = parseMonkeyId("humn");
    if (std.mem.eql(u8, &name, &humn_id)) {
        return target;
    }

    const job = monkeys.get(name) orelse unreachable;
    const expr = switch (job) {
        .number => unreachable,
        .expr => |e| e,
    };

    const left_has_humn = containsHumn(monkeys, expr.left, humn_memo);

    if (left_has_humn) {
        const right_val = evaluate(monkeys, expr.right, eval_memo);
        const new_target: i64 = switch (expr.op) {
            .add => target - right_val,
            .sub => target + right_val,
            .mul => @divTrunc(target, right_val),
            .div => target * right_val,
        };
        return solveForHumn(monkeys, expr.left, new_target, humn_memo, eval_memo);
    } else {
        const left_val = evaluate(monkeys, expr.left, eval_memo);
        const new_target: i64 = switch (expr.op) {
            .add => target - left_val,
            .sub => left_val - target,
            .mul => @divTrunc(target, left_val),
            .div => @divTrunc(left_val, target),
        };
        return solveForHumn(monkeys, expr.right, new_target, humn_memo, eval_memo);
    }
}

fn part1(monkeys: *const std.AutoHashMap(MonkeyId, Job), allocator: std.mem.Allocator) !i64 {
    var memo = std.AutoHashMap(MonkeyId, i64).init(allocator);
    defer memo.deinit();

    const root_id = parseMonkeyId("root");
    return evaluate(monkeys, root_id, &memo);
}

fn part2(monkeys: *const std.AutoHashMap(MonkeyId, Job), allocator: std.mem.Allocator) !i64 {
    var humn_memo = std.AutoHashMap(MonkeyId, bool).init(allocator);
    defer humn_memo.deinit();

    var eval_memo = std.AutoHashMap(MonkeyId, i64).init(allocator);
    defer eval_memo.deinit();

    const root_id = parseMonkeyId("root");
    const root_job = monkeys.get(root_id) orelse unreachable;
    const root_expr = switch (root_job) {
        .number => unreachable,
        .expr => |e| e,
    };

    const left_has_humn = containsHumn(monkeys, root_expr.left, &humn_memo);

    if (left_has_humn) {
        const target = evaluate(monkeys, root_expr.right, &eval_memo);
        return solveForHumn(monkeys, root_expr.left, target, &humn_memo, &eval_memo);
    } else {
        const target = evaluate(monkeys, root_expr.left, &eval_memo);
        return solveForHumn(monkeys, root_expr.right, target, &humn_memo, &eval_memo);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = std.fs.cwd().openFile("../input.txt", .{}) catch |err| {
        std.debug.print("Failed to open ../input.txt: {}\n", .{err});
        return err;
    };
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    var monkeys = try parse(allocator, input);
    defer monkeys.deinit();

    const p1 = try part1(&monkeys, allocator);
    const p2 = try part2(&monkeys, allocator);

    // Use posix write to stdout
    const stdout_fd = std.posix.STDOUT_FILENO;
    var buf1: [64]u8 = undefined;
    const msg1 = std.fmt.bufPrint(&buf1, "Part 1: {d}\n", .{p1}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg1) catch {};

    var buf2: [64]u8 = undefined;
    const msg2 = std.fmt.bufPrint(&buf2, "Part 2: {d}\n", .{p2}) catch unreachable;
    _ = std.posix.write(stdout_fd, msg2) catch {};
}
