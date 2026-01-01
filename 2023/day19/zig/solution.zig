const std = @import("std");

const Attr = enum(u2) { x = 0, m = 1, a = 2, s = 3 };
const Op = enum(u2) { lt = 0, gt = 1, none = 2 };

const Rule = struct {
    attr: Attr,
    op: Op,
    value: i32,
    dest_idx: i32, // Index into workflow names array, or -1 for A, -2 for R
};

const Workflow = struct {
    rules: [8]Rule,
    rule_count: u8,
};

const Part = struct {
    values: [4]i32,

    fn rating(self: Part) i64 {
        var sum: i64 = 0;
        for (self.values) |v| {
            sum += v;
        }
        return sum;
    }
};

const Range = struct {
    lo: i32,
    hi: i32,

    fn count(self: Range) i64 {
        if (self.hi < self.lo) return 0;
        return @as(i64, self.hi - self.lo + 1);
    }
};

const Ranges = struct {
    r: [4]Range,

    fn init() Ranges {
        return Ranges{
            .r = [4]Range{
                .{ .lo = 1, .hi = 4000 },
                .{ .lo = 1, .hi = 4000 },
                .{ .lo = 1, .hi = 4000 },
                .{ .lo = 1, .hi = 4000 },
            },
        };
    }

    fn combinations(self: Ranges) i64 {
        var result: i64 = 1;
        for (self.r) |range| {
            result *= range.count();
        }
        return result;
    }
};

const MAX_WORKFLOWS = 600;
const MAX_PARTS = 300;

const ParsedData = struct {
    workflows: [MAX_WORKFLOWS]Workflow,
    workflow_names: [MAX_WORKFLOWS][4]u8, // Max 3 chars + padding
    workflow_count: usize,
    in_idx: i32,
    parts: [MAX_PARTS]Part,
    part_count: usize,

    fn findWorkflow(self: *const ParsedData, name: []const u8) i32 {
        if (std.mem.eql(u8, name, "A")) return -1;
        if (std.mem.eql(u8, name, "R")) return -2;

        for (0..self.workflow_count) |i| {
            const wf_name = self.workflow_names[i];
            const wf_len = for (0..4) |j| {
                if (wf_name[j] == 0) break j;
            } else 4;
            if (std.mem.eql(u8, wf_name[0..wf_len], name)) {
                return @intCast(i);
            }
        }
        return -3; // Not found
    }
};

fn parseAttr(c: u8) Attr {
    return switch (c) {
        'x' => .x,
        'm' => .m,
        'a' => .a,
        's' => .s,
        else => unreachable,
    };
}

fn parseInput(content: []const u8) ParsedData {
    var data = ParsedData{
        .workflows = undefined,
        .workflow_names = undefined,
        .workflow_count = 0,
        .in_idx = 0,
        .parts = undefined,
        .part_count = 0,
    };

    // First pass: collect all workflow names
    var sections = std.mem.splitSequence(u8, content, "\n\n");
    const workflow_section = sections.next() orelse return data;
    const parts_section = sections.next() orelse return data;

    var lines = std.mem.splitScalar(u8, workflow_section, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const brace_pos = std.mem.indexOf(u8, line, "{") orelse continue;
        const name = line[0..brace_pos];

        var stored_name: [4]u8 = [4]u8{ 0, 0, 0, 0 };
        const copy_len = @min(name.len, 4);
        @memcpy(stored_name[0..copy_len], name[0..copy_len]);
        data.workflow_names[data.workflow_count] = stored_name;

        if (std.mem.eql(u8, name, "in")) {
            data.in_idx = @intCast(data.workflow_count);
        }

        data.workflow_count += 1;
    }

    // Second pass: parse rules with resolved destinations
    lines = std.mem.splitScalar(u8, workflow_section, '\n');
    var wf_idx: usize = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const brace_pos = std.mem.indexOf(u8, line, "{") orelse continue;

        const rules_str = line[brace_pos + 1 .. line.len - 1];
        var workflow = Workflow{ .rules = undefined, .rule_count = 0 };

        var rule_iter = std.mem.splitScalar(u8, rules_str, ',');
        while (rule_iter.next()) |rule_str| {
            if (std.mem.indexOf(u8, rule_str, ":")) |colon_pos| {
                const condition = rule_str[0..colon_pos];
                const destination = rule_str[colon_pos + 1 ..];

                workflow.rules[workflow.rule_count] = Rule{
                    .attr = parseAttr(condition[0]),
                    .op = if (condition[1] == '<') .lt else .gt,
                    .value = std.fmt.parseInt(i32, condition[2..], 10) catch 0,
                    .dest_idx = data.findWorkflow(destination),
                };
            } else {
                workflow.rules[workflow.rule_count] = Rule{
                    .attr = .x,
                    .op = .none,
                    .value = 0,
                    .dest_idx = data.findWorkflow(rule_str),
                };
            }
            workflow.rule_count += 1;
        }

        data.workflows[wf_idx] = workflow;
        wf_idx += 1;
    }

    // Parse parts
    var part_lines = std.mem.splitScalar(u8, parts_section, '\n');
    while (part_lines.next()) |line| {
        if (line.len == 0 or line[0] != '{') continue;

        var part = Part{ .values = [4]i32{ 0, 0, 0, 0 } };
        const inner = line[1 .. line.len - 1];

        var attr_iter = std.mem.splitScalar(u8, inner, ',');
        while (attr_iter.next()) |attr_str| {
            const attr = parseAttr(attr_str[0]);
            const value = std.fmt.parseInt(i32, attr_str[2..], 10) catch 0;
            part.values[@intFromEnum(attr)] = value;
        }

        data.parts[data.part_count] = part;
        data.part_count += 1;
    }

    return data;
}

fn processPart(data: *const ParsedData, part: Part) bool {
    var current: i32 = data.in_idx;

    while (current >= 0) {
        const workflow = data.workflows[@intCast(current)];

        for (workflow.rules[0..workflow.rule_count]) |rule| {
            if (rule.op == .none) {
                current = rule.dest_idx;
                break;
            } else {
                const part_val = part.values[@intFromEnum(rule.attr)];
                const matches = switch (rule.op) {
                    .lt => part_val < rule.value,
                    .gt => part_val > rule.value,
                    .none => unreachable,
                };
                if (matches) {
                    current = rule.dest_idx;
                    break;
                }
            }
        }
    }

    return current == -1; // -1 = A (accepted)
}

fn part1(data: *const ParsedData) i64 {
    var total: i64 = 0;
    for (data.parts[0..data.part_count]) |part| {
        if (processPart(data, part)) {
            total += part.rating();
        }
    }
    return total;
}

fn countAccepted(data: *const ParsedData, workflow_idx: i32, ranges: Ranges) i64 {
    if (workflow_idx == -2) return 0; // R (rejected)
    if (workflow_idx == -1) return ranges.combinations(); // A (accepted)

    const workflow = data.workflows[@intCast(workflow_idx)];

    var total: i64 = 0;
    var current_ranges = ranges;

    for (workflow.rules[0..workflow.rule_count]) |rule| {
        if (rule.op == .none) {
            total += countAccepted(data, rule.dest_idx, current_ranges);
        } else {
            const attr_idx = @intFromEnum(rule.attr);
            const lo = current_ranges.r[attr_idx].lo;
            const hi = current_ranges.r[attr_idx].hi;

            if (rule.op == .lt) {
                if (lo < rule.value) {
                    var new_ranges = current_ranges;
                    new_ranges.r[attr_idx] = .{ .lo = lo, .hi = @min(hi, rule.value - 1) };
                    total += countAccepted(data, rule.dest_idx, new_ranges);
                }
                if (hi >= rule.value) {
                    current_ranges.r[attr_idx] = .{ .lo = @max(lo, rule.value), .hi = hi };
                } else {
                    break;
                }
            } else {
                if (hi > rule.value) {
                    var new_ranges = current_ranges;
                    new_ranges.r[attr_idx] = .{ .lo = @max(lo, rule.value + 1), .hi = hi };
                    total += countAccepted(data, rule.dest_idx, new_ranges);
                }
                if (lo <= rule.value) {
                    current_ranges.r[attr_idx] = .{ .lo = lo, .hi = @min(hi, rule.value) };
                } else {
                    break;
                }
            }
        }
    }

    return total;
}

fn part2(data: *const ParsedData) i64 {
    return countAccepted(data, data.in_idx, Ranges.init());
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const data = parseInput(content);

    std.debug.print("Part 1: {d}\n", .{part1(&data)});
    std.debug.print("Part 2: {d}\n", .{part2(&data)});
}
