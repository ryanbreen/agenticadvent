const std = @import("std");

const Machine = struct {
    ax: i64,
    ay: i64,
    bx: i64,
    by: i64,
    px: i64,
    py: i64,
};

fn parseMachines(allocator: std.mem.Allocator, input: []const u8) !std.ArrayList(Machine) {
    var machines = std.ArrayList(Machine){};
    var blocks = std.mem.splitSequence(u8, input, "\n\n");

    while (blocks.next()) |block| {
        if (block.len == 0) continue;

        var lines = std.mem.splitScalar(u8, block, '\n');
        const line1 = lines.next() orelse continue;
        const line2 = lines.next() orelse continue;
        const line3 = lines.next() orelse continue;

        // Parse Button A: X+94, Y+34
        var ax: i64 = 0;
        var ay: i64 = 0;
        if (std.mem.indexOf(u8, line1, "X+")) |x_pos| {
            const x_start = x_pos + 2;
            if (std.mem.indexOf(u8, line1[x_start..], ",")) |comma_offset| {
                const x_end = x_start + comma_offset;
                ax = try std.fmt.parseInt(i64, line1[x_start..x_end], 10);

                if (std.mem.indexOf(u8, line1[x_end..], "Y+")) |y_offset| {
                    const y_start = x_end + y_offset + 2;
                    ay = try std.fmt.parseInt(i64, line1[y_start..], 10);
                }
            }
        }

        // Parse Button B: X+22, Y+67
        var bx: i64 = 0;
        var by: i64 = 0;
        if (std.mem.indexOf(u8, line2, "X+")) |x_pos| {
            const x_start = x_pos + 2;
            if (std.mem.indexOf(u8, line2[x_start..], ",")) |comma_offset| {
                const x_end = x_start + comma_offset;
                bx = try std.fmt.parseInt(i64, line2[x_start..x_end], 10);

                if (std.mem.indexOf(u8, line2[x_end..], "Y+")) |y_offset| {
                    const y_start = x_end + y_offset + 2;
                    by = try std.fmt.parseInt(i64, line2[y_start..], 10);
                }
            }
        }

        // Parse Prize: X=8400, Y=5400
        var px: i64 = 0;
        var py: i64 = 0;
        if (std.mem.indexOf(u8, line3, "X=")) |x_pos| {
            const x_start = x_pos + 2;
            if (std.mem.indexOf(u8, line3[x_start..], ",")) |comma_offset| {
                const x_end = x_start + comma_offset;
                px = try std.fmt.parseInt(i64, line3[x_start..x_end], 10);

                if (std.mem.indexOf(u8, line3[x_end..], "Y=")) |y_offset| {
                    const y_start = x_end + y_offset + 2;
                    py = try std.fmt.parseInt(i64, line3[y_start..], 10);
                }
            }
        }

        try machines.append(allocator, Machine{
            .ax = ax,
            .ay = ay,
            .bx = bx,
            .by = by,
            .px = px,
            .py = py,
        });
    }

    return machines;
}

fn solveMachine(machine: Machine, max_presses: ?i64) ?i64 {
    // Cramer's rule for solving system:
    // a*ax + b*bx = px
    // a*ay + b*by = py
    //
    // det = ax*by - ay*bx
    // a = (px*by - py*bx) / det
    // b = (ax*py - ay*px) / det

    const det = machine.ax * machine.by - machine.ay * machine.bx;

    if (det == 0) {
        return null; // No unique solution
    }

    const a_num = machine.px * machine.by - machine.py * machine.bx;
    const b_num = machine.ax * machine.py - machine.ay * machine.px;

    // Check if solutions are integers
    if (@mod(a_num, det) != 0 or @mod(b_num, det) != 0) {
        return null;
    }

    const a = @divTrunc(a_num, det);
    const b = @divTrunc(b_num, det);

    // Check non-negative
    if (a < 0 or b < 0) {
        return null;
    }

    // Check max presses constraint (Part 1)
    if (max_presses) |max| {
        if (a > max or b > max) {
            return null;
        }
    }

    return 3 * a + b;
}

fn part1(machines: []const Machine) i64 {
    var total: i64 = 0;

    for (machines) |machine| {
        if (solveMachine(machine, 100)) |cost| {
            total += cost;
        }
    }

    return total;
}

fn part2(machines: []const Machine) i64 {
    const offset: i64 = 10_000_000_000_000;
    var total: i64 = 0;

    for (machines) |machine| {
        var shifted = machine;
        shifted.px += offset;
        shifted.py += offset;

        if (solveMachine(shifted, null)) |cost| {
            total += cost;
        }
    }

    return total;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    // Trim whitespace
    const trimmed = std.mem.trim(u8, input, " \n\r\t");

    // Parse machines
    var machines = try parseMachines(allocator, trimmed);
    defer machines.deinit(allocator);

    // Solve
    const p1 = part1(machines.items);
    const p2 = part2(machines.items);

    std.debug.print("Part 1: {d}\n", .{p1});
    std.debug.print("Part 2: {d}\n", .{p2});
}
