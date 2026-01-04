const std = @import("std");

const Hailstone = struct {
    px: i128,
    py: i128,
    pz: i128,
    vx: i128,
    vy: i128,
    vz: i128,
};

fn parseInput(allocator: std.mem.Allocator, content: []const u8) ![]Hailstone {
    var hailstones: std.ArrayListUnmanaged(Hailstone) = .{};

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Find '@' separator
        var parts = std.mem.splitSequence(u8, line, " @ ");
        const pos_str = parts.next() orelse continue;
        const vel_str = parts.next() orelse continue;

        // Parse position
        var pos_parts = std.mem.splitSequence(u8, pos_str, ", ");
        const px = try std.fmt.parseInt(i128, std.mem.trim(u8, pos_parts.next() orelse continue, " "), 10);
        const py = try std.fmt.parseInt(i128, std.mem.trim(u8, pos_parts.next() orelse continue, " "), 10);
        const pz = try std.fmt.parseInt(i128, std.mem.trim(u8, pos_parts.next() orelse continue, " "), 10);

        // Parse velocity
        var vel_parts = std.mem.splitSequence(u8, vel_str, ", ");
        const vx = try std.fmt.parseInt(i128, std.mem.trim(u8, vel_parts.next() orelse continue, " "), 10);
        const vy = try std.fmt.parseInt(i128, std.mem.trim(u8, vel_parts.next() orelse continue, " "), 10);
        const vz = try std.fmt.parseInt(i128, std.mem.trim(u8, vel_parts.next() orelse continue, " "), 10);

        try hailstones.append(allocator, Hailstone{
            .px = px,
            .py = py,
            .pz = pz,
            .vx = vx,
            .vy = vy,
            .vz = vz,
        });
    }

    return hailstones.toOwnedSlice(allocator);
}

fn findIntersection2D(h1: Hailstone, h2: Hailstone) ?struct { x: f64, y: f64, t1: f64, t2: f64 } {
    // Using Cramer's rule to solve:
    // vx1*t1 - vx2*t2 = px2 - px1
    // vy1*t1 - vy2*t2 = py2 - py1
    const vx1: f64 = @floatFromInt(h1.vx);
    const vy1: f64 = @floatFromInt(h1.vy);
    const vx2: f64 = @floatFromInt(h2.vx);
    const vy2: f64 = @floatFromInt(h2.vy);
    const px1: f64 = @floatFromInt(h1.px);
    const py1: f64 = @floatFromInt(h1.py);
    const px2: f64 = @floatFromInt(h2.px);
    const py2: f64 = @floatFromInt(h2.py);

    const det = vx1 * (-vy2) - (-vx2) * vy1;
    if (@abs(det) < 1e-10) return null; // Parallel

    const dx = px2 - px1;
    const dy = py2 - py1;

    const t1 = (dx * (-vy2) - (-vx2) * dy) / det;
    const t2 = (vx1 * dy - dx * vy1) / det;

    const x = px1 + vx1 * t1;
    const y = py1 + vy1 * t1;

    return .{ .x = x, .y = y, .t1 = t1, .t2 = t2 };
}

fn part1(hailstones: []const Hailstone) u64 {
    const min_coord: f64 = 200000000000000;
    const max_coord: f64 = 400000000000000;
    var count: u64 = 0;

    for (0..hailstones.len) |i| {
        for (i + 1..hailstones.len) |j| {
            const result = findIntersection2D(hailstones[i], hailstones[j]) orelse continue;

            // Check if intersection is in the future for both
            if (result.t1 < 0 or result.t2 < 0) continue;

            // Check if within bounds
            if (result.x >= min_coord and result.x <= max_coord and
                result.y >= min_coord and result.y <= max_coord)
            {
                count += 1;
            }
        }
    }

    return count;
}

// Solve 4x4 system using Gaussian elimination with f128 precision
fn solveSystem(matrix: [4][4]f128, rhs: [4]f128) [4]f128 {
    var aug: [4][5]f128 = undefined;

    // Build augmented matrix
    for (0..4) |i| {
        for (0..4) |jj| {
            aug[i][jj] = matrix[i][jj];
        }
        aug[i][4] = rhs[i];
    }

    // Forward elimination
    for (0..4) |col| {
        // Find pivot (row with largest absolute value in this column)
        var max_row = col;
        for (col + 1..4) |row| {
            if (@abs(aug[row][col]) > @abs(aug[max_row][col])) {
                max_row = row;
            }
        }

        // Swap rows
        const tmp = aug[col];
        aug[col] = aug[max_row];
        aug[max_row] = tmp;

        if (@abs(aug[col][col]) < 1e-30) continue;

        // Eliminate column
        for (col + 1..4) |row| {
            if (@abs(aug[row][col]) > 1e-30) {
                const factor = aug[row][col] / aug[col][col];
                for (col..5) |k| {
                    aug[row][k] = aug[row][k] - factor * aug[col][k];
                }
            }
        }
    }

    // Back substitution
    var solution: [4]f128 = undefined;
    var i: i32 = 3;
    while (i >= 0) : (i -= 1) {
        const idx: usize = @intCast(i);
        solution[idx] = aug[idx][4];
        for (idx + 1..4) |jj| {
            solution[idx] = solution[idx] - aug[idx][jj] * solution[jj];
        }
        solution[idx] = solution[idx] / aug[idx][idx];
    }

    return solution;
}

fn part2(hailstones: []const Hailstone) i128 {
    // Build system for XY plane
    // For hailstones i and j:
    // (vyi - vyj)*rx + (vxj - vxi)*ry + (pyj - pyi)*rvx + (pxi - pxj)*rvy = pxi*vyi - pyi*vxi - (pxj*vyj - pyj*vxj)

    var matrix_xy: [4][4]f128 = undefined;
    var rhs_xy: [4]f128 = undefined;

    for (0..4) |i| {
        const h1 = hailstones[i];
        const h2 = hailstones[i + 1];

        const a: f128 = @floatFromInt(h1.vy - h2.vy);
        const b: f128 = @floatFromInt(h2.vx - h1.vx);
        const c: f128 = @floatFromInt(h2.py - h1.py);
        const d: f128 = @floatFromInt(h1.px - h2.px);
        const e: f128 = @floatFromInt(h1.px * h1.vy - h1.py * h1.vx - (h2.px * h2.vy - h2.py * h2.vx));

        matrix_xy[i][0] = a;
        matrix_xy[i][1] = b;
        matrix_xy[i][2] = c;
        matrix_xy[i][3] = d;
        rhs_xy[i] = e;
    }

    const sol_xy = solveSystem(matrix_xy, rhs_xy);
    const rx: i128 = @intFromFloat(@round(sol_xy[0]));
    const ry: i128 = @intFromFloat(@round(sol_xy[1]));

    // Build system for XZ plane to get rz
    var matrix_xz: [4][4]f128 = undefined;
    var rhs_xz: [4]f128 = undefined;

    for (0..4) |i| {
        const h1 = hailstones[i];
        const h2 = hailstones[i + 1];

        const a: f128 = @floatFromInt(h1.vz - h2.vz);
        const b: f128 = @floatFromInt(h2.vx - h1.vx);
        const c: f128 = @floatFromInt(h2.pz - h1.pz);
        const d: f128 = @floatFromInt(h1.px - h2.px);
        const e: f128 = @floatFromInt(h1.px * h1.vz - h1.pz * h1.vx - (h2.px * h2.vz - h2.pz * h2.vx));

        matrix_xz[i][0] = a;
        matrix_xz[i][1] = b;
        matrix_xz[i][2] = c;
        matrix_xz[i][3] = d;
        rhs_xz[i] = e;
    }

    const sol_xz = solveSystem(matrix_xz, rhs_xz);
    const rz: i128 = @intFromFloat(@round(sol_xz[1]));

    return rx + ry + rz;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const content = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(content);

    const hailstones = try parseInput(allocator, content);
    defer allocator.free(hailstones);

    const p1 = part1(hailstones);
    std.debug.print("Part 1: {d}\n", .{p1});

    const p2 = part2(hailstones);
    std.debug.print("Part 2: {d}\n", .{p2});
}
