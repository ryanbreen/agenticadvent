const std = @import("std");
const Allocator = std.mem.Allocator;

// Fraction type for rational arithmetic in Part 2
const Fraction = struct {
    num: i64,
    den: i64,

    fn init(num: i64, den: i64) Fraction {
        if (den == 0) return .{ .num = 0, .den = 1 };
        const g = gcd(@as(i64, @intCast(@abs(num))), @as(i64, @intCast(@abs(den))));
        const sign: i64 = if (den < 0) -1 else 1;
        return .{
            .num = sign * @divTrunc(num, g),
            .den = @divTrunc(@as(i64, @intCast(@abs(den))), g),
        };
    }

    fn add(self: Fraction, other: Fraction) Fraction {
        return init(self.num * other.den + other.num * self.den, self.den * other.den);
    }

    fn sub(self: Fraction, other: Fraction) Fraction {
        return init(self.num * other.den - other.num * self.den, self.den * other.den);
    }

    fn mul(self: Fraction, other: Fraction) Fraction {
        return init(self.num * other.num, self.den * other.den);
    }

    fn div(self: Fraction, other: Fraction) Fraction {
        return init(self.num * other.den, self.den * other.num);
    }

    fn neg(self: Fraction) Fraction {
        return .{ .num = -self.num, .den = self.den };
    }

    fn isZero(self: Fraction) bool {
        return self.num == 0;
    }

    fn toFloat(self: Fraction) f64 {
        return @as(f64, @floatFromInt(self.num)) / @as(f64, @floatFromInt(self.den));
    }

    fn isInteger(self: Fraction) bool {
        return self.den == 1;
    }

    fn toInt(self: Fraction) i64 {
        return @divTrunc(self.num, self.den);
    }

    fn cmp(self: Fraction, other: Fraction) std.math.Order {
        const diff = self.sub(other);
        if (diff.num > 0) return .gt;
        if (diff.num < 0) return .lt;
        return .eq;
    }
};

fn gcd(a: i64, b: i64) i64 {
    var x = a;
    var y = b;
    while (y != 0) {
        const t = y;
        y = @mod(x, y);
        x = t;
    }
    return x;
}

const Machine1 = struct {
    n_lights: u32,
    target: u64,
    buttons: []u64,
};

const Machine2 = struct {
    n_counters: u32,
    joltage: []i64,
    buttons: [][]u32,
};

fn parseLine1(allocator: Allocator, line: []const u8) !Machine1 {
    // Parse [.##.] pattern
    const bracket_start = std.mem.indexOf(u8, line, "[") orelse return error.InvalidFormat;
    const bracket_end = std.mem.indexOf(u8, line[bracket_start..], "]") orelse return error.InvalidFormat;
    const pattern = line[bracket_start + 1 .. bracket_start + bracket_end];
    const n_lights: u32 = @intCast(pattern.len);

    // Build target bitmask
    var target: u64 = 0;
    for (pattern, 0..) |c, i| {
        if (c == '#') {
            target |= @as(u64, 1) << @intCast(i);
        }
    }

    // Parse button schematics (x,y,z)
    var buttons: std.ArrayList(u64) = .{};
    var i: usize = bracket_start + bracket_end + 1;
    while (i < line.len) {
        if (line[i] == '(') {
            const close = std.mem.indexOfPos(u8, line, i, ")") orelse break;
            const button_str = line[i + 1 .. close];

            // Parse comma-separated indices
            var mask: u64 = 0;
            var it = std.mem.splitScalar(u8, button_str, ',');
            while (it.next()) |num_str| {
                const idx = try std.fmt.parseInt(u32, num_str, 10);
                mask |= @as(u64, 1) << @intCast(idx);
            }
            try buttons.append(allocator, mask);
            i = close + 1;
        } else {
            i += 1;
        }
    }

    return Machine1{
        .n_lights = n_lights,
        .target = target,
        .buttons = try buttons.toOwnedSlice(allocator),
    };
}

fn parseLine2(allocator: Allocator, line: []const u8) !Machine2 {
    // Parse {x,y,z} joltage requirements
    const brace_start = std.mem.indexOf(u8, line, "{") orelse return error.InvalidFormat;
    const brace_end = std.mem.indexOf(u8, line[brace_start..], "}") orelse return error.InvalidFormat;
    const joltage_str = line[brace_start + 1 .. brace_start + brace_end];

    var joltage: std.ArrayList(i64) = .{};
    var jit = std.mem.splitScalar(u8, joltage_str, ',');
    while (jit.next()) |num_str| {
        const val = try std.fmt.parseInt(i64, num_str, 10);
        try joltage.append(allocator, val);
    }

    // Parse button schematics (x,y,z)
    var buttons: std.ArrayList([]u32) = .{};
    var i: usize = 0;
    while (i < brace_start) {
        if (line[i] == '(') {
            const close = std.mem.indexOfPos(u8, line, i, ")") orelse break;
            const button_str = line[i + 1 .. close];

            var indices: std.ArrayList(u32) = .{};
            var it = std.mem.splitScalar(u8, button_str, ',');
            while (it.next()) |num_str| {
                const idx = try std.fmt.parseInt(u32, num_str, 10);
                try indices.append(allocator, idx);
            }
            try buttons.append(allocator, try indices.toOwnedSlice(allocator));
            i = close + 1;
        } else {
            i += 1;
        }
    }

    return Machine2{
        .n_counters = @intCast(joltage.items.len),
        .joltage = try joltage.toOwnedSlice(allocator),
        .buttons = try buttons.toOwnedSlice(allocator),
    };
}

fn solveMachine1(machine: Machine1) u64 {
    const n_buttons = machine.buttons.len;
    var min_presses: u64 = std.math.maxInt(u64);

    // Brute force all 2^n combinations
    const max_combo = @as(u64, 1) << @intCast(n_buttons);
    var combo: u64 = 0;
    while (combo < max_combo) : (combo += 1) {
        var state: u64 = 0;
        var presses: u64 = 0;

        var i: u6 = 0;
        while (i < n_buttons) : (i += 1) {
            if (combo & (@as(u64, 1) << i) != 0) {
                state ^= machine.buttons[i];
                presses += 1;
            }
        }

        if (state == machine.target) {
            min_presses = @min(min_presses, presses);
        }
    }

    return if (min_presses == std.math.maxInt(u64)) 0 else min_presses;
}

fn solveMachine2(allocator: Allocator, machine: Machine2) !u64 {
    const n_counters = machine.n_counters;
    const n_buttons = machine.buttons.len;

    if (n_buttons == 0) {
        for (machine.joltage) |j| {
            if (j != 0) return std.math.maxInt(u64);
        }
        return 0;
    }

    // Build matrix A (n_counters x n_buttons)
    var A = try allocator.alloc([]Fraction, n_counters);
    for (0..n_counters) |i| {
        A[i] = try allocator.alloc(Fraction, n_buttons);
        for (0..n_buttons) |j| {
            A[i][j] = Fraction.init(0, 1);
        }
    }

    for (machine.buttons, 0..) |button, j| {
        for (button) |idx| {
            if (idx < n_counters) {
                A[idx][j] = Fraction.init(1, 1);
            }
        }
    }

    var b = try allocator.alloc(Fraction, n_counters);
    for (machine.joltage, 0..) |jolt, i| {
        b[i] = Fraction.init(jolt, 1);
    }

    // Augmented matrix [A | b]
    var aug = try allocator.alloc([]Fraction, n_counters);
    for (0..n_counters) |i| {
        aug[i] = try allocator.alloc(Fraction, n_buttons + 1);
        for (0..n_buttons) |j| {
            aug[i][j] = A[i][j];
        }
        aug[i][n_buttons] = b[i];
    }

    // Gaussian elimination
    var pivot_cols: std.ArrayList(struct { col: usize, row: usize }) = .{};
    var pivot_row: usize = 0;

    for (0..n_buttons) |col| {
        // Find non-zero entry
        var found: ?usize = null;
        for (pivot_row..n_counters) |row| {
            if (!aug[row][col].isZero()) {
                found = row;
                break;
            }
        }

        if (found == null) continue;

        // Swap rows
        const tmp = aug[pivot_row];
        aug[pivot_row] = aug[found.?];
        aug[found.?] = tmp;

        try pivot_cols.append(allocator, .{ .col = col, .row = pivot_row });

        // Scale pivot row
        const scale = aug[pivot_row][col];
        for (0..n_buttons + 1) |c| {
            aug[pivot_row][c] = aug[pivot_row][c].div(scale);
        }

        // Eliminate column
        for (0..n_counters) |row| {
            if (row != pivot_row and !aug[row][col].isZero()) {
                const factor = aug[row][col];
                for (0..n_buttons + 1) |c| {
                    aug[row][c] = aug[row][c].sub(factor.mul(aug[pivot_row][c]));
                }
            }
        }

        pivot_row += 1;
    }

    // Check for inconsistency
    for (pivot_row..n_counters) |row| {
        if (!aug[row][n_buttons].isZero()) {
            return std.math.maxInt(u64);
        }
    }

    // Identify free variables
    var pivot_col_set = std.AutoHashMap(usize, void).init(allocator);
    for (pivot_cols.items) |pc| {
        try pivot_col_set.put(pc.col, {});
    }

    var free_vars: std.ArrayList(usize) = .{};
    for (0..n_buttons) |c| {
        if (!pivot_col_set.contains(c)) {
            try free_vars.append(allocator, c);
        }
    }

    const n_free = free_vars.items.len;

    // No free variables - unique solution
    if (n_free == 0) {
        var solution = try allocator.alloc(Fraction, n_buttons);
        for (0..n_buttons) |i| {
            solution[i] = Fraction.init(0, 1);
        }
        for (pivot_cols.items) |pc| {
            solution[pc.col] = aug[pc.row][n_buttons];
        }

        var total: u64 = 0;
        for (solution) |val| {
            if (val.cmp(Fraction.init(0, 1)) == .lt or !val.isInteger()) {
                return std.math.maxInt(u64);
            }
            total += @intCast(val.toInt());
        }
        return total;
    }

    // Build null space vectors and particular solution
    var null_vectors = try allocator.alloc([]Fraction, n_free);
    for (free_vars.items, 0..) |fv, i| {
        null_vectors[i] = try allocator.alloc(Fraction, n_buttons);
        for (0..n_buttons) |j| {
            null_vectors[i][j] = Fraction.init(0, 1);
        }
        null_vectors[i][fv] = Fraction.init(1, 1);

        for (pivot_cols.items) |pc| {
            null_vectors[i][pc.col] = aug[pc.row][fv].neg();
        }
    }

    var particular = try allocator.alloc(Fraction, n_buttons);
    for (0..n_buttons) |j| {
        particular[j] = Fraction.init(0, 1);
    }
    for (pivot_cols.items) |pc| {
        particular[pc.col] = aug[pc.row][n_buttons];
    }

    // Search for optimal solution
    const max_j = blk: {
        var max: i64 = 100;
        for (machine.joltage) |j| {
            max = @max(max, j);
        }
        break :blk max;
    };

    var min_total: u64 = std.math.maxInt(u64);

    if (n_free == 1) {
        // 1D search
        var t_low: f64 = -1e18;
        var t_high: f64 = 1e18;

        for (0..n_buttons) |j| {
            const p = particular[j].toFloat();
            const nv = null_vectors[0][j].toFloat();

            if (nv == 0.0) {
                if (p < 0.0) return std.math.maxInt(u64);
            } else if (nv > 0.0) {
                t_low = @max(t_low, -p / nv);
            } else {
                t_high = @min(t_high, -p / nv);
            }
        }

        if (t_low > t_high) return std.math.maxInt(u64);

        const t_low_int = @as(i64, @intFromFloat(@ceil(t_low)));
        const t_high_int = @as(i64, @intFromFloat(@floor(t_high)));

        var t = t_low_int;
        while (t <= t_high_int) : (t += 1) {
            const t_frac = Fraction.init(t, 1);
            var valid = true;
            var total: u64 = 0;

            for (0..n_buttons) |j| {
                const val = particular[j].add(t_frac.mul(null_vectors[0][j]));
                if (val.cmp(Fraction.init(0, 1)) == .lt or !val.isInteger()) {
                    valid = false;
                    break;
                }
                total += @intCast(val.toInt());
            }

            if (valid) {
                min_total = @min(min_total, total);
            }
        }
    } else if (n_free == 2) {
        // 2D search
        var bounds = try allocator.alloc(struct { low: i64, high: i64 }, n_free);
        for (0..n_free) |i| {
            var t_low: f64 = -1e18;
            var t_high: f64 = 1e18;

            for (0..n_buttons) |j| {
                const p = particular[j].toFloat();
                const nv = null_vectors[i][j].toFloat();
                if (nv > 0.0) {
                    t_low = @max(t_low, -p / nv);
                } else if (nv < 0.0) {
                    t_high = @min(t_high, -p / nv);
                }
            }

            bounds[i] = .{
                .low = @max(@as(i64, @intFromFloat(@floor(t_low - @as(f64, @floatFromInt(max_j))))), -max_j * 2),
                .high = @min(@as(i64, @intFromFloat(@ceil(t_high + @as(f64, @floatFromInt(max_j))))), max_j * 2),
            };
        }

        var t0 = bounds[0].low;
        while (t0 <= bounds[0].high) : (t0 += 1) {
            const t0_frac = Fraction.init(t0, 1);
            var intermediate = try allocator.alloc(Fraction, n_buttons);
            for (0..n_buttons) |j| {
                intermediate[j] = particular[j].add(t0_frac.mul(null_vectors[0][j]));
            }

            // Compute bounds for t1
            var t1_low: f64 = -1e18;
            var t1_high: f64 = 1e18;
            for (0..n_buttons) |j| {
                const p = intermediate[j].toFloat();
                const nv = null_vectors[1][j].toFloat();
                if (nv > 0.0) {
                    t1_low = @max(t1_low, -p / nv);
                } else if (nv < 0.0) {
                    t1_high = @min(t1_high, -p / nv);
                }
            }

            const t1_low_int = @as(i64, @intFromFloat(@ceil(t1_low)));
            const t1_high_int = @as(i64, @intFromFloat(@floor(t1_high)));

            var t1 = t1_low_int;
            while (t1 <= t1_high_int) : (t1 += 1) {
                const t1_frac = Fraction.init(t1, 1);
                var valid = true;
                var total: u64 = 0;

                for (0..n_buttons) |j| {
                    const val = intermediate[j].add(t1_frac.mul(null_vectors[1][j]));
                    if (val.cmp(Fraction.init(0, 1)) == .lt or !val.isInteger()) {
                        valid = false;
                        break;
                    }
                    total += @intCast(val.toInt());
                }

                if (valid and total < min_total) {
                    min_total = total;
                }
            }
        }
    } else {
        // Fall back to simpler bounds for higher dimensions
        const bound = max_j;

        if (n_free == 3) {
            var t0: i64 = -bound;
            while (t0 <= bound) : (t0 += 1) {
                const t0_frac = Fraction.init(t0, 1);
                var inter0 = try allocator.alloc(Fraction, n_buttons);
                for (0..n_buttons) |j| {
                    inter0[j] = particular[j].add(t0_frac.mul(null_vectors[0][j]));
                }

                var t1_low: f64 = -1e18;
                var t1_high: f64 = 1e18;
                for (0..n_buttons) |j| {
                    const p = inter0[j].toFloat();
                    const nv = null_vectors[1][j].toFloat();
                    if (nv > 0.0) {
                        t1_low = @max(t1_low, -p / nv - @as(f64, @floatFromInt(bound)));
                    } else if (nv < 0.0) {
                        t1_high = @min(t1_high, -p / nv + @as(f64, @floatFromInt(bound)));
                    }
                }

                const t1_low_int = @max(@as(i64, @intFromFloat(@ceil(t1_low))), -bound);
                const t1_high_int = @min(@as(i64, @intFromFloat(@floor(t1_high))), bound);

                var t1 = t1_low_int;
                while (t1 <= t1_high_int) : (t1 += 1) {
                    const t1_frac = Fraction.init(t1, 1);
                    var inter1 = try allocator.alloc(Fraction, n_buttons);
                    for (0..n_buttons) |j| {
                        inter1[j] = inter0[j].add(t1_frac.mul(null_vectors[1][j]));
                    }

                    var t2_low: f64 = -1e18;
                    var t2_high: f64 = 1e18;
                    for (0..n_buttons) |j| {
                        const p = inter1[j].toFloat();
                        const nv = null_vectors[2][j].toFloat();
                        if (nv > 0.0) {
                            t2_low = @max(t2_low, -p / nv);
                        } else if (nv < 0.0) {
                            t2_high = @min(t2_high, -p / nv);
                        }
                    }

                    const t2_low_int = @as(i64, @intFromFloat(@ceil(t2_low)));
                    const t2_high_int = @as(i64, @intFromFloat(@floor(t2_high)));

                    var t2 = t2_low_int;
                    while (t2 <= t2_high_int) : (t2 += 1) {
                        const t2_frac = Fraction.init(t2, 1);
                        var valid = true;
                        var total: u64 = 0;

                        for (0..n_buttons) |j| {
                            const val = inter1[j].add(t2_frac.mul(null_vectors[2][j]));
                            if (val.cmp(Fraction.init(0, 1)) == .lt or !val.isInteger()) {
                                valid = false;
                                break;
                            }
                            total += @intCast(val.toInt());
                        }

                        if (valid and total < min_total) {
                            min_total = total;
                        }
                    }
                }
            }
        }
    }

    return if (min_total == std.math.maxInt(u64)) 0 else min_total;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines_list: std.ArrayList([]const u8) = .{};
    defer lines_list.deinit(allocator);

    var it = std.mem.splitScalar(u8, content, '\n');
    while (it.next()) |line| {
        if (line.len > 0) {
            try lines_list.append(allocator, line);
        }
    }

    const lines = try lines_list.toOwnedSlice(allocator);
    defer allocator.free(lines);

    // Part 1
    var total1: u64 = 0;
    for (lines) |line| {
        const machine = try parseLine1(allocator, line);
        defer allocator.free(machine.buttons);
        const result = solveMachine1(machine);
        total1 += result;
    }

    std.debug.print("Part 1: {d}\n", .{total1});

    // Part 2
    var total2: u64 = 0;
    for (lines) |line| {
        const machine = try parseLine2(allocator, line);
        defer {
            allocator.free(machine.joltage);
            for (machine.buttons) |button| {
                allocator.free(button);
            }
            allocator.free(machine.buttons);
        }
        const result = try solveMachine2(allocator, machine);
        total2 += result;
    }

    std.debug.print("Part 2: {d}\n", .{total2});
}
