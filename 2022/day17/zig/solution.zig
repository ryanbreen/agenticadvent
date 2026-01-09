const std = @import("std");
const Allocator = std.mem.Allocator;

const WIDTH: i32 = 7;
const PROFILE_DEPTH: usize = 30;

// Rock shapes as arrays of (dx, dy) offsets from bottom-left
const ROCKS = [5][]const [2]i32{
    // Horizontal line: ####
    &[_][2]i32{ .{ 0, 0 }, .{ 1, 0 }, .{ 2, 0 }, .{ 3, 0 } },
    // Plus: .#. / ### / .#.
    &[_][2]i32{ .{ 1, 0 }, .{ 0, 1 }, .{ 1, 1 }, .{ 2, 1 }, .{ 1, 2 } },
    // L shape (reversed): ..# / ..# / ###
    &[_][2]i32{ .{ 0, 0 }, .{ 1, 0 }, .{ 2, 0 }, .{ 2, 1 }, .{ 2, 2 } },
    // Vertical line
    &[_][2]i32{ .{ 0, 0 }, .{ 0, 1 }, .{ 0, 2 }, .{ 0, 3 } },
    // Square
    &[_][2]i32{ .{ 0, 0 }, .{ 1, 0 }, .{ 0, 1 }, .{ 1, 1 } },
};

// State key for cycle detection
const StateKey = struct {
    rock_type: u3,
    jet_idx: usize,
    profile: [7]i32,
};

fn makeCoordKey(x: i32, y: i64) u64 {
    // Combine x (0-6) and y into a single key
    const xu: u64 = @intCast(@as(u32, @bitCast(x)));
    const yu: u64 = @bitCast(y);
    return (yu << 3) | xu;
}

fn simulate(allocator: Allocator, jets: []const u8, num_rocks: u64) !i64 {
    // Store occupied cells as a hash set
    var occupied = std.AutoHashMap(u64, void).init(allocator);
    defer occupied.deinit();

    // For cycle detection - stores state -> rock_num
    var states = std.AutoHashMap(StateKey, u64).init(allocator);
    defer states.deinit();

    // Store heights for cycle calculation
    var heights: std.ArrayList(i64) = .{};
    defer heights.deinit(allocator);

    var height: i64 = 0;
    var jet_idx: usize = 0;

    var rock_num: u64 = 0;
    while (rock_num < num_rocks) : (rock_num += 1) {
        const rock_type: usize = @intCast(rock_num % 5);
        const rock = ROCKS[rock_type];

        // Starting position: left edge at x=2, bottom at y=height+3
        var x: i32 = 2;
        var y: i64 = height + 3;

        while (true) {
            // Jet push
            const jet = jets[jet_idx];
            jet_idx = (jet_idx + 1) % jets.len;

            const dx: i32 = if (jet == '>') 1 else -1;

            // Check if can move horizontally
            var can_move = true;
            for (rock) |offset| {
                const nx = x + offset[0] + dx;
                const ny = y + offset[1];
                if (nx < 0 or nx >= WIDTH) {
                    can_move = false;
                    break;
                }
                if (occupied.contains(makeCoordKey(nx, ny))) {
                    can_move = false;
                    break;
                }
            }

            if (can_move) {
                x += dx;
            }

            // Fall down
            var can_fall = true;
            for (rock) |offset| {
                const nx = x + offset[0];
                const ny = y + offset[1] - 1;
                if (ny < 0) {
                    can_fall = false;
                    break;
                }
                if (occupied.contains(makeCoordKey(nx, ny))) {
                    can_fall = false;
                    break;
                }
            }

            if (can_fall) {
                y -= 1;
            } else {
                // Rock stops - add to occupied
                for (rock) |offset| {
                    const final_x = x + offset[0];
                    const final_y = y + offset[1];
                    try occupied.put(makeCoordKey(final_x, final_y), {});
                    height = @max(height, final_y + 1);
                }
                break;
            }
        }

        try heights.append(allocator, height);

        // Cycle detection for Part 2
        if (num_rocks > 10000) {
            // Create state key from surface profile
            var profile: [7]i32 = undefined;
            for (0..7) |col| {
                const col_i32: i32 = @intCast(col);
                var found = false;
                for (0..PROFILE_DEPTH) |row| {
                    const row_i64: i64 = @intCast(row);
                    if (occupied.contains(makeCoordKey(col_i32, height - 1 - row_i64))) {
                        profile[col] = @intCast(row);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    profile[col] = PROFILE_DEPTH;
                }
            }

            const state = StateKey{
                .rock_type = @intCast(rock_type),
                .jet_idx = jet_idx,
                .profile = profile,
            };

            if (states.get(state)) |cycle_start| {
                // Found cycle!
                const cycle_len = rock_num - cycle_start;
                const cycle_height = height - heights.items[cycle_start];

                // Calculate final height
                const remaining = num_rocks - rock_num - 1;
                const full_cycles = remaining / cycle_len;
                const leftover = remaining % cycle_len;

                var final_height = height + @as(i64, @intCast(full_cycles)) * cycle_height;
                if (leftover > 0) {
                    final_height += heights.items[cycle_start + leftover] - heights.items[cycle_start];
                }

                return final_height;
            }

            try states.put(state, rock_num);
        }
    }

    return height;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    // Trim whitespace
    var jets = buffer;
    while (jets.len > 0 and (jets[jets.len - 1] == '\n' or jets[jets.len - 1] == '\r' or jets[jets.len - 1] == ' ')) {
        jets = jets[0 .. jets.len - 1];
    }

    // Part 1: 2022 rocks
    const part1 = try simulate(allocator, jets, 2022);

    // Part 2: 1000000000000 rocks
    const part2 = try simulate(allocator, jets, 1000000000000);

    std.debug.print("Part 1: {d}\n", .{part1});
    std.debug.print("Part 2: {d}\n", .{part2});
}
