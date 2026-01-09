const std = @import("std");
const Allocator = std.mem.Allocator;

const Blueprint = struct {
    id: u32,
    ore_ore: u32,
    clay_ore: u32,
    obs_ore: u32,
    obs_clay: u32,
    geo_ore: u32,
    geo_obs: u32,
    max_ore: u32,
    max_clay: u32,
    max_obs: u32,
};

const State = struct {
    time: u32,
    ore: u32,
    clay: u32,
    obs: u32,
    geodes: u32,
    ore_r: u32,
    clay_r: u32,
    obs_r: u32,
    geo_r: u32,
};

fn parseBlueprints(allocator: Allocator, input: []const u8) !std.ArrayList(Blueprint) {
    var blueprints: std.ArrayList(Blueprint) = .{};
    var lines = std.mem.splitScalar(u8, input, '\n');

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse: Blueprint N: Each ore robot costs X ore. Each clay robot costs Y ore. Each obsidian robot costs Z ore and W clay. Each geode robot costs A ore and B obsidian.
        var nums: [7]u32 = undefined;
        var num_idx: usize = 0;
        var i: usize = 0;

        while (i < line.len and num_idx < 7) {
            if (line[i] >= '0' and line[i] <= '9') {
                var num: u32 = 0;
                while (i < line.len and line[i] >= '0' and line[i] <= '9') {
                    num = num * 10 + @as(u32, line[i] - '0');
                    i += 1;
                }
                nums[num_idx] = num;
                num_idx += 1;
            } else {
                i += 1;
            }
        }

        if (num_idx == 7) {
            const max_ore = @max(@max(nums[1], nums[2]), @max(nums[3], nums[5]));
            try blueprints.append(allocator, Blueprint{
                .id = nums[0],
                .ore_ore = nums[1],
                .clay_ore = nums[2],
                .obs_ore = nums[3],
                .obs_clay = nums[4],
                .geo_ore = nums[5],
                .geo_obs = nums[6],
                .max_ore = max_ore,
                .max_clay = nums[4],
                .max_obs = nums[6],
            });
        }
    }
    return blueprints;
}

fn maxGeodes(allocator: Allocator, bp: Blueprint, time_limit: u32) !u32 {
    var best: u32 = 0;

    var stack: std.ArrayList(State) = .{};
    defer stack.deinit(allocator);

    try stack.append(allocator, State{
        .time = 0,
        .ore = 0,
        .clay = 0,
        .obs = 0,
        .geodes = 0,
        .ore_r = 1,
        .clay_r = 0,
        .obs_r = 0,
        .geo_r = 0,
    });

    while (stack.pop()) |s| {
        // Pruning: upper bound on possible geodes
        const remaining = time_limit - s.time;
        const upper_bound = s.geodes + s.geo_r * remaining + (remaining * (remaining -| 1)) / 2;
        if (upper_bound <= best) continue;

        if (s.time == time_limit) {
            best = @max(best, s.geodes);
            continue;
        }

        // Cap resources to max we could ever use
        const capped_ore = @min(s.ore, remaining * bp.max_ore);
        const capped_clay = @min(s.clay, remaining * bp.max_clay);
        const capped_obs = @min(s.obs, remaining * bp.max_obs);

        // Collect resources
        const new_ore = capped_ore + s.ore_r;
        const new_clay = capped_clay + s.clay_r;
        const new_obs = capped_obs + s.obs_r;
        const new_geodes = s.geodes + s.geo_r;

        // Try building geode robot (always do if possible)
        if (capped_ore >= bp.geo_ore and capped_obs >= bp.geo_obs) {
            try stack.append(allocator, State{
                .time = s.time + 1,
                .ore = new_ore - bp.geo_ore,
                .clay = new_clay,
                .obs = new_obs - bp.geo_obs,
                .geodes = new_geodes,
                .ore_r = s.ore_r,
                .clay_r = s.clay_r,
                .obs_r = s.obs_r,
                .geo_r = s.geo_r + 1,
            });
            continue; // If we can build geode, always do
        }

        // Try building obsidian robot
        if (capped_ore >= bp.obs_ore and capped_clay >= bp.obs_clay and s.obs_r < bp.max_obs) {
            try stack.append(allocator, State{
                .time = s.time + 1,
                .ore = new_ore - bp.obs_ore,
                .clay = new_clay - bp.obs_clay,
                .obs = new_obs,
                .geodes = new_geodes,
                .ore_r = s.ore_r,
                .clay_r = s.clay_r,
                .obs_r = s.obs_r + 1,
                .geo_r = s.geo_r,
            });
        }

        // Try building clay robot
        if (capped_ore >= bp.clay_ore and s.clay_r < bp.max_clay) {
            try stack.append(allocator, State{
                .time = s.time + 1,
                .ore = new_ore - bp.clay_ore,
                .clay = new_clay,
                .obs = new_obs,
                .geodes = new_geodes,
                .ore_r = s.ore_r,
                .clay_r = s.clay_r + 1,
                .obs_r = s.obs_r,
                .geo_r = s.geo_r,
            });
        }

        // Try building ore robot
        if (capped_ore >= bp.ore_ore and s.ore_r < bp.max_ore) {
            try stack.append(allocator, State{
                .time = s.time + 1,
                .ore = new_ore - bp.ore_ore,
                .clay = new_clay,
                .obs = new_obs,
                .geodes = new_geodes,
                .ore_r = s.ore_r + 1,
                .clay_r = s.clay_r,
                .obs_r = s.obs_r,
                .geo_r = s.geo_r,
            });
        }

        // Do nothing (wait)
        try stack.append(allocator, State{
            .time = s.time + 1,
            .ore = new_ore,
            .clay = new_clay,
            .obs = new_obs,
            .geodes = new_geodes,
            .ore_r = s.ore_r,
            .clay_r = s.clay_r,
            .obs_r = s.obs_r,
            .geo_r = s.geo_r,
        });
    }

    return best;
}

fn part1(allocator: Allocator, blueprints: []const Blueprint) !u32 {
    var total: u32 = 0;
    for (blueprints) |bp| {
        const geodes = try maxGeodes(allocator, bp, 24);
        total += bp.id * geodes;
    }
    return total;
}

fn part2(allocator: Allocator, blueprints: []const Blueprint) !u64 {
    var result: u64 = 1;
    const count = @min(blueprints.len, 3);
    for (blueprints[0..count]) |bp| {
        const geodes = try maxGeodes(allocator, bp, 32);
        result *= @as(u64, geodes);
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    const trimmed = std.mem.trim(u8, buffer, &[_]u8{ '\n', '\r', ' ' });

    var blueprints = try parseBlueprints(allocator, trimmed);
    defer blueprints.deinit(allocator);

    const answer1 = try part1(allocator, blueprints.items);
    const answer2 = try part2(allocator, blueprints.items);

    std.debug.print("Part 1: {d}\n", .{answer1});
    std.debug.print("Part 2: {d}\n", .{answer2});
}
