const std = @import("std");

const Range = struct {
    dst_start: u64,
    src_start: u64,
    length: u64,
};

const SeedRange = struct {
    start: u64,
    end: u64,
};

fn parseInput(allocator: std.mem.Allocator, text: []const u8) !struct {
    seeds: []u64,
    maps: [][]Range,
} {
    var sections = std.mem.splitSequence(u8, text, "\n\n");

    // Parse seeds
    const seeds_section = sections.next() orelse return error.InvalidInput;
    var seeds_line_iter = std.mem.splitSequence(u8, seeds_section, ": ");
    _ = seeds_line_iter.next(); // Skip "seeds" label
    const seeds_str = seeds_line_iter.next() orelse return error.InvalidInput;

    var seeds_list: std.ArrayListUnmanaged(u64) = .empty;
    defer seeds_list.deinit(allocator);

    var seeds_iter = std.mem.tokenizeAny(u8, seeds_str, " \n");
    while (seeds_iter.next()) |seed_str| {
        const seed = try std.fmt.parseInt(u64, seed_str, 10);
        try seeds_list.append(allocator, seed);
    }

    // Parse maps
    var maps_list: std.ArrayListUnmanaged([]Range) = .empty;
    errdefer {
        for (maps_list.items) |m| {
            allocator.free(m);
        }
        maps_list.deinit(allocator);
    }

    while (sections.next()) |section| {
        if (section.len == 0) continue;

        var lines = std.mem.splitScalar(u8, section, '\n');
        _ = lines.next(); // Skip header

        var ranges_list: std.ArrayListUnmanaged(Range) = .empty;
        defer ranges_list.deinit(allocator);

        while (lines.next()) |line| {
            if (line.len == 0) continue;

            var nums = std.mem.tokenizeAny(u8, line, " ");
            const dst_start = try std.fmt.parseInt(u64, nums.next() orelse continue, 10);
            const src_start = try std.fmt.parseInt(u64, nums.next() orelse continue, 10);
            const length = try std.fmt.parseInt(u64, nums.next() orelse continue, 10);

            try ranges_list.append(allocator, .{
                .dst_start = dst_start,
                .src_start = src_start,
                .length = length,
            });
        }

        try maps_list.append(allocator, try ranges_list.toOwnedSlice(allocator));
    }

    return .{
        .seeds = try seeds_list.toOwnedSlice(allocator),
        .maps = try maps_list.toOwnedSlice(allocator),
    };
}

fn applyMap(value: u64, ranges: []const Range) u64 {
    for (ranges) |r| {
        if (value >= r.src_start and value < r.src_start + r.length) {
            return r.dst_start + (value - r.src_start);
        }
    }
    return value;
}

fn seedToLocation(seed: u64, maps: []const []Range) u64 {
    var value = seed;
    for (maps) |map_ranges| {
        value = applyMap(value, map_ranges);
    }
    return value;
}

fn part1(seeds: []const u64, maps: []const []Range) u64 {
    var min_loc: u64 = std.math.maxInt(u64);
    for (seeds) |seed| {
        const loc = seedToLocation(seed, maps);
        if (loc < min_loc) {
            min_loc = loc;
        }
    }
    return min_loc;
}

fn applyMapToRanges(allocator: std.mem.Allocator, input_ranges: []const SeedRange, map_ranges: []const Range) ![]SeedRange {
    var result: std.ArrayListUnmanaged(SeedRange) = .empty;
    errdefer result.deinit(allocator);

    for (input_ranges) |in_range| {
        var remaining: std.ArrayListUnmanaged(SeedRange) = .empty;
        defer remaining.deinit(allocator);
        try remaining.append(allocator, .{ .start = in_range.start, .end = in_range.end });

        for (map_ranges) |r| {
            const src_end = r.src_start + r.length;
            var new_remaining: std.ArrayListUnmanaged(SeedRange) = .empty;
            defer new_remaining.deinit(allocator);

            for (remaining.items) |rem| {
                // Part before the map range (unmapped)
                if (rem.start < r.src_start) {
                    try new_remaining.append(allocator, .{
                        .start = rem.start,
                        .end = @min(rem.end, r.src_start),
                    });
                }

                // Part within the map range (mapped)
                const overlap_start = @max(rem.start, r.src_start);
                const overlap_end = @min(rem.end, src_end);
                if (overlap_start < overlap_end) {
                    const offset = r.dst_start -% r.src_start;
                    try result.append(allocator, .{
                        .start = overlap_start +% offset,
                        .end = overlap_end +% offset,
                    });
                }

                // Part after the map range (unmapped)
                if (rem.end > src_end) {
                    try new_remaining.append(allocator, .{
                        .start = @max(rem.start, src_end),
                        .end = rem.end,
                    });
                }
            }

            remaining.clearRetainingCapacity();
            try remaining.appendSlice(allocator, new_remaining.items);
        }

        // Any remaining parts are unmapped (identity)
        try result.appendSlice(allocator, remaining.items);
    }

    return result.toOwnedSlice(allocator);
}

fn part2(allocator: std.mem.Allocator, seeds: []const u64, maps: []const []Range) !u64 {
    // Convert seeds to ranges
    var ranges: std.ArrayListUnmanaged(SeedRange) = .empty;
    defer ranges.deinit(allocator);

    var i: usize = 0;
    while (i < seeds.len) : (i += 2) {
        const start = seeds[i];
        const length = seeds[i + 1];
        try ranges.append(allocator, .{ .start = start, .end = start + length });
    }

    // Apply each map to the ranges
    var current_ranges = try ranges.toOwnedSlice(allocator);
    defer allocator.free(current_ranges);

    for (maps) |map_ranges| {
        const new_ranges = try applyMapToRanges(allocator, current_ranges, map_ranges);
        allocator.free(current_ranges);
        current_ranges = new_ranges;
    }

    // Find minimum start of any range
    var min_loc: u64 = std.math.maxInt(u64);
    for (current_ranges) |r| {
        if (r.start < min_loc) {
            min_loc = r.start;
        }
    }

    return min_loc;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input
    const input = try std.fs.cwd().readFileAlloc(allocator, "../input.txt", 1024 * 1024);
    defer allocator.free(input);

    // Parse input
    const parsed = try parseInput(allocator, input);
    defer {
        allocator.free(parsed.seeds);
        for (parsed.maps) |m| {
            allocator.free(m);
        }
        allocator.free(parsed.maps);
    }

    // Solve both parts
    const result1 = part1(parsed.seeds, parsed.maps);
    const result2 = try part2(allocator, parsed.seeds, parsed.maps);

    std.debug.print("Part 1: {d}\n", .{result1});
    std.debug.print("Part 2: {d}\n", .{result2});
}
