const std = @import("std");

const CacheKey = struct {
    value: u64,
    blinks: u32,
};

const CacheContext = struct {
    pub fn hash(_: CacheContext, key: CacheKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.value);
        std.hash.autoHash(&hasher, key.blinks);
        return hasher.final();
    }

    pub fn eql(_: CacheContext, a: CacheKey, b: CacheKey) bool {
        return a.value == b.value and a.blinks == b.blinks;
    }
};

const CacheMap = std.HashMap(CacheKey, u64, CacheContext, std.hash_map.default_max_load_percentage);

fn countDigits(n: u64) u32 {
    if (n == 0) return 1;
    var count: u32 = 0;
    var num = n;
    while (num > 0) : (num /= 10) {
        count += 1;
    }
    return count;
}

fn splitNumber(n: u64, digits: u32) struct { left: u64, right: u64 } {
    const half = digits / 2;
    var divisor: u64 = 1;
    var i: u32 = 0;
    while (i < half) : (i += 1) {
        divisor *= 10;
    }
    return .{
        .left = n / divisor,
        .right = n % divisor,
    };
}

fn countStones(cache: *CacheMap, value: u64, blinks: u32) !u64 {
    if (blinks == 0) {
        return 1;
    }

    const key = CacheKey{ .value = value, .blinks = blinks };

    if (cache.get(key)) |cached| {
        return cached;
    }

    var result: u64 = 0;

    // Rule 1: 0 becomes 1
    if (value == 0) {
        result = try countStones(cache, 1, blinks - 1);
    }
    // Rule 2: Even number of digits -> split
    else {
        const digits = countDigits(value);
        if (digits % 2 == 0) {
            const split = splitNumber(value, digits);
            const left_count = try countStones(cache, split.left, blinks - 1);
            const right_count = try countStones(cache, split.right, blinks - 1);
            result = left_count + right_count;
        }
        // Rule 3: Multiply by 2024
        else {
            result = try countStones(cache, value * 2024, blinks - 1);
        }
    }

    try cache.put(key, result);
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Trim whitespace
    const trimmed = std.mem.trim(u8, content, &std.ascii.whitespace);

    // Parse space-separated numbers into array
    var stone_list: std.ArrayList(u64) = .{};
    defer stone_list.deinit(allocator);

    var iter = std.mem.tokenizeScalar(u8, trimmed, ' ');
    while (iter.next()) |token| {
        const num = try std.fmt.parseInt(u64, token, 10);
        try stone_list.append(allocator, num);
    }

    const stones = stone_list.items;

    // Create cache for memoization
    var cache = CacheMap.init(allocator);
    defer cache.deinit();

    // Part 1: 25 blinks
    var part1_total: u64 = 0;
    for (stones) |stone| {
        part1_total += try countStones(&cache, stone, 25);
    }

    // Part 2: 75 blinks (reuse cache)
    var part2_total: u64 = 0;
    for (stones) |stone| {
        part2_total += try countStones(&cache, stone, 75);
    }

    std.debug.print("Part 1: {d}\n", .{part1_total});
    std.debug.print("Part 2: {d}\n", .{part2_total});
}
