const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const data = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(data);

    // Parse input - split into rules and updates sections
    var sections = std.mem.tokenizeSequence(u8, data, "\n\n");
    const rules_section = sections.next().?;
    const updates_section = sections.next().?;

    // Parse rules into a hash map: rules[before] = set of pages that must come after
    var rules = std.AutoHashMap(i32, std.AutoHashMap(i32, void)).init(allocator);
    defer {
        var it = rules.valueIterator();
        while (it.next()) |set| {
            set.deinit();
        }
        rules.deinit();
    }

    var rules_iter = std.mem.tokenizeScalar(u8, rules_section, '\n');
    while (rules_iter.next()) |line| {
        if (line.len == 0) continue;
        var parts = std.mem.tokenizeScalar(u8, line, '|');
        const before = try std.fmt.parseInt(i32, parts.next().?, 10);
        const after = try std.fmt.parseInt(i32, parts.next().?, 10);

        const entry = try rules.getOrPut(before);
        if (!entry.found_existing) {
            entry.value_ptr.* = std.AutoHashMap(i32, void).init(allocator);
        }
        try entry.value_ptr.*.put(after, {});
    }

    // Parse updates
    var updates: std.ArrayList([]i32) = .{};
    defer {
        for (updates.items) |update| {
            allocator.free(update);
        }
        updates.deinit(allocator);
    }

    var updates_iter = std.mem.tokenizeScalar(u8, updates_section, '\n');
    while (updates_iter.next()) |line| {
        if (line.len == 0) continue;

        var pages: std.ArrayList(i32) = .{};
        defer pages.deinit(allocator);

        var parts = std.mem.tokenizeScalar(u8, line, ',');
        while (parts.next()) |part| {
            const page = try std.fmt.parseInt(i32, part, 10);
            try pages.append(allocator, page);
        }

        const update = try allocator.alloc(i32, pages.items.len);
        @memcpy(update, pages.items);
        try updates.append(allocator, update);
    }

    // Solve Part 1 and Part 2
    var part1_total: i32 = 0;
    var part2_total: i32 = 0;

    for (updates.items) |update| {
        if (isValidOrder(&rules, update)) {
            const middle_idx = update.len / 2;
            part1_total += update[middle_idx];
        } else {
            const fixed = try fixOrder(allocator, &rules, update);
            defer allocator.free(fixed);
            const middle_idx = fixed.len / 2;
            part2_total += fixed[middle_idx];
        }
    }

    std.debug.print("Part 1: {d}\n", .{part1_total});
    std.debug.print("Part 2: {d}\n", .{part2_total});
}

fn isValidOrder(rules: *std.AutoHashMap(i32, std.AutoHashMap(i32, void)), update: []i32) bool {
    // Build position map for this update
    var positions: [100]usize = undefined;
    var present: [100]bool = [_]bool{false} ** 100;

    for (update, 0..) |page, i| {
        const idx: usize = @intCast(page);
        positions[idx] = i;
        present[idx] = true;
    }

    // Check each page in the update
    for (update, 0..) |page, i| {
        // Get pages that must come after this page
        if (rules.get(page)) |after_set| {
            var it = after_set.keyIterator();
            while (it.next()) |after_page| {
                const idx: usize = @intCast(after_page.*);
                if (present[idx]) {
                    // This page must come after current page
                    if (positions[idx] < i) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

fn fixOrder(allocator: std.mem.Allocator, rules: *std.AutoHashMap(i32, std.AutoHashMap(i32, void)), update: []i32) ![]i32 {
    const fixed = try allocator.alloc(i32, update.len);
    @memcpy(fixed, update);

    // Bubble sort using the rules as comparator - multiple passes until stable
    var swapped = true;
    while (swapped) {
        swapped = false;
        var i: usize = 0;
        while (i < fixed.len - 1) : (i += 1) {
            const a = fixed[i];
            const b = fixed[i + 1];

            // Check if b should come before a (rule b|a exists)
            var should_swap = false;
            if (rules.get(b)) |after_set| {
                if (after_set.contains(a)) {
                    should_swap = true;
                }
            }

            if (should_swap) {
                fixed[i] = b;
                fixed[i + 1] = a;
                swapped = true;
            }
        }
    }

    return fixed;
}
