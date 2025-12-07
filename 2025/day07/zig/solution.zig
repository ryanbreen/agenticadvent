const std = @import("std");

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
    const input = std.mem.trim(u8, buffer, &std.ascii.whitespace);

    const part1_result = try part1(allocator, input);
    const part2_result = try part2(allocator, input);

    std.debug.print("Part 1: {d}\n", .{part1_result});
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn part1(allocator: std.mem.Allocator, input: []const u8) !u64 {
    // Count lines
    var line_count: usize = 0;
    var line_iter1 = std.mem.splitScalar(u8, input, '\n');
    while (line_iter1.next()) |_| {
        line_count += 1;
    }

    if (line_count == 0) return 0;

    // Allocate array for lines
    const lines = try allocator.alloc([]const u8, line_count);
    defer allocator.free(lines);

    // Parse lines
    var idx: usize = 0;
    var line_iter2 = std.mem.splitScalar(u8, input, '\n');
    while (line_iter2.next()) |line| {
        lines[idx] = line;
        idx += 1;
    }

    const rows = lines.len;
    const cols = lines[0].len;

    // Find starting position S
    var start_col: ?usize = null;
    for (0..cols) |col| {
        if (lines[0][col] == 'S') {
            start_col = col;
            break;
        }
    }

    if (start_col == null) return 0;

    // Track active beam columns at each row using a set
    var active_beams = std.AutoHashMap(usize, void).init(allocator);
    defer active_beams.deinit();
    try active_beams.put(start_col.?, {});

    var split_count: u64 = 0;

    // Process row by row starting from row 1 (below S)
    for (1..rows) |row| {
        var new_beams = std.AutoHashMap(usize, void).init(allocator);
        defer new_beams.deinit();

        var beam_iter = active_beams.keyIterator();
        while (beam_iter.next()) |col_ptr| {
            const col = col_ptr.*;
            if (col < cols) {
                const cell = lines[row][col];
                if (cell == '^') {
                    // Beam hits splitter - count it and emit left/right
                    split_count += 1;
                    // Left beam goes to col-1, right beam goes to col+1
                    if (col > 0) {
                        try new_beams.put(col - 1, {});
                    }
                    if (col + 1 < cols) {
                        try new_beams.put(col + 1, {});
                    }
                } else if (cell == '.') {
                    // Beam continues straight down
                    try new_beams.put(col, {});
                } else {
                    // Other characters - beam continues
                    try new_beams.put(col, {});
                }
            }
        }

        active_beams.deinit();
        active_beams = new_beams.move();

        // If no more beams, stop
        if (active_beams.count() == 0) {
            break;
        }
    }

    return split_count;
}

fn part2(allocator: std.mem.Allocator, input: []const u8) !u128 {
    // Count lines
    var line_count: usize = 0;
    var line_iter1 = std.mem.splitScalar(u8, input, '\n');
    while (line_iter1.next()) |_| {
        line_count += 1;
    }

    if (line_count == 0) return 0;

    // Allocate array for lines
    const lines = try allocator.alloc([]const u8, line_count);
    defer allocator.free(lines);

    // Parse lines
    var idx: usize = 0;
    var line_iter2 = std.mem.splitScalar(u8, input, '\n');
    while (line_iter2.next()) |line| {
        lines[idx] = line;
        idx += 1;
    }

    const rows = lines.len;
    const cols = lines[0].len;

    // Find starting position S
    var start_col: ?usize = null;
    for (0..cols) |col| {
        if (lines[0][col] == 'S') {
            start_col = col;
            break;
        }
    }

    if (start_col == null) return 0;

    // Track number of timelines at each column position
    // Use a map: col -> count of timelines at that position
    var timelines = std.AutoHashMap(usize, u128).init(allocator);
    defer timelines.deinit();
    try timelines.put(start_col.?, 1);

    // Process row by row starting from row 1 (below S)
    for (1..rows) |row| {
        var new_timelines = std.AutoHashMap(usize, u128).init(allocator);
        defer new_timelines.deinit();

        var timeline_iter = timelines.iterator();
        while (timeline_iter.next()) |entry| {
            const col = entry.key_ptr.*;
            const count = entry.value_ptr.*;

            if (col < cols) {
                const cell = lines[row][col];
                if (cell == '^') {
                    // Each timeline splits into 2 (left and right)
                    if (col > 0) {
                        const left_col = col - 1;
                        const existing = new_timelines.get(left_col) orelse 0;
                        try new_timelines.put(left_col, existing + count);
                    }
                    if (col + 1 < cols) {
                        const right_col = col + 1;
                        const existing = new_timelines.get(right_col) orelse 0;
                        try new_timelines.put(right_col, existing + count);
                    }
                } else if (cell == '.') {
                    // Timelines continue straight down
                    const existing = new_timelines.get(col) orelse 0;
                    try new_timelines.put(col, existing + count);
                } else {
                    // Other characters - timelines continue
                    const existing = new_timelines.get(col) orelse 0;
                    try new_timelines.put(col, existing + count);
                }
            }
        }

        timelines.deinit();
        timelines = new_timelines.move();

        // If no more timelines, stop
        if (timelines.count() == 0) {
            break;
        }
    }

    // Total number of timelines
    var total: u128 = 0;
    var value_iter = timelines.valueIterator();
    while (value_iter.next()) |count_ptr| {
        total += count_ptr.*;
    }

    return total;
}
