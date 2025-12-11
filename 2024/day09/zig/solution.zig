const std = @import("std");

const FileInfo = struct {
    start: usize,
    length: usize,
};

fn parseDiskMap(allocator: std.mem.Allocator, filename: []const u8) !std.ArrayList(i32) {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Trim newline/whitespace
    const disk_map = std.mem.trim(u8, content, &std.ascii.whitespace);

    var blocks: std.ArrayList(i32) = .{};
    var file_id: i32 = 0;
    var is_file = true;

    for (disk_map) |ch| {
        const length = ch - '0';
        var i: u8 = 0;
        while (i < length) : (i += 1) {
            if (is_file) {
                try blocks.append(allocator, file_id);
            } else {
                try blocks.append(allocator, -1); // -1 represents free space
            }
        }
        if (is_file) {
            file_id += 1;
        }
        is_file = !is_file;
    }

    return blocks;
}

fn compactBlocks(allocator: std.mem.Allocator, blocks: []const i32) !std.ArrayList(i32) {
    var result: std.ArrayList(i32) = .{};
    try result.appendSlice(allocator, blocks);

    var left: usize = 0;
    var right: usize = result.items.len - 1;

    while (left < right) {
        // Find leftmost free space
        while (left < right and result.items[left] != -1) {
            left += 1;
        }
        // Find rightmost file block
        while (left < right and result.items[right] == -1) {
            right -= 1;
        }

        if (left < right) {
            // Swap
            result.items[left] = result.items[right];
            result.items[right] = -1;
            left += 1;
            right -= 1;
        }
    }

    return result;
}

fn calculateChecksum(blocks: []const i32) u64 {
    var checksum: u64 = 0;
    for (blocks, 0..) |file_id, pos| {
        if (file_id != -1) {
            checksum += @as(u64, pos) * @as(u64, @intCast(file_id));
        }
    }
    return checksum;
}

fn part1(allocator: std.mem.Allocator) !u64 {
    var blocks = try parseDiskMap(allocator, "../input.txt");
    defer blocks.deinit(allocator);

    var compacted = try compactBlocks(allocator, blocks.items);
    defer compacted.deinit(allocator);

    return calculateChecksum(compacted.items);
}

fn part2(allocator: std.mem.Allocator) !u64 {
    var blocks = try parseDiskMap(allocator, "../input.txt");
    defer blocks.deinit(allocator);

    // Find all files: file_id -> (start_pos, length)
    var files = std.AutoHashMap(i32, FileInfo).init(allocator);
    defer files.deinit();

    var i: usize = 0;
    while (i < blocks.items.len) {
        if (blocks.items[i] != -1) {
            const file_id = blocks.items[i];
            const start = i;
            while (i < blocks.items.len and blocks.items[i] == file_id) {
                i += 1;
            }
            try files.put(file_id, FileInfo{ .start = start, .length = i - start });
        } else {
            i += 1;
        }
    }

    // Find max file ID
    var max_file_id: i32 = 0;
    var it = files.keyIterator();
    while (it.next()) |key| {
        if (key.* > max_file_id) {
            max_file_id = key.*;
        }
    }

    // Process files in decreasing order of file ID
    var file_id = max_file_id;
    while (file_id >= 0) : (file_id -= 1) {
        const file_info = files.get(file_id) orelse continue;
        const start = file_info.start;
        const length = file_info.length;

        // Find leftmost span of free space that fits this file
        // Must be to the left of current position
        var free_start: ?usize = null;
        i = 0;
        while (i < start) {
            if (blocks.items[i] == -1) {
                // Count consecutive free blocks
                const span_start = i;
                var span_length: usize = 0;
                while (i < start and blocks.items[i] == -1) {
                    span_length += 1;
                    i += 1;
                }
                if (span_length >= length) {
                    free_start = span_start;
                    break;
                }
            } else {
                i += 1;
            }
        }

        // Move file if we found a suitable span
        if (free_start) |fs| {
            // Clear old position
            var j: usize = start;
            while (j < start + length) : (j += 1) {
                blocks.items[j] = -1;
            }
            // Write to new position
            j = fs;
            while (j < fs + length) : (j += 1) {
                blocks.items[j] = file_id;
            }
            // Update file position in map
            try files.put(file_id, FileInfo{ .start = fs, .length = length });
        }
    }

    return calculateChecksum(blocks.items);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const p1 = try part1(allocator);
    std.debug.print("Part 1: {d}\n", .{p1});

    const p2 = try part2(allocator);
    std.debug.print("Part 2: {d}\n", .{p2});
}
