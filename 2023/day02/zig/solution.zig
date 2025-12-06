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

    // Trim trailing whitespace
    const input = std.mem.trim(u8, buffer, &std.ascii.whitespace);

    // Parse and solve
    const part1_result = try part1(allocator, input);
    const part2_result = try part2(allocator, input);

    std.debug.print("Part 1: {d}\n", .{part1_result});
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn part1(_: std.mem.Allocator, input: []const u8) !u32 {
    var sum: u32 = 0;
    var lines = std.mem.splitSequence(u8, input, "\n");

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse game ID
        const colon_pos = std.mem.indexOf(u8, line, ":") orelse continue;
        const game_id_str = line[5..colon_pos]; // Skip "Game "
        const game_id = try std.fmt.parseInt(u32, game_id_str, 10);

        // Parse reveals (semicolon-separated)
        const reveals_str = line[colon_pos + 2 ..]; // Skip ": "
        var reveals = std.mem.splitSequence(u8, reveals_str, ";");

        var possible = true;
        while (reveals.next()) |reveal| {
            // Parse each cube count in this reveal
            var cubes = std.mem.splitSequence(u8, std.mem.trim(u8, reveal, " "), ",");

            while (cubes.next()) |cube| {
                const trimmed = std.mem.trim(u8, cube, " ");
                var parts = std.mem.splitSequence(u8, trimmed, " ");

                const count_str = parts.next() orelse continue;
                const color = parts.next() orelse continue;

                const count = try std.fmt.parseInt(u32, count_str, 10);

                // Check against limits: 12 red, 13 green, 14 blue
                if (std.mem.eql(u8, color, "red") and count > 12) {
                    possible = false;
                } else if (std.mem.eql(u8, color, "green") and count > 13) {
                    possible = false;
                } else if (std.mem.eql(u8, color, "blue") and count > 14) {
                    possible = false;
                }
            }
        }

        if (possible) {
            sum += game_id;
        }
    }

    return sum;
}

fn part2(_: std.mem.Allocator, input: []const u8) !u32 {
    var sum: u32 = 0;
    var lines = std.mem.splitSequence(u8, input, "\n");

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        // Parse game
        const colon_pos = std.mem.indexOf(u8, line, ":") orelse continue;
        const reveals_str = line[colon_pos + 2 ..]; // Skip ": "

        // Track minimum cubes needed for each color
        var min_red: u32 = 0;
        var min_green: u32 = 0;
        var min_blue: u32 = 0;

        // Parse reveals (semicolon-separated)
        var reveals = std.mem.splitSequence(u8, reveals_str, ";");

        while (reveals.next()) |reveal| {
            // Parse each cube count in this reveal
            var cubes = std.mem.splitSequence(u8, std.mem.trim(u8, reveal, " "), ",");

            while (cubes.next()) |cube| {
                const trimmed = std.mem.trim(u8, cube, " ");
                var parts = std.mem.splitSequence(u8, trimmed, " ");

                const count_str = parts.next() orelse continue;
                const color = parts.next() orelse continue;

                const count = try std.fmt.parseInt(u32, count_str, 10);

                // Update minimum needed for each color
                if (std.mem.eql(u8, color, "red")) {
                    min_red = @max(min_red, count);
                } else if (std.mem.eql(u8, color, "green")) {
                    min_green = @max(min_green, count);
                } else if (std.mem.eql(u8, color, "blue")) {
                    min_blue = @max(min_blue, count);
                }
            }
        }

        // Calculate power (product of minimums)
        const power = min_red * min_green * min_blue;
        sum += power;
    }

    return sum;
}
