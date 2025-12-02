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

    const part1_result = try part1(input);
    const part2_result = try part2(input);

    std.debug.print("Part 1: {d}\n", .{part1_result});
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn part1(input: []const u8) !i64 {
    var position: i32 = 50; // Starting position
    var zero_count: i64 = 0;

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue; // Skip empty lines

        const direction = line[0];
        const distance = try std.fmt.parseInt(i32, line[1..], 10);

        if (direction == 'L') {
            position = @mod(position - distance, 100);
        } else { // direction == 'R'
            position = @mod(position + distance, 100);
        }

        if (position == 0) {
            zero_count += 1;
        }
    }

    return zero_count;
}

fn part2(input: []const u8) !i64 {
    var position: i32 = 50; // Starting position
    var zero_count: i64 = 0;

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue; // Skip empty lines

        const direction = line[0];
        const distance = try std.fmt.parseInt(i32, line[1..], 10);

        if (direction == 'L') {
            // Moving left (toward lower numbers)
            if (position > 0 and distance >= position) {
                zero_count += 1 + @divFloor(distance - position, 100);
            } else if (position == 0 and distance >= 100) {
                zero_count += @divFloor(distance, 100);
            }
        } else { // direction == 'R'
            // Moving right (toward higher numbers)
            if (position > 0) {
                const steps_to_zero: i32 = 100 - position;
                if (distance >= steps_to_zero) {
                    zero_count += 1 + @divFloor(distance - steps_to_zero, 100);
                }
            } else { // position == 0
                if (distance >= 100) {
                    zero_count += @divFloor(distance, 100);
                }
            }
        }

        // Update position
        if (direction == 'L') {
            position = @mod(position - distance, 100);
        } else {
            position = @mod(position + distance, 100);
        }
    }

    return zero_count;
}
