const std = @import("std");

const Number = struct {
    value: u32,
    row: usize,
    col_start: usize,
    col_end: usize,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Trim trailing whitespace
    const trimmed = std.mem.trimRight(u8, content, " \t\n\r");

    // Count lines
    var line_count: usize = 0;
    var iter = std.mem.splitScalar(u8, trimmed, '\n');
    while (iter.next()) |_| {
        line_count += 1;
    }

    // Allocate grid
    const grid = try allocator.alloc([]const u8, line_count);
    defer allocator.free(grid);

    // Parse lines into grid
    var idx: usize = 0;
    var line_iter = std.mem.splitScalar(u8, trimmed, '\n');
    while (line_iter.next()) |line| {
        grid[idx] = line;
        idx += 1;
    }

    // Part 1
    const part1_result = try part1(allocator, grid);
    std.debug.print("Part 1: {d}\n", .{part1_result});

    // Part 2
    const part2_result = try part2(allocator, grid);
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

fn part1(allocator: std.mem.Allocator, grid: []const []const u8) !u32 {
    // Count numbers first
    var num_count: usize = 0;
    for (grid) |line| {
        var col: usize = 0;
        while (col < line.len) {
            if (std.ascii.isDigit(line[col])) {
                num_count += 1;
                while (col < line.len and std.ascii.isDigit(line[col])) {
                    col += 1;
                }
            } else {
                col += 1;
            }
        }
    }

    // Allocate array for numbers
    const numbers = try allocator.alloc(Number, num_count);
    defer allocator.free(numbers);

    // Find all numbers in the grid
    var num_idx: usize = 0;
    for (grid, 0..) |line, row| {
        var col: usize = 0;
        while (col < line.len) {
            if (std.ascii.isDigit(line[col])) {
                const start_col = col;
                var num_value: u32 = 0;

                while (col < line.len and std.ascii.isDigit(line[col])) {
                    num_value = num_value * 10 + (line[col] - '0');
                    col += 1;
                }

                numbers[num_idx] = Number{
                    .value = num_value,
                    .row = row,
                    .col_start = start_col,
                    .col_end = col - 1,
                };
                num_idx += 1;
            } else {
                col += 1;
            }
        }
    }

    var sum: u32 = 0;

    // Check each number to see if it's adjacent to a symbol
    for (numbers) |num| {
        if (isAdjacentToSymbol(grid, num)) {
            sum += num.value;
        }
    }

    return sum;
}

fn part2(allocator: std.mem.Allocator, grid: []const []const u8) !u32 {
    // Count numbers first
    var num_count: usize = 0;
    for (grid) |line| {
        var col: usize = 0;
        while (col < line.len) {
            if (std.ascii.isDigit(line[col])) {
                num_count += 1;
                while (col < line.len and std.ascii.isDigit(line[col])) {
                    col += 1;
                }
            } else {
                col += 1;
            }
        }
    }

    // Allocate array for numbers
    const numbers = try allocator.alloc(Number, num_count);
    defer allocator.free(numbers);

    // Find all numbers in the grid
    var num_idx: usize = 0;
    for (grid, 0..) |line, row| {
        var col: usize = 0;
        while (col < line.len) {
            if (std.ascii.isDigit(line[col])) {
                const start_col = col;
                var num_value: u32 = 0;

                while (col < line.len and std.ascii.isDigit(line[col])) {
                    num_value = num_value * 10 + (line[col] - '0');
                    col += 1;
                }

                numbers[num_idx] = Number{
                    .value = num_value,
                    .row = row,
                    .col_start = start_col,
                    .col_end = col - 1,
                };
                num_idx += 1;
            } else {
                col += 1;
            }
        }
    }

    var sum: u32 = 0;

    // Find all '*' symbols and check if they're gears
    for (grid, 0..) |line, row| {
        for (line, 0..) |char, col| {
            if (char == '*') {
                // Find all numbers adjacent to this '*'
                var adjacent_count: usize = 0;
                var first_num: u32 = 0;
                var second_num: u32 = 0;

                for (numbers) |num| {
                    if (isNumberAdjacentToPosition(num, row, col)) {
                        if (adjacent_count == 0) {
                            first_num = num.value;
                        } else if (adjacent_count == 1) {
                            second_num = num.value;
                        }
                        adjacent_count += 1;
                    }
                }

                // If exactly 2 numbers are adjacent, it's a gear
                if (adjacent_count == 2) {
                    const gear_ratio = first_num * second_num;
                    sum += gear_ratio;
                }
            }
        }
    }

    return sum;
}

fn isAdjacentToSymbol(grid: []const []const u8, num: Number) bool {
    const height = grid.len;
    const width = grid[0].len;

    // Check all positions around the number
    const row_start = if (num.row > 0) num.row - 1 else 0;
    const row_end = @min(num.row + 1, height - 1);
    const col_start = if (num.col_start > 0) num.col_start - 1 else 0;
    const col_end = @min(num.col_end + 1, width - 1);

    var row = row_start;
    while (row <= row_end) : (row += 1) {
        var col = col_start;
        while (col <= col_end) : (col += 1) {
            const char = grid[row][col];
            // Symbol is anything that's not a digit and not a period
            if (char != '.' and !std.ascii.isDigit(char)) {
                return true;
            }
        }
    }

    return false;
}

fn isNumberAdjacentToPosition(num: Number, pos_row: usize, pos_col: usize) bool {
    // Check if the position is adjacent to any cell of the number
    const row_diff = if (pos_row > num.row) pos_row - num.row else num.row - pos_row;
    if (row_diff > 1) return false;

    // Check if position is within or adjacent to the number's column range
    if (pos_col + 1 < num.col_start) return false;
    if (pos_col > num.col_end + 1) return false;

    return true;
}
