const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);

    // Trim trailing whitespace
    const input = std.mem.trimRight(u8, content, &std.ascii.whitespace);

    // Split into lines
    var lines: std.ArrayList([]const u8) = .empty;

    var line_iter = std.mem.splitScalar(u8, input, '\n');
    while (line_iter.next()) |line| {
        try lines.append(allocator, line);
    }

    const part1_result = try part1(allocator, lines.items);
    const part2_result = try part2(allocator, lines.items);

    std.debug.print("Part 1: {d}\n", .{part1_result});
    std.debug.print("Part 2: {d}\n", .{part2_result});
}

const Problem = struct {
    numbers: std.ArrayList(i64),
    operator: u8,

    fn deinit(self: *Problem, allocator: std.mem.Allocator) void {
        self.numbers.deinit(allocator);
    }
};

fn part1(allocator: std.mem.Allocator, lines: []const []const u8) !i64 {
    var problems = try parseProblems(allocator, lines);
    defer {
        for (problems.items) |*problem| {
            problem.deinit(allocator);
        }
        problems.deinit(allocator);
    }

    var total: i64 = 0;
    for (problems.items) |problem| {
        const result = solveProblem(problem.numbers.items, problem.operator);
        total += result;
    }

    return total;
}

fn part2(allocator: std.mem.Allocator, lines: []const []const u8) !i64 {
    var problems = try parseProblemsPart2(allocator, lines);
    defer {
        for (problems.items) |*problem| {
            problem.deinit(allocator);
        }
        problems.deinit(allocator);
    }

    var total: i64 = 0;
    for (problems.items) |problem| {
        const result = solveProblem(problem.numbers.items, problem.operator);
        total += result;
    }

    return total;
}

fn parseProblems(allocator: std.mem.Allocator, lines: []const []const u8) !std.ArrayList(Problem) {
    var problems: std.ArrayList(Problem) = .empty;

    if (lines.len == 0) return problems;

    // Find the operator row (last non-empty row with only +, *, and spaces)
    var op_row_idx: ?usize = null;
    var i = lines.len;
    while (i > 0) {
        i -= 1;
        const line = lines[i];
        if (line.len == 0) continue;

        var valid = true;
        var has_op = false;
        for (line) |c| {
            if (c != '+' and c != '*' and c != ' ') {
                valid = false;
                break;
            }
            if (c == '+' or c == '*') {
                has_op = true;
            }
        }
        if (valid and has_op) {
            op_row_idx = i;
            break;
        }
    }

    if (op_row_idx == null) return problems;

    const op_row = lines[op_row_idx.?];
    const number_rows = lines[0..op_row_idx.?];

    // Find max width
    var max_width: usize = 0;
    for (lines) |line| {
        if (line.len > max_width) max_width = line.len;
    }

    // Pad all rows to the same width
    var padded_number_rows: std.ArrayList([]u8) = .empty;
    defer {
        for (padded_number_rows.items) |row| {
            allocator.free(row);
        }
        padded_number_rows.deinit(allocator);
    }

    for (number_rows) |row| {
        const padded = try allocator.alloc(u8, max_width);
        @memcpy(padded[0..row.len], row);
        @memset(padded[row.len..], ' ');
        try padded_number_rows.append(allocator, padded);
    }

    const padded_op_row = try allocator.alloc(u8, max_width);
    defer allocator.free(padded_op_row);
    @memcpy(padded_op_row[0..op_row.len], op_row);
    @memset(padded_op_row[op_row.len..], ' ');

    // Find problem boundaries by looking for columns that are all spaces
    var col: usize = 0;

    while (col < max_width) {
        // Skip separator columns (all spaces)
        while (col < max_width) {
            var is_sep = true;
            for (padded_number_rows.items) |row| {
                if (row[col] != ' ') {
                    is_sep = false;
                    break;
                }
            }
            if (is_sep and padded_op_row[col] == ' ') {
                col += 1;
            } else {
                break;
            }
        }

        if (col >= max_width) break;

        // Find the end of this problem
        const start_col = col;
        while (col < max_width) {
            var is_sep = true;
            for (padded_number_rows.items) |row| {
                if (row[col] != ' ') {
                    is_sep = false;
                    break;
                }
            }
            if (is_sep and padded_op_row[col] == ' ') {
                break;
            }
            col += 1;
        }

        const end_col = col;

        // Extract numbers and operator for this problem
        var numbers: std.ArrayList(i64) = .empty;
        for (padded_number_rows.items) |row| {
            const num_str = std.mem.trim(u8, row[start_col..end_col], &std.ascii.whitespace);
            if (num_str.len > 0) {
                const num = try std.fmt.parseInt(i64, num_str, 10);
                try numbers.append(allocator, num);
            }
        }

        const op_str = std.mem.trim(u8, padded_op_row[start_col..end_col], &std.ascii.whitespace);
        if (op_str.len > 0 and numbers.items.len > 0) {
            try problems.append(allocator, Problem{
                .numbers = numbers,
                .operator = op_str[0],
            });
        } else {
            numbers.deinit(allocator);
        }
    }

    return problems;
}

fn parseProblemsPart2(allocator: std.mem.Allocator, lines: []const []const u8) !std.ArrayList(Problem) {
    var problems: std.ArrayList(Problem) = .empty;

    if (lines.len == 0) return problems;

    // Find the operator row (last non-empty row with only +, *, and spaces)
    var op_row_idx: ?usize = null;
    var i = lines.len;
    while (i > 0) {
        i -= 1;
        const line = lines[i];
        if (line.len == 0) continue;

        var valid = true;
        var has_op = false;
        for (line) |c| {
            if (c != '+' and c != '*' and c != ' ') {
                valid = false;
                break;
            }
            if (c == '+' or c == '*') {
                has_op = true;
            }
        }
        if (valid and has_op) {
            op_row_idx = i;
            break;
        }
    }

    if (op_row_idx == null) return problems;

    const op_row = lines[op_row_idx.?];
    const number_rows = lines[0..op_row_idx.?];

    // Find max width
    var max_width: usize = 0;
    for (lines) |line| {
        if (line.len > max_width) max_width = line.len;
    }

    // Pad all rows to the same width
    var padded_number_rows: std.ArrayList([]u8) = .empty;
    defer {
        for (padded_number_rows.items) |row| {
            allocator.free(row);
        }
        padded_number_rows.deinit(allocator);
    }

    for (number_rows) |row| {
        const padded = try allocator.alloc(u8, max_width);
        @memcpy(padded[0..row.len], row);
        @memset(padded[row.len..], ' ');
        try padded_number_rows.append(allocator, padded);
    }

    const padded_op_row = try allocator.alloc(u8, max_width);
    defer allocator.free(padded_op_row);
    @memcpy(padded_op_row[0..op_row.len], op_row);
    @memset(padded_op_row[op_row.len..], ' ');

    // Find problem boundaries by looking for columns that are all spaces
    var col: usize = 0;

    while (col < max_width) {
        // Skip separator columns (all spaces)
        while (col < max_width) {
            var is_sep = true;
            for (padded_number_rows.items) |row| {
                if (row[col] != ' ') {
                    is_sep = false;
                    break;
                }
            }
            if (is_sep and padded_op_row[col] == ' ') {
                col += 1;
            } else {
                break;
            }
        }

        if (col >= max_width) break;

        // Find the end of this problem
        const start_col = col;
        while (col < max_width) {
            var is_sep = true;
            for (padded_number_rows.items) |row| {
                if (row[col] != ' ') {
                    is_sep = false;
                    break;
                }
            }
            if (is_sep and padded_op_row[col] == ' ') {
                break;
            }
            col += 1;
        }

        const end_col = col;

        // For Part 2: Read columns right-to-left, each column forms a number
        // reading top-to-bottom as most-to-least significant digit
        var numbers: std.ArrayList(i64) = .empty;
        var c = end_col;
        while (c > start_col) {
            c -= 1;
            var digits: std.ArrayList(u8) = .empty;
            defer digits.deinit(allocator);

            for (padded_number_rows.items) |row| {
                const ch = row[c];
                if (std.ascii.isDigit(ch)) {
                    try digits.append(allocator, ch);
                }
            }

            if (digits.items.len > 0) {
                // Join digits to form number (top=most significant, bottom=least)
                const num = try std.fmt.parseInt(i64, digits.items, 10);
                try numbers.append(allocator, num);
            }
        }

        const op_str = std.mem.trim(u8, padded_op_row[start_col..end_col], &std.ascii.whitespace);
        if (op_str.len > 0 and numbers.items.len > 0) {
            try problems.append(allocator, Problem{
                .numbers = numbers,
                .operator = op_str[0],
            });
        } else {
            numbers.deinit(allocator);
        }
    }

    return problems;
}

fn solveProblem(numbers: []const i64, operator: u8) i64 {
    if (operator == '+') {
        var sum: i64 = 0;
        for (numbers) |n| {
            sum += n;
        }
        return sum;
    } else if (operator == '*') {
        var product: i64 = 1;
        for (numbers) |n| {
            product *= n;
        }
        return product;
    }
    return 0;
}
