const std = @import("std");

const Board = struct {
    cells: [5][5]u8,
    marked: [5][5]bool,
    won: bool,

    fn init() Board {
        return Board{
            .cells = undefined,
            .marked = [_][5]bool{[_]bool{false} ** 5} ** 5,
            .won = false,
        };
    }

    fn mark(self: *Board, number: u8) void {
        for (0..5) |row| {
            for (0..5) |col| {
                if (self.cells[row][col] == number) {
                    self.marked[row][col] = true;
                }
            }
        }
    }

    fn isWinner(self: *const Board) bool {
        // Check rows
        for (0..5) |row| {
            var all_marked = true;
            for (0..5) |col| {
                if (!self.marked[row][col]) {
                    all_marked = false;
                    break;
                }
            }
            if (all_marked) return true;
        }
        // Check columns
        for (0..5) |col| {
            var all_marked = true;
            for (0..5) |row| {
                if (!self.marked[row][col]) {
                    all_marked = false;
                    break;
                }
            }
            if (all_marked) return true;
        }
        return false;
    }

    fn unmarkedSum(self: *const Board) u32 {
        var sum: u32 = 0;
        for (0..5) |row| {
            for (0..5) |col| {
                if (!self.marked[row][col]) {
                    sum += self.cells[row][col];
                }
            }
        }
        return sum;
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Read input file
    const input = try std.fs.cwd().openFile("../input.txt", .{});
    defer input.close();

    const content = try input.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Parse numbers and boards
    var numbers: [256]u8 = undefined;
    var num_count: usize = 0;
    var boards: [256]Board = undefined;
    var board_count: usize = 0;

    var sections = std.mem.splitSequence(u8, content, "\n\n");

    // Parse numbers from first section
    if (sections.next()) |numbers_section| {
        var num_iter = std.mem.splitScalar(u8, numbers_section, ',');
        while (num_iter.next()) |num_str| {
            const trimmed = std.mem.trim(u8, num_str, " \t\r\n");
            if (trimmed.len > 0) {
                numbers[num_count] = try std.fmt.parseInt(u8, trimmed, 10);
                num_count += 1;
            }
        }
    }

    // Parse boards
    while (sections.next()) |board_section| {
        const trimmed_section = std.mem.trim(u8, board_section, " \t\r\n");
        if (trimmed_section.len == 0) continue;

        var board = Board.init();
        var row: usize = 0;

        var lines = std.mem.splitScalar(u8, trimmed_section, '\n');
        while (lines.next()) |line| {
            const trimmed_line = std.mem.trim(u8, line, " \t\r\n");
            if (trimmed_line.len == 0) continue;

            var col: usize = 0;
            var num_parts = std.mem.tokenizeAny(u8, trimmed_line, " \t");
            while (num_parts.next()) |num_str| {
                board.cells[row][col] = try std.fmt.parseInt(u8, num_str, 10);
                col += 1;
            }
            row += 1;
        }

        boards[board_count] = board;
        board_count += 1;
    }

    // Part 1: Find first winner
    var part1_result: u32 = 0;
    var boards_copy1: [256]Board = undefined;
    @memcpy(boards_copy1[0..board_count], boards[0..board_count]);

    outer1: for (0..num_count) |i| {
        const number = numbers[i];
        for (0..board_count) |b| {
            boards_copy1[b].mark(number);
            if (boards_copy1[b].isWinner()) {
                part1_result = boards_copy1[b].unmarkedSum() * number;
                break :outer1;
            }
        }
    }

    // Part 2: Find last winner
    var part2_result: u32 = 0;
    var boards_copy2: [256]Board = undefined;
    @memcpy(boards_copy2[0..board_count], boards[0..board_count]);

    for (0..num_count) |i| {
        const number = numbers[i];
        for (0..board_count) |b| {
            if (boards_copy2[b].won) continue;
            boards_copy2[b].mark(number);
            if (boards_copy2[b].isWinner()) {
                boards_copy2[b].won = true;
                part2_result = boards_copy2[b].unmarkedSum() * number;
            }
        }
    }

    std.debug.print("Part 1: {d}\n", .{part1_result});
    std.debug.print("Part 2: {d}\n", .{part2_result});
}
