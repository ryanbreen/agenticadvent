const std = @import("std");
const ArrayList = std.ArrayList;

/// Opcodes for the 3-bit computer instruction set
const Opcode = enum(u3) {
    adv = 0, // A = A >> combo
    bxl = 1, // B = B XOR literal
    bst = 2, // B = combo % 8
    jnz = 3, // jump if A != 0
    bxc = 4, // B = B XOR C
    out = 5, // output combo % 8
    bdv = 6, // B = A >> combo
    cdv = 7, // C = A >> combo
};

const Program = struct {
    reg_a: u64,
    reg_b: u64,
    reg_c: u64,
    instructions: []const u8,
};

/// Safely perform a right shift, returning 0 if shift amount exceeds bit width
fn safeShift(value: u64, shift: u64) u64 {
    return if (shift >= 64) 0 else value >> @intCast(shift);
}

fn parseInput(allocator: std.mem.Allocator, content: []const u8) !Program {
    var lines = std.mem.splitScalar(u8, content, '\n');

    // Parse Register A
    const line_a = lines.next() orelse return error.InvalidInput;
    const a_start = std.mem.indexOf(u8, line_a, ": ") orelse return error.InvalidInput;
    const reg_a = try std.fmt.parseInt(u64, line_a[a_start + 2 ..], 10);

    // Parse Register B
    const line_b = lines.next() orelse return error.InvalidInput;
    const b_start = std.mem.indexOf(u8, line_b, ": ") orelse return error.InvalidInput;
    const reg_b = try std.fmt.parseInt(u64, line_b[b_start + 2 ..], 10);

    // Parse Register C
    const line_c = lines.next() orelse return error.InvalidInput;
    const c_start = std.mem.indexOf(u8, line_c, ": ") orelse return error.InvalidInput;
    const reg_c = try std.fmt.parseInt(u64, line_c[c_start + 2 ..], 10);

    // Skip empty line
    _ = lines.next();

    // Parse Program
    const prog_line = lines.next() orelse return error.InvalidInput;
    const prog_start = std.mem.indexOf(u8, prog_line, ": ") orelse return error.InvalidInput;
    const prog_str = prog_line[prog_start + 2 ..];

    var instructions = try ArrayList(u8).initCapacity(allocator, 32);
    var parts = std.mem.splitScalar(u8, prog_str, ',');
    while (parts.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " \t\r\n");
        if (trimmed.len > 0) {
            const val = try std.fmt.parseInt(u8, trimmed, 10);
            try instructions.append(allocator, val);
        }
    }

    return Program{
        .reg_a = reg_a,
        .reg_b = reg_b,
        .reg_c = reg_c,
        .instructions = try instructions.toOwnedSlice(allocator),
    };
}

/// Evaluate a combo operand (0-3 = literal, 4-6 = registers A/B/C)
fn combo(operand: u8, reg_a: u64, reg_b: u64, reg_c: u64) u64 {
    return switch (operand) {
        0...3 => operand,
        4 => reg_a,
        5 => reg_b,
        6 => reg_c,
        else => unreachable,
    };
}

fn runProgram(allocator: std.mem.Allocator, init_a: u64, init_b: u64, init_c: u64, instructions: []const u8) ![]u8 {
    var reg_a = init_a;
    var reg_b = init_b;
    var reg_c = init_c;
    var ip: usize = 0;

    var output = try ArrayList(u8).initCapacity(allocator, 32);

    while (ip < instructions.len) {
        const opcode: Opcode = @enumFromInt(instructions[ip]);
        const operand = instructions[ip + 1];

        switch (opcode) {
            .adv => {
                const shift = combo(operand, reg_a, reg_b, reg_c);
                reg_a = safeShift(reg_a, shift);
            },
            .bxl => {
                reg_b = reg_b ^ operand;
            },
            .bst => {
                reg_b = combo(operand, reg_a, reg_b, reg_c) & 7;
            },
            .jnz => {
                if (reg_a != 0) {
                    ip = operand;
                    continue;
                }
            },
            .bxc => {
                reg_b = reg_b ^ reg_c;
            },
            .out => {
                const val: u8 = @intCast(combo(operand, reg_a, reg_b, reg_c) & 7);
                try output.append(allocator, val);
            },
            .bdv => {
                const shift = combo(operand, reg_a, reg_b, reg_c);
                reg_b = safeShift(reg_a, shift);
            },
            .cdv => {
                const shift = combo(operand, reg_a, reg_b, reg_c);
                reg_c = safeShift(reg_a, shift);
            },
        }
        ip += 2;
    }

    return output.toOwnedSlice(allocator);
}

fn part1(allocator: std.mem.Allocator, program: Program) ![]u8 {
    const output = try runProgram(allocator, program.reg_a, program.reg_b, program.reg_c, program.instructions);
    defer allocator.free(output);

    // Convert to comma-separated string
    var result = try ArrayList(u8).initCapacity(allocator, 64);
    for (output, 0..) |val, i| {
        if (i > 0) try result.append(allocator, ',');
        try result.append(allocator, '0' + val);
    }
    return result.toOwnedSlice(allocator);
}

fn search(allocator: std.mem.Allocator, program: Program, target_idx: usize, current_a: u64) !?u64 {
    // Try all 8 possible 3-bit values for this position
    for (0..8) |bits| {
        const candidate_a = (current_a << 3) | @as(u64, @intCast(bits));

        // A can't be 0 at start (would halt immediately)
        if (candidate_a == 0 and target_idx == program.instructions.len - 1) {
            continue;
        }

        const output = try runProgram(allocator, candidate_a, program.reg_b, program.reg_c, program.instructions);
        defer allocator.free(output);

        // Check if output matches the suffix of the program
        const expected = program.instructions[target_idx..];
        if (std.mem.eql(u8, output, expected)) {
            if (target_idx == 0) {
                return candidate_a;
            }
            if (try search(allocator, program, target_idx - 1, candidate_a)) |result| {
                return result;
            }
        }
    }
    return null;
}

fn part2(allocator: std.mem.Allocator, program: Program) !u64 {
    // Work backwards from the last digit - build A 3 bits at a time
    if (try search(allocator, program, program.instructions.len - 1, 0)) |result| {
        return result;
    }
    return error.NoSolutionFound;
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

    const program = try parseInput(allocator, content);
    defer allocator.free(program.instructions);

    // Part 1
    const p1_result = try part1(allocator, program);
    defer allocator.free(p1_result);

    std.debug.print("Part 1: {s}\n", .{p1_result});

    // Part 2
    const p2_result = try part2(allocator, program);
    std.debug.print("Part 2: {d}\n", .{p2_result});
}
