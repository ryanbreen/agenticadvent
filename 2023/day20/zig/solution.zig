const std = @import("std");
const Allocator = std.mem.Allocator;

const ModuleType = enum {
    broadcaster,
    flip_flop,
    conjunction,
    output, // sink module that doesn't exist in input
};

const Module = struct {
    name: []const u8,
    module_type: ModuleType,
    destinations: []usize, // indices into modules array
    // For flip-flops: state (on/off)
    state: bool = false,
    // For conjunctions: memory of last pulse from each input
    memory: std.AutoHashMapUnmanaged(usize, bool) = .{}, // input index -> last pulse (true=high)
    inputs: []usize, // indices of input modules
};

const Pulse = struct {
    source: usize,
    dest: usize,
    high: bool,
};

fn gcd(a: u64, b: u64) u64 {
    var x = a;
    var y = b;
    while (y != 0) {
        const t = y;
        y = x % y;
        x = t;
    }
    return x;
}

fn lcm(a: u64, b: u64) u64 {
    return (a / gcd(a, b)) * b;
}

const Simulator = struct {
    allocator: Allocator,
    modules: std.ArrayListUnmanaged(Module),
    name_to_index: std.StringHashMapUnmanaged(usize),
    broadcaster_index: usize = 0,
    rx_input_index: ?usize = null,

    fn init() Simulator {
        return Simulator{
            .modules = .{},
            .name_to_index = .{},
            .allocator = undefined,
        };
    }

    fn deinit(self: *Simulator) void {
        for (self.modules.items) |*module| {
            self.allocator.free(module.destinations);
            module.memory.deinit(self.allocator);
            self.allocator.free(module.inputs);
        }
        self.modules.deinit(self.allocator);
        self.name_to_index.deinit(self.allocator);
    }

    fn getOrCreateModule(self: *Simulator, name: []const u8) !usize {
        if (self.name_to_index.get(name)) |index| {
            return index;
        }

        const index = self.modules.items.len;
        try self.name_to_index.put(self.allocator, name, index);

        try self.modules.append(self.allocator, Module{
            .name = name,
            .module_type = .output, // default, will be set properly when parsed
            .destinations = &[_]usize{},
            .inputs = &[_]usize{},
        });

        return index;
    }

    fn parse(self: *Simulator, allocator: Allocator, input: []const u8) !void {
        self.allocator = allocator;

        // Temporary storage for destinations and inputs during parsing
        var temp_destinations = std.ArrayListUnmanaged(std.ArrayListUnmanaged(usize)){};
        defer temp_destinations.deinit(allocator);

        var temp_inputs = std.ArrayListUnmanaged(std.ArrayListUnmanaged(usize)){};
        defer temp_inputs.deinit(allocator);

        var lines = std.mem.splitScalar(u8, input, '\n');

        // First pass: create all modules
        while (lines.next()) |line| {
            if (line.len == 0) continue;

            var parts = std.mem.splitSequence(u8, line, " -> ");
            const name_part = parts.next().?;

            var name: []const u8 = undefined;
            var module_type: ModuleType = undefined;

            if (std.mem.eql(u8, name_part, "broadcaster")) {
                name = "broadcaster";
                module_type = .broadcaster;
            } else if (name_part[0] == '%') {
                name = name_part[1..];
                module_type = .flip_flop;
            } else if (name_part[0] == '&') {
                name = name_part[1..];
                module_type = .conjunction;
            } else {
                continue;
            }

            const index = try self.getOrCreateModule(name);
            self.modules.items[index].module_type = module_type;

            if (module_type == .broadcaster) {
                self.broadcaster_index = index;
            }
        }

        // Ensure temp arrays are sized
        while (temp_destinations.items.len < self.modules.items.len) {
            try temp_destinations.append(allocator, .{});
        }
        while (temp_inputs.items.len < self.modules.items.len) {
            try temp_inputs.append(allocator, .{});
        }

        // Second pass: set up destinations and inputs
        lines = std.mem.splitScalar(u8, input, '\n');
        while (lines.next()) |line| {
            if (line.len == 0) continue;

            var parts = std.mem.splitSequence(u8, line, " -> ");
            const name_part = parts.next().?;
            const dest_part = parts.next().?;

            var name: []const u8 = undefined;
            if (std.mem.eql(u8, name_part, "broadcaster")) {
                name = "broadcaster";
            } else if (name_part[0] == '%' or name_part[0] == '&') {
                name = name_part[1..];
            } else {
                continue;
            }

            const src_index = self.name_to_index.get(name).?;

            var dests = std.mem.splitSequence(u8, dest_part, ", ");
            while (dests.next()) |dest_name| {
                const dest_index = try self.getOrCreateModule(dest_name);

                // Grow temp arrays if needed
                while (temp_destinations.items.len <= dest_index) {
                    try temp_destinations.append(allocator, .{});
                }
                while (temp_inputs.items.len <= dest_index) {
                    try temp_inputs.append(allocator, .{});
                }

                try temp_destinations.items[src_index].append(allocator, dest_index);
                try temp_inputs.items[dest_index].append(allocator, src_index);

                // Check if this destination is rx
                if (std.mem.eql(u8, dest_name, "rx")) {
                    self.rx_input_index = src_index;
                }
            }
        }

        // Convert temp arrays to slices and initialize conjunction memory
        for (self.modules.items, 0..) |*module, i| {
            if (i < temp_destinations.items.len) {
                module.destinations = try temp_destinations.items[i].toOwnedSlice(allocator);
            }
            if (i < temp_inputs.items.len) {
                module.inputs = try temp_inputs.items[i].toOwnedSlice(allocator);
            }

            if (module.module_type == .conjunction) {
                for (module.inputs) |input_index| {
                    try module.memory.put(allocator, input_index, false);
                }
            }
        }

        // Clean up any remaining temp arrays
        for (temp_destinations.items) |*arr| {
            arr.deinit(allocator);
        }
        for (temp_inputs.items) |*arr| {
            arr.deinit(allocator);
        }
    }

    fn reset(self: *Simulator) void {
        for (self.modules.items) |*module| {
            module.state = false;
            if (module.module_type == .conjunction) {
                var it = module.memory.iterator();
                while (it.next()) |entry| {
                    entry.value_ptr.* = false;
                }
            }
        }
    }

    fn simulateButtonPress(
        self: *Simulator,
        watch_nodes: ?std.AutoHashMapUnmanaged(usize, bool),
    ) !struct { low: u64, high: u64, high_senders: std.AutoHashMapUnmanaged(usize, bool) } {
        var low_count: u64 = 0;
        var high_count: u64 = 0;
        var high_senders: std.AutoHashMapUnmanaged(usize, bool) = .{};

        var queue = std.ArrayListUnmanaged(Pulse){};
        defer queue.deinit(self.allocator);

        // Button sends low pulse to broadcaster
        // Use a special index for button (max usize)
        const button_index: usize = std.math.maxInt(usize);
        try queue.append(self.allocator, Pulse{
            .source = button_index,
            .dest = self.broadcaster_index,
            .high = false,
        });

        var head: usize = 0;
        while (head < queue.items.len) {
            const pulse = queue.items[head];
            head += 1;

            if (pulse.high) {
                high_count += 1;
            } else {
                low_count += 1;
            }

            // Track if watched nodes send high pulses
            if (watch_nodes) |wn| {
                if (wn.contains(pulse.source) and pulse.high) {
                    try high_senders.put(self.allocator, pulse.source, true);
                }
            }

            if (pulse.dest >= self.modules.items.len) continue;

            var module = &self.modules.items[pulse.dest];

            switch (module.module_type) {
                .broadcaster => {
                    for (module.destinations) |dest| {
                        try queue.append(self.allocator, Pulse{
                            .source = pulse.dest,
                            .dest = dest,
                            .high = pulse.high,
                        });
                    }
                },
                .flip_flop => {
                    if (!pulse.high) {
                        module.state = !module.state;
                        for (module.destinations) |dest| {
                            try queue.append(self.allocator, Pulse{
                                .source = pulse.dest,
                                .dest = dest,
                                .high = module.state,
                            });
                        }
                    }
                },
                .conjunction => {
                    module.memory.put(self.allocator, pulse.source, pulse.high) catch {};

                    // Send low if all inputs are high, otherwise send high
                    var all_high = true;
                    var it = module.memory.iterator();
                    while (it.next()) |entry| {
                        if (!entry.value_ptr.*) {
                            all_high = false;
                            break;
                        }
                    }

                    const output = !all_high;
                    for (module.destinations) |dest| {
                        try queue.append(self.allocator, Pulse{
                            .source = pulse.dest,
                            .dest = dest,
                            .high = output,
                        });
                    }
                },
                .output => {
                    // Sink module, does nothing
                },
            }
        }

        return .{ .low = low_count, .high = high_count, .high_senders = high_senders };
    }

    fn part1(self: *Simulator) !u64 {
        self.reset();

        var total_low: u64 = 0;
        var total_high: u64 = 0;

        for (0..1000) |_| {
            const result = try self.simulateButtonPress(null);
            total_low += result.low;
            total_high += result.high;
        }

        return total_low * total_high;
    }

    fn part2(self: *Simulator) !u64 {
        self.reset();

        // rx_input_index is the conjunction that feeds rx
        const rx_input = self.rx_input_index orelse return 0;

        // Get all modules that feed into rx_input
        var watch_nodes: std.AutoHashMapUnmanaged(usize, bool) = .{};
        defer watch_nodes.deinit(self.allocator);

        for (self.modules.items[rx_input].inputs) |input_index| {
            try watch_nodes.put(self.allocator, input_index, true);
        }

        var cycle_lengths: std.AutoHashMapUnmanaged(usize, u64) = .{};
        defer cycle_lengths.deinit(self.allocator);

        var button_press: u64 = 0;
        while (cycle_lengths.count() < watch_nodes.count()) {
            button_press += 1;
            var result = try self.simulateButtonPress(watch_nodes);
            defer result.high_senders.deinit(self.allocator);

            var it = result.high_senders.iterator();
            while (it.next()) |entry| {
                if (!cycle_lengths.contains(entry.key_ptr.*)) {
                    try cycle_lengths.put(self.allocator, entry.key_ptr.*, button_press);
                }
            }
        }

        // Compute LCM of all cycle lengths
        var final_result: u64 = 1;
        var it = cycle_lengths.iterator();
        while (it.next()) |entry| {
            final_result = lcm(final_result, entry.value_ptr.*);
        }

        return final_result;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input file
    const file = try std.fs.cwd().openFile("../input.txt", .{});
    defer file.close();
    const input = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    var sim = Simulator.init();
    defer sim.deinit();

    try sim.parse(allocator, input);

    const p1 = try sim.part1();
    std.debug.print("Part 1: {}\n", .{p1});

    // Re-parse for part 2 (fresh state)
    var sim2 = Simulator.init();
    defer sim2.deinit();
    try sim2.parse(allocator, input);

    const p2 = try sim2.part2();
    std.debug.print("Part 2: {}\n", .{p2});
}
