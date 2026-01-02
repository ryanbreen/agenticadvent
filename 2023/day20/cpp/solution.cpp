// Day 20: Pulse Propagation - Module communication simulation
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <queue>
#include <numeric>

enum class ModuleType { BROADCASTER, FLIP_FLOP, CONJUNCTION };

struct Module {
    ModuleType type;
    std::vector<std::string> destinations;
    bool state = false;  // For flip-flops
    std::unordered_map<std::string, bool> memory;  // For conjunctions
};

struct Pulse {
    std::string source;
    std::string dest;
    bool high;
};

std::unordered_map<std::string, Module> parse_input(const std::string& filename) {
    std::unordered_map<std::string, Module> modules;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        size_t arrow_pos = line.find(" -> ");
        std::string name_part = line.substr(0, arrow_pos);
        std::string dest_part = line.substr(arrow_pos + 4);

        Module module;
        std::string name;

        // Parse destinations
        std::stringstream ss(dest_part);
        std::string dest;
        while (std::getline(ss, dest, ',')) {
            // Trim whitespace
            size_t start = dest.find_first_not_of(" ");
            size_t end = dest.find_last_not_of(" ");
            if (start != std::string::npos) {
                module.destinations.push_back(dest.substr(start, end - start + 1));
            }
        }

        if (name_part == "broadcaster") {
            name = "broadcaster";
            module.type = ModuleType::BROADCASTER;
        } else if (name_part[0] == '%') {
            name = name_part.substr(1);
            module.type = ModuleType::FLIP_FLOP;
            module.state = false;
        } else if (name_part[0] == '&') {
            name = name_part.substr(1);
            module.type = ModuleType::CONJUNCTION;
        }

        modules[name] = module;
    }

    // Initialize conjunction memory for all inputs
    for (auto& [name, module] : modules) {
        for (const auto& dest : module.destinations) {
            if (modules.count(dest) && modules[dest].type == ModuleType::CONJUNCTION) {
                modules[dest].memory[name] = false;
            }
        }
    }

    return modules;
}

void reset_state(std::unordered_map<std::string, Module>& modules) {
    for (auto& [name, module] : modules) {
        if (module.type == ModuleType::FLIP_FLOP) {
            module.state = false;
        } else if (module.type == ModuleType::CONJUNCTION) {
            for (auto& [key, val] : module.memory) {
                val = false;
            }
        }
    }
}

struct SimResult {
    long long low_count;
    long long high_count;
    std::unordered_set<std::string> high_senders;
};

SimResult simulate_button_press(std::unordered_map<std::string, Module>& modules,
                                 const std::unordered_set<std::string>& watch_nodes = {}) {
    long long low_count = 0;
    long long high_count = 0;
    std::unordered_set<std::string> high_senders;

    std::queue<Pulse> q;
    q.push({"button", "broadcaster", false});

    while (!q.empty()) {
        Pulse pulse = q.front();
        q.pop();

        if (pulse.high) {
            high_count++;
        } else {
            low_count++;
        }

        // Track if watched nodes send high pulses
        if (!watch_nodes.empty() && watch_nodes.count(pulse.source) && pulse.high) {
            high_senders.insert(pulse.source);
        }

        if (!modules.count(pulse.dest)) {
            continue;
        }

        Module& module = modules[pulse.dest];

        if (module.type == ModuleType::BROADCASTER) {
            for (const auto& next_dest : module.destinations) {
                q.push({pulse.dest, next_dest, pulse.high});
            }
        } else if (module.type == ModuleType::FLIP_FLOP) {
            if (!pulse.high) {  // Only react to low pulses
                module.state = !module.state;
                for (const auto& next_dest : module.destinations) {
                    q.push({pulse.dest, next_dest, module.state});
                }
            }
        } else if (module.type == ModuleType::CONJUNCTION) {
            module.memory[pulse.source] = pulse.high;
            // Send low if all inputs are high, otherwise send high
            bool all_high = true;
            for (const auto& [key, val] : module.memory) {
                if (!val) {
                    all_high = false;
                    break;
                }
            }
            bool output = !all_high;
            for (const auto& next_dest : module.destinations) {
                q.push({pulse.dest, next_dest, output});
            }
        }
    }

    return {low_count, high_count, high_senders};
}

long long part1(std::unordered_map<std::string, Module> modules) {
    reset_state(modules);

    long long total_low = 0;
    long long total_high = 0;

    for (int i = 0; i < 1000; i++) {
        auto result = simulate_button_press(modules);
        total_low += result.low_count;
        total_high += result.high_count;
    }

    return total_low * total_high;
}

long long part2(std::unordered_map<std::string, Module> modules) {
    reset_state(modules);

    // Find the module that feeds into rx
    std::string rx_input;
    for (const auto& [name, module] : modules) {
        for (const auto& dest : module.destinations) {
            if (dest == "rx") {
                rx_input = name;
                break;
            }
        }
        if (!rx_input.empty()) break;
    }

    if (rx_input.empty()) {
        return 0;
    }

    // Find all modules that feed into rx_input
    std::unordered_set<std::string> watch_nodes;
    for (const auto& [key, val] : modules[rx_input].memory) {
        watch_nodes.insert(key);
    }

    std::unordered_map<std::string, long long> cycle_lengths;

    long long button_press = 0;
    while (cycle_lengths.size() < watch_nodes.size()) {
        button_press++;
        auto result = simulate_button_press(modules, watch_nodes);

        for (const auto& node : result.high_senders) {
            if (!cycle_lengths.count(node)) {
                cycle_lengths[node] = button_press;
            }
        }
    }

    // LCM of all cycle lengths
    long long result = 1;
    for (const auto& [node, length] : cycle_lengths) {
        result = std::lcm(result, length);
    }

    return result;
}

int main() {
    auto modules = parse_input("../input.txt");

    std::cout << "Part 1: " << part1(modules) << std::endl;
    std::cout << "Part 2: " << part2(modules) << std::endl;

    return 0;
}
