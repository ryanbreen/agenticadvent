// Day 20: Pulse Propagation - Module communication simulation

use std::collections::{HashMap, VecDeque};
use std::fs;

#[derive(Clone)]
enum ModuleType {
    Broadcaster,
    FlipFlop { state: bool },
    Conjunction { memory: HashMap<String, bool> },
}

#[derive(Clone)]
struct Module {
    module_type: ModuleType,
    destinations: Vec<String>,
}

fn parse_input(filename: &str) -> HashMap<String, Module> {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    let mut modules: HashMap<String, Module> = HashMap::new();

    for line in content.trim().lines() {
        let parts: Vec<&str> = line.split(" -> ").collect();
        let name_part = parts[0];
        let destinations: Vec<String> = parts[1].split(", ").map(|s| s.to_string()).collect();

        if name_part == "broadcaster" {
            modules.insert(
                "broadcaster".to_string(),
                Module {
                    module_type: ModuleType::Broadcaster,
                    destinations,
                },
            );
        } else if name_part.starts_with('%') {
            let name = &name_part[1..];
            modules.insert(
                name.to_string(),
                Module {
                    module_type: ModuleType::FlipFlop { state: false },
                    destinations,
                },
            );
        } else if name_part.starts_with('&') {
            let name = &name_part[1..];
            modules.insert(
                name.to_string(),
                Module {
                    module_type: ModuleType::Conjunction {
                        memory: HashMap::new(),
                    },
                    destinations,
                },
            );
        }
    }

    // Initialize conjunction memory for all inputs
    let module_names: Vec<String> = modules.keys().cloned().collect();
    for name in &module_names {
        let dests: Vec<String> = modules.get(name).unwrap().destinations.clone();
        for dest in dests {
            if let Some(module) = modules.get_mut(&dest) {
                if let ModuleType::Conjunction { ref mut memory } = module.module_type {
                    memory.insert(name.clone(), false);
                }
            }
        }
    }

    modules
}

fn simulate_button_press(
    modules: &mut HashMap<String, Module>,
    watch_nodes: &Option<Vec<String>>,
) -> (u64, u64, Vec<String>) {
    let mut low_count: u64 = 0;
    let mut high_count: u64 = 0;
    let mut high_senders: Vec<String> = Vec::new();

    // Queue: (source, destination, pulse) where pulse is true for high, false for low
    let mut queue: VecDeque<(String, String, bool)> = VecDeque::new();
    queue.push_back(("button".to_string(), "broadcaster".to_string(), false));

    while let Some((source, dest, pulse)) = queue.pop_front() {
        if pulse {
            high_count += 1;
        } else {
            low_count += 1;
        }

        // Track if watched nodes send high pulses
        if let Some(ref nodes) = watch_nodes {
            if pulse && nodes.contains(&source) {
                high_senders.push(source.clone());
            }
        }

        if let Some(module) = modules.get_mut(&dest) {
            match &mut module.module_type {
                ModuleType::Broadcaster => {
                    let dests = module.destinations.clone();
                    for next_dest in dests {
                        queue.push_back((dest.clone(), next_dest, pulse));
                    }
                }
                ModuleType::FlipFlop { ref mut state } => {
                    if !pulse {
                        // Only react to low pulses
                        *state = !*state;
                        let new_pulse = *state;
                        let dests = module.destinations.clone();
                        for next_dest in dests {
                            queue.push_back((dest.clone(), next_dest, new_pulse));
                        }
                    }
                }
                ModuleType::Conjunction { ref mut memory } => {
                    memory.insert(source.clone(), pulse);
                    // Send low if all inputs are high, otherwise send high
                    let output = !memory.values().all(|&v| v);
                    let dests = module.destinations.clone();
                    for next_dest in dests {
                        queue.push_back((dest.clone(), next_dest, output));
                    }
                }
            }
        }
    }

    (low_count, high_count, high_senders)
}

fn reset_state(modules: &mut HashMap<String, Module>) {
    for module in modules.values_mut() {
        match &mut module.module_type {
            ModuleType::FlipFlop { ref mut state } => {
                *state = false;
            }
            ModuleType::Conjunction { ref mut memory } => {
                for val in memory.values_mut() {
                    *val = false;
                }
            }
            _ => {}
        }
    }
}

fn part1(modules: &mut HashMap<String, Module>) -> u64 {
    reset_state(modules);

    let mut total_low: u64 = 0;
    let mut total_high: u64 = 0;

    for _ in 0..1000 {
        let (low, high, _) = simulate_button_press(modules, &None);
        total_low += low;
        total_high += high;
    }

    total_low * total_high
}

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(a: u64, b: u64) -> u64 {
    a / gcd(a, b) * b
}

fn part2(modules: &mut HashMap<String, Module>) -> u64 {
    reset_state(modules);

    // Find the module that feeds into rx
    let mut rx_input: Option<String> = None;
    for (name, module) in modules.iter() {
        if module.destinations.contains(&"rx".to_string()) {
            rx_input = Some(name.clone());
            break;
        }
    }

    let rx_input = match rx_input {
        Some(name) => name,
        None => return 0,
    };

    // Find all modules that feed into rx_input
    let watch_nodes: Vec<String> = if let Some(module) = modules.get(&rx_input) {
        if let ModuleType::Conjunction { ref memory } = module.module_type {
            memory.keys().cloned().collect()
        } else {
            return 0;
        }
    } else {
        return 0;
    };

    let watch_option = Some(watch_nodes.clone());
    let mut cycle_lengths: HashMap<String, u64> = HashMap::new();

    let mut button_press: u64 = 0;
    while cycle_lengths.len() < watch_nodes.len() {
        button_press += 1;
        let (_, _, high_senders) = simulate_button_press(modules, &watch_option);

        for node in high_senders {
            if !cycle_lengths.contains_key(&node) {
                cycle_lengths.insert(node, button_press);
            }
        }
    }

    // LCM of all cycle lengths
    let mut result: u64 = 1;
    for &length in cycle_lengths.values() {
        result = lcm(result, length);
    }

    result
}

fn main() {
    let mut modules = parse_input("../input.txt");
    println!("Part 1: {}", part1(&mut modules));

    // Re-parse for part 2 (fresh state)
    let mut modules = parse_input("../input.txt");
    println!("Part 2: {}", part2(&mut modules));
}
