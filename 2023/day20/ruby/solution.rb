#!/usr/bin/env ruby
# Day 20: Pulse Propagation - Module communication simulation

def parse_input(filename)
  modules = {}

  File.readlines(filename, chomp: true).each do |line|
    name_part, dest_part = line.split(' -> ')
    destinations = dest_part.split(',').map(&:strip)

    if name_part == 'broadcaster'
      modules['broadcaster'] = { type: :broadcaster, destinations: destinations }
    elsif name_part.start_with?('%')
      name = name_part[1..]
      modules[name] = { type: :flip_flop, destinations: destinations, state: false }
    elsif name_part.start_with?('&')
      name = name_part[1..]
      modules[name] = { type: :conjunction, destinations: destinations, memory: {} }
    end
  end

  # Initialize conjunction memory for all inputs
  modules.each do |name, mod|
    mod[:destinations].each do |dest|
      if modules[dest] && modules[dest][:type] == :conjunction
        modules[dest][:memory][name] = false
      end
    end
  end

  modules
end

def simulate_button_press(modules, watch_nodes = nil)
  low_count = 0
  high_count = 0
  high_senders = []

  # Queue: [source, destination, pulse] where pulse is true for high, false for low
  queue = [['button', 'broadcaster', false]]

  while !queue.empty?
    source, dest, pulse = queue.shift

    if pulse
      high_count += 1
    else
      low_count += 1
    end

    # Track if watched nodes send high pulses
    if watch_nodes && watch_nodes.include?(source) && pulse
      high_senders << source
    end

    next unless modules[dest]

    mod = modules[dest]

    case mod[:type]
    when :broadcaster
      mod[:destinations].each do |next_dest|
        queue << [dest, next_dest, pulse]
      end

    when :flip_flop
      # Only react to low pulses
      unless pulse
        mod[:state] = !mod[:state]
        mod[:destinations].each do |next_dest|
          queue << [dest, next_dest, mod[:state]]
        end
      end

    when :conjunction
      mod[:memory][source] = pulse
      # Send low if all inputs are high, otherwise send high
      output = !mod[:memory].values.all?
      mod[:destinations].each do |next_dest|
        queue << [dest, next_dest, output]
      end
    end
  end

  [low_count, high_count, high_senders]
end

def reset_state(modules)
  modules.each_value do |mod|
    case mod[:type]
    when :flip_flop
      mod[:state] = false
    when :conjunction
      mod[:memory].transform_values! { false }
    end
  end
end

def part1(modules)
  reset_state(modules)

  total_low = 0
  total_high = 0

  1000.times do
    low, high, _ = simulate_button_press(modules)
    total_low += low
    total_high += high
  end

  total_low * total_high
end

def part2(modules)
  reset_state(modules)

  # Find the module that feeds into rx
  rx_input = nil
  modules.each do |name, mod|
    if mod[:destinations].include?('rx')
      rx_input = name
      break
    end
  end

  return 0 if rx_input.nil?

  # Find all modules that feed into rx_input
  watch_nodes = modules[rx_input][:memory].keys
  cycle_lengths = {}

  button_press = 0
  while cycle_lengths.size < watch_nodes.size
    button_press += 1
    _, _, high_senders = simulate_button_press(modules, watch_nodes)

    high_senders.each do |node|
      cycle_lengths[node] ||= button_press
    end
  end

  # LCM of all cycle lengths
  cycle_lengths.values.reduce(1) { |acc, len| acc.lcm(len) }
end

def main
  input_file = File.join(File.dirname(__FILE__), '..', 'input.txt')

  modules = parse_input(input_file)
  puts "Part 1: #{part1(modules)}"

  # Re-parse for part 2 (fresh state)
  modules = parse_input(input_file)
  puts "Part 2: #{part2(modules)}"
end

main
