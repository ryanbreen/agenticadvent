# frozen_string_literal: true

# Day 7: Camel Cards

input_path = File.join(__dir__, '..', 'input.txt')
lines = File.read(input_path).strip.split("\n")

# Card strength order (higher index = stronger)
CARD_STRENGTH = '23456789TJQKA'
CARD_STRENGTH_JOKER = 'J23456789TQKA' # J is weakest in Part 2

def tally(chars)
  # Count occurrences of each element (Ruby 2.7+ has Array#tally built-in)
  chars.each_with_object(Hash.new(0)) { |c, h| h[c] += 1 }
end

def type_from_counts(counts)
  # Convert sorted count array to hand type (higher = stronger)
  case counts
  when [5]          then 6  # Five of a kind
  when [4, 1]       then 5  # Four of a kind
  when [3, 2]       then 4  # Full house
  when [3, 1, 1]    then 3  # Three of a kind
  when [2, 2, 1]    then 2  # Two pair
  when [2, 1, 1, 1] then 1  # One pair
  else                   0  # High card
  end
end

def hand_type(hand)
  # Return hand type as integer (higher = stronger)
  counts = tally(hand.chars).values.sort.reverse
  type_from_counts(counts)
end

def hand_type_with_jokers(hand)
  # Return hand type with J as wildcards (higher = stronger)
  joker_count = hand.count('J')
  return hand_type(hand) if joker_count.zero?
  return 6 if joker_count == 5 # Five of a kind

  # Count non-joker cards and add jokers to highest count
  non_jokers = hand.chars.reject { |c| c == 'J' }
  counts = tally(non_jokers).values.sort.reverse
  counts[0] += joker_count

  type_from_counts(counts)
end

def hand_key(hand)
  # Return sort key for a hand (type, then card strengths)
  [hand_type(hand), hand.chars.map { |c| CARD_STRENGTH.index(c) }]
end

def hand_key_with_jokers(hand)
  # Return sort key for a hand with joker rules
  [hand_type_with_jokers(hand), hand.chars.map { |c| CARD_STRENGTH_JOKER.index(c) }]
end

def parse_hands(lines)
  # Parse input lines into [hand, bid] pairs
  lines.map { |line| line.split.then { |parts| [parts[0], parts[1].to_i] } }
end

def calculate_winnings(hands, &key_fn)
  # Sort hands by the provided key function and calculate total winnings
  hands.sort_by { |hand, _bid| key_fn.call(hand) }
       .each_with_index
       .sum { |(_, bid), index| (index + 1) * bid }
end

def part1(lines)
  calculate_winnings(parse_hands(lines)) { |hand| hand_key(hand) }
end

def part2(lines)
  calculate_winnings(parse_hands(lines)) { |hand| hand_key_with_jokers(hand) }
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
