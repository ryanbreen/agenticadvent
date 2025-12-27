use std::collections::HashSet;
use std::fs;

struct Card {
    winning: HashSet<u32>,
    have: Vec<u32>,
}

fn parse_card(line: &str) -> Option<Card> {
    let (_, numbers) = line.split_once(':')?;
    let (winning_str, have_str) = numbers.split_once('|')?;

    let winning: HashSet<u32> = winning_str
        .split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect();

    let have: Vec<u32> = have_str
        .split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect();

    Some(Card { winning, have })
}

fn count_matches(card: &Card) -> usize {
    card.have.iter().filter(|n| card.winning.contains(n)).count()
}

fn part1(cards: &[Card]) -> u32 {
    cards.iter()
        .map(|card| {
            let matches = count_matches(card);
            if matches > 0 {
                1 << (matches - 1)  // 2^(matches-1)
            } else {
                0
            }
        })
        .sum()
}

fn part2(cards: &[Card]) -> u32 {
    let matches: Vec<usize> = cards.iter().map(count_matches).collect();
    let mut copies: Vec<u32> = vec![1; cards.len()];

    for (i, &m) in matches.iter().enumerate() {
        let current_copies = copies[i];
        for j in (i + 1)..(i + 1 + m).min(cards.len()) {
            copies[j] += current_copies;
        }
    }

    copies.iter().sum()
}

fn main() {
    // Read input file
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    // Parse all cards
    let cards: Vec<Card> = input.lines()
        .filter_map(parse_card)
        .collect();

    // Solve both parts
    let part1_result = part1(&cards);
    let part2_result = part2(&cards);

    println!("Part 1: {}", part1_result);
    println!("Part 2: {}", part2_result);
}
