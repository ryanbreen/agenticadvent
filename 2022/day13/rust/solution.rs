use std::cmp::Ordering;
use std::fs;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    Int(i32),
    List(Vec<Value>),
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            // Both integers
            (Value::Int(l), Value::Int(r)) => l.cmp(r),

            // Both lists
            (Value::List(l), Value::List(r)) => {
                for i in 0..l.len().min(r.len()) {
                    match l[i].cmp(&r[i]) {
                        Ordering::Equal => continue,
                        ord => return ord,
                    }
                }
                l.len().cmp(&r.len())
            }

            // Mixed types - convert integer to list
            (Value::Int(l), Value::List(_)) => Value::List(vec![Value::Int(*l)]).cmp(other),
            (Value::List(_), Value::Int(r)) => self.cmp(&Value::List(vec![Value::Int(*r)])),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn parse_value(s: &str) -> (Value, usize) {
    let chars: Vec<char> = s.chars().collect();
    parse_value_inner(&chars, 0)
}

fn parse_value_inner(chars: &[char], mut pos: usize) -> (Value, usize) {
    if chars[pos] == '[' {
        // Parse list
        pos += 1; // skip '['
        let mut items = Vec::new();

        while chars[pos] != ']' {
            if chars[pos] == ',' {
                pos += 1;
                continue;
            }
            let (value, new_pos) = parse_value_inner(chars, pos);
            items.push(value);
            pos = new_pos;
        }
        pos += 1; // skip ']'
        (Value::List(items), pos)
    } else {
        // Parse integer
        let start = pos;
        while pos < chars.len() && (chars[pos].is_ascii_digit() || chars[pos] == '-') {
            pos += 1;
        }
        let num: i32 = chars[start..pos].iter().collect::<String>().parse().unwrap();
        (Value::Int(num), pos)
    }
}

fn parse_packet(line: &str) -> Value {
    let (value, _) = parse_value(line);
    value
}

fn part1(text: &str) -> usize {
    let pairs: Vec<&str> = text.trim().split("\n\n").collect();
    let mut total = 0;

    for (i, pair) in pairs.iter().enumerate() {
        let lines: Vec<&str> = pair.lines().collect();
        let left = parse_packet(lines[0]);
        let right = parse_packet(lines[1]);

        if left < right {
            total += i + 1;
        }
    }

    total
}

fn part2(text: &str) -> usize {
    let mut packets: Vec<Value> = text
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| parse_packet(line))
        .collect();

    // Add divider packets
    let divider1 = Value::List(vec![Value::List(vec![Value::Int(2)])]);
    let divider2 = Value::List(vec![Value::List(vec![Value::Int(6)])]);
    packets.push(divider1.clone());
    packets.push(divider2.clone());

    // Sort
    packets.sort();

    // Find positions of dividers (1-indexed)
    let pos1 = packets.iter().position(|p| *p == divider1).unwrap() + 1;
    let pos2 = packets.iter().position(|p| *p == divider2).unwrap() + 1;

    pos1 * pos2
}

fn main() {
    let input_path = std::env::current_dir()
        .unwrap()
        .join("../input.txt");
    let text = fs::read_to_string(input_path).expect("Failed to read input file");

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
