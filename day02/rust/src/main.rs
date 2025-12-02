use std::fs;

fn is_invalid_id(num: u64) -> bool {
    let s = num.to_string();
    let len = s.len();

    // Must have even length to be split into two equal halves
    if len % 2 != 0 {
        return false;
    }

    let half_len = len / 2;
    let first_half = &s[..half_len];
    let second_half = &s[half_len..];

    // Check if the two halves are identical
    // Also ensure no leading zeroes (first half can't start with 0 unless it's "0")
    first_half == second_half && !first_half.starts_with('0')
}

fn count_invalid_ids_in_range(start: u64, end: u64) -> u64 {
    let mut sum = 0;
    for id in start..=end {
        if is_invalid_id(id) {
            sum += id;
        }
    }
    sum
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let input = input.trim();

    // Parse the ranges
    let mut total_sum = 0u64;

    for range in input.split(',') {
        let range = range.trim();
        if range.is_empty() {
            continue;
        }

        let parts: Vec<&str> = range.split('-').collect();
        if parts.len() != 2 {
            eprintln!("Invalid range format: {}", range);
            continue;
        }

        let start: u64 = parts[0].parse().expect("Failed to parse start");
        let end: u64 = parts[1].parse().expect("Failed to parse end");

        let range_sum = count_invalid_ids_in_range(start, end);
        total_sum += range_sum;
    }

    println!("Part 1: {}", total_sum);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_invalid_id() {
        assert!(is_invalid_id(11));
        assert!(is_invalid_id(22));
        assert!(is_invalid_id(99));
        assert!(is_invalid_id(1010));
        assert!(is_invalid_id(6464));
        assert!(is_invalid_id(123123));
        assert!(is_invalid_id(222222));
        assert!(is_invalid_id(446446));
        assert!(is_invalid_id(1188511885));
        assert!(is_invalid_id(38593859));

        assert!(!is_invalid_id(101)); // odd length
        assert!(!is_invalid_id(12)); // halves don't match
        assert!(!is_invalid_id(1234)); // halves don't match
    }

    #[test]
    fn test_example() {
        // Test example ranges
        assert_eq!(count_invalid_ids_in_range(11, 22), 11 + 22);
        assert_eq!(count_invalid_ids_in_range(95, 115), 99);
        assert_eq!(count_invalid_ids_in_range(998, 1012), 1010);
        assert_eq!(count_invalid_ids_in_range(222220, 222224), 222222);
        assert_eq!(count_invalid_ids_in_range(1698522, 1698528), 0);
        assert_eq!(count_invalid_ids_in_range(446443, 446449), 446446);
    }
}
