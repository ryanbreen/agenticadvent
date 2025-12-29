use std::fs;

fn parse_input(text: &str) -> Vec<Vec<i64>> {
    text.lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect()
        })
        .collect()
}

fn get_differences(seq: &[i64]) -> Vec<i64> {
    seq.windows(2).map(|w| w[1] - w[0]).collect()
}

fn extrapolate_next(seq: &[i64]) -> i64 {
    let mut sequences: Vec<Vec<i64>> = vec![seq.to_vec()];
    let mut current = seq.to_vec();

    while !current.iter().all(|&x| x == 0) {
        current = get_differences(&current);
        sequences.push(current.clone());
    }

    // Work backwards, adding last elements
    for i in (0..sequences.len() - 1).rev() {
        let next_val = sequences[i].last().unwrap() + sequences[i + 1].last().unwrap();
        sequences[i].push(next_val);
    }

    *sequences[0].last().unwrap()
}

fn extrapolate_prev(seq: &[i64]) -> i64 {
    let mut sequences: Vec<Vec<i64>> = vec![seq.to_vec()];
    let mut current = seq.to_vec();

    while !current.iter().all(|&x| x == 0) {
        current = get_differences(&current);
        sequences.push(current.clone());
    }

    // Work backwards, subtracting first elements
    for i in (0..sequences.len() - 1).rev() {
        let prev_val = sequences[i][0] - sequences[i + 1][0];
        sequences[i].insert(0, prev_val);
    }

    sequences[0][0]
}

fn part1(histories: &[Vec<i64>]) -> i64 {
    histories.iter().map(|h| extrapolate_next(h)).sum()
}

fn part2(histories: &[Vec<i64>]) -> i64 {
    histories.iter().map(|h| extrapolate_prev(h)).sum()
}

fn main() {
    let input = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let histories = parse_input(&input);

    println!("Part 1: {}", part1(&histories));
    println!("Part 2: {}", part2(&histories));
}
