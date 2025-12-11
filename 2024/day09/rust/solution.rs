use std::fs;
use std::collections::HashMap;

/// Parse disk map into expanded block representation.
/// Returns vector where each element is file ID (as i32) or -1 for free space.
fn parse_disk_map(filename: &str) -> Vec<i32> {
    let disk_map = fs::read_to_string(filename)
        .expect("Failed to read input file")
        .trim()
        .to_string();

    let mut blocks = Vec::new();
    let mut file_id: i32 = 0;
    let mut is_file = true;

    for ch in disk_map.chars() {
        let length = ch.to_digit(10).expect("Invalid digit") as usize;
        if is_file {
            for _ in 0..length {
                blocks.push(file_id);
            }
            file_id += 1;
        } else {
            for _ in 0..length {
                blocks.push(-1); // -1 represents free space
            }
        }
        is_file = !is_file;
    }

    blocks
}

/// Compact disk by moving blocks one at a time from end to leftmost free space.
fn compact_blocks(blocks: &[i32]) -> Vec<i32> {
    let mut blocks = blocks.to_vec();
    let mut left = 0;
    let mut right = blocks.len() - 1;

    while left < right {
        // Find leftmost free space
        while left < right && blocks[left] != -1 {
            left += 1;
        }
        // Find rightmost file block
        while left < right && blocks[right] == -1 {
            right -= 1;
        }

        if left < right {
            // Swap
            blocks[left] = blocks[right];
            blocks[right] = -1;
            left += 1;
            right -= 1;
        }
    }

    blocks
}

/// Calculate filesystem checksum: sum of position * file_id for each block.
fn calculate_checksum(blocks: &[i32]) -> i64 {
    blocks
        .iter()
        .enumerate()
        .filter(|(_, &file_id)| file_id != -1)
        .map(|(pos, &file_id)| pos as i64 * file_id as i64)
        .sum()
}

/// Part 1: Compact by moving individual blocks, return checksum.
fn part1() -> i64 {
    let blocks = parse_disk_map("../input.txt");
    let compacted = compact_blocks(&blocks);
    calculate_checksum(&compacted)
}

/// Part 2: Compact by moving whole files (highest ID first), return checksum.
fn part2() -> i64 {
    let mut blocks = parse_disk_map("../input.txt");

    // Find all files: file_id -> (start_pos, length)
    let mut files: HashMap<i32, (usize, usize)> = HashMap::new();
    let mut i = 0;
    while i < blocks.len() {
        if blocks[i] != -1 {
            let file_id = blocks[i];
            let start = i;
            while i < blocks.len() && blocks[i] == file_id {
                i += 1;
            }
            files.insert(file_id, (start, i - start));
        } else {
            i += 1;
        }
    }

    // Process files in decreasing order of file ID
    let max_file_id = *files.keys().max().unwrap();

    for file_id in (0..=max_file_id).rev() {
        if let Some(&(start, length)) = files.get(&file_id) {
            // Find leftmost span of free space that fits this file
            // Must be to the left of current position
            let mut free_start: Option<usize> = None;
            let mut i = 0;
            while i < start {
                if blocks[i] == -1 {
                    // Count consecutive free blocks
                    let span_start = i;
                    let mut span_length = 0;
                    while i < start && blocks[i] == -1 {
                        span_length += 1;
                        i += 1;
                    }
                    if span_length >= length {
                        free_start = Some(span_start);
                        break;
                    }
                } else {
                    i += 1;
                }
            }

            // Move file if we found a suitable span
            if let Some(free_start) = free_start {
                // Clear old position
                for j in start..start + length {
                    blocks[j] = -1;
                }
                // Write to new position
                for j in free_start..free_start + length {
                    blocks[j] = file_id;
                }
                // Update file position
                files.insert(file_id, (free_start, length));
            }
        }
    }

    calculate_checksum(&blocks)
}

fn main() {
    println!("Part 1: {}", part1());
    println!("Part 2: {}", part2());
}
