#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>

using namespace std;

/**
 * Parse disk map into expanded block representation.
 * Returns vector where each element is file ID or -1 for free space.
 */
vector<int> parse_disk_map(const string& filename) {
    ifstream file(filename);
    string disk_map;
    getline(file, disk_map);

    vector<int> blocks;
    int file_id = 0;
    bool is_file = true;

    for (char ch : disk_map) {
        int length = ch - '0';
        if (is_file) {
            for (int i = 0; i < length; i++) {
                blocks.push_back(file_id);
            }
            file_id++;
        } else {
            for (int i = 0; i < length; i++) {
                blocks.push_back(-1);  // -1 represents free space
            }
        }
        is_file = !is_file;
    }

    return blocks;
}

/**
 * Compact disk by moving blocks one at a time from end to leftmost free space.
 */
vector<int> compact_blocks(vector<int> blocks) {
    int left = 0;
    int right = blocks.size() - 1;

    while (left < right) {
        // Find leftmost free space
        while (left < right && blocks[left] != -1) {
            left++;
        }
        // Find rightmost file block
        while (left < right && blocks[right] == -1) {
            right--;
        }

        if (left < right) {
            // Swap
            blocks[left] = blocks[right];
            blocks[right] = -1;
            left++;
            right--;
        }
    }

    return blocks;
}

/**
 * Calculate filesystem checksum: sum of position * file_id for each block.
 */
long long calculate_checksum(const vector<int>& blocks) {
    long long checksum = 0;
    for (size_t pos = 0; pos < blocks.size(); pos++) {
        if (blocks[pos] != -1) {
            checksum += pos * blocks[pos];
        }
    }
    return checksum;
}

/**
 * Part 1: Compact by moving individual blocks, return checksum.
 */
long long part1() {
    vector<int> blocks = parse_disk_map("../input.txt");
    vector<int> compacted = compact_blocks(blocks);
    return calculate_checksum(compacted);
}

/**
 * Part 2: Compact by moving whole files (highest ID first), return checksum.
 */
long long part2() {
    vector<int> blocks = parse_disk_map("../input.txt");

    // Find all files: file_id -> (start_pos, length)
    map<int, pair<int, int>> files;
    size_t i = 0;
    while (i < blocks.size()) {
        if (blocks[i] != -1) {
            int file_id = blocks[i];
            int start = i;
            while (i < blocks.size() && blocks[i] == file_id) {
                i++;
            }
            files[file_id] = make_pair(start, i - start);
        } else {
            i++;
        }
    }

    // Find max file ID
    int max_file_id = 0;
    for (const auto& entry : files) {
        max_file_id = max(max_file_id, entry.first);
    }

    // Process files in decreasing order of file ID
    for (int file_id = max_file_id; file_id >= 0; file_id--) {
        if (files.find(file_id) == files.end()) {
            continue;
        }

        int start = files[file_id].first;
        int length = files[file_id].second;

        // Find leftmost span of free space that fits this file
        // Must be to the left of current position
        int free_start = -1;
        i = 0;
        while (i < (size_t)start) {
            if (blocks[i] == -1) {
                // Count consecutive free blocks
                int span_start = i;
                int span_length = 0;
                while (i < (size_t)start && blocks[i] == -1) {
                    span_length++;
                    i++;
                }
                if (span_length >= length) {
                    free_start = span_start;
                    break;
                }
            } else {
                i++;
            }
        }

        // Move file if we found a suitable span
        if (free_start != -1) {
            // Clear old position
            for (int j = start; j < start + length; j++) {
                blocks[j] = -1;
            }
            // Write to new position
            for (int j = free_start; j < free_start + length; j++) {
                blocks[j] = file_id;
            }
            // Update file position
            files[file_id] = make_pair(free_start, length);
        }
    }

    return calculate_checksum(blocks);
}

int main() {
    cout << "Part 1: " << part1() << endl;
    cout << "Part 2: " << part2() << endl;
    return 0;
}
