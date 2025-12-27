#include <algorithm>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>
#include <vector>

struct Range {
    int64_t dst_start;
    int64_t src_start;
    int64_t length;
};

struct Interval {
    int64_t start;
    int64_t end;
};

std::vector<int64_t> seeds;
std::vector<std::vector<Range>> maps;

void parse_input(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;

    // Parse seeds
    std::getline(file, line);
    std::istringstream iss(line.substr(7)); // Skip "seeds: "
    int64_t num;
    while (iss >> num) {
        seeds.push_back(num);
    }

    // Skip empty line
    std::getline(file, line);

    // Parse maps
    std::vector<Range> current_map;
    while (std::getline(file, line)) {
        if (line.empty()) {
            if (!current_map.empty()) {
                maps.push_back(current_map);
                current_map.clear();
            }
        } else if (line.find("map:") != std::string::npos) {
            // Header line, skip
            continue;
        } else {
            Range r;
            std::istringstream range_iss(line);
            range_iss >> r.dst_start >> r.src_start >> r.length;
            current_map.push_back(r);
        }
    }
    if (!current_map.empty()) {
        maps.push_back(current_map);
    }
}

int64_t apply_map(int64_t value, const std::vector<Range>& ranges) {
    for (const auto& r : ranges) {
        if (value >= r.src_start && value < r.src_start + r.length) {
            return r.dst_start + (value - r.src_start);
        }
    }
    return value;
}

int64_t seed_to_location(int64_t seed) {
    int64_t value = seed;
    for (const auto& map_ranges : maps) {
        value = apply_map(value, map_ranges);
    }
    return value;
}

int64_t part1() {
    int64_t min_location = std::numeric_limits<int64_t>::max();
    for (int64_t seed : seeds) {
        min_location = std::min(min_location, seed_to_location(seed));
    }
    return min_location;
}

std::vector<Interval> apply_map_to_ranges(const std::vector<Interval>& input_ranges,
                                          const std::vector<Range>& map_ranges) {
    std::vector<Interval> result;

    for (const auto& interval : input_ranges) {
        std::vector<Interval> remaining;
        remaining.push_back(interval);

        for (const auto& r : map_ranges) {
            int64_t src_end = r.src_start + r.length;
            std::vector<Interval> new_remaining;

            for (const auto& rem : remaining) {
                // Part before the map range (unmapped)
                if (rem.start < r.src_start) {
                    new_remaining.push_back({rem.start, std::min(rem.end, r.src_start)});
                }

                // Part within the map range (mapped)
                int64_t overlap_start = std::max(rem.start, r.src_start);
                int64_t overlap_end = std::min(rem.end, src_end);
                if (overlap_start < overlap_end) {
                    int64_t offset = r.dst_start - r.src_start;
                    result.push_back({overlap_start + offset, overlap_end + offset});
                }

                // Part after the map range (unmapped)
                if (rem.end > src_end) {
                    new_remaining.push_back({std::max(rem.start, src_end), rem.end});
                }
            }

            remaining = new_remaining;
        }

        // Any remaining parts are unmapped (identity)
        for (const auto& rem : remaining) {
            result.push_back(rem);
        }
    }

    return result;
}

int64_t part2() {
    // Convert seeds to ranges
    std::vector<Interval> ranges;
    for (size_t i = 0; i < seeds.size(); i += 2) {
        int64_t start = seeds[i];
        int64_t length = seeds[i + 1];
        ranges.push_back({start, start + length});
    }

    // Apply each map to the ranges
    for (const auto& map_ranges : maps) {
        ranges = apply_map_to_ranges(ranges, map_ranges);
    }

    // Find minimum start of any range
    int64_t min_location = std::numeric_limits<int64_t>::max();
    for (const auto& r : ranges) {
        min_location = std::min(min_location, r.start);
    }
    return min_location;
}

int main() {
    parse_input("../input.txt");

    std::cout << "Part 1: " << part1() << std::endl;
    std::cout << "Part 2: " << part2() << std::endl;

    return 0;
}
