#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <variant>
#include <algorithm>
#include <memory>

// Forward declaration for recursive type
struct Packet;

// A value is either an integer or a list of packets
using PacketList = std::vector<Packet>;
using PacketValue = std::variant<int, PacketList>;

struct Packet {
    PacketValue value;

    Packet() : value(PacketList{}) {}
    Packet(int n) : value(n) {}
    Packet(PacketList list) : value(std::move(list)) {}

    bool is_int() const { return std::holds_alternative<int>(value); }
    bool is_list() const { return std::holds_alternative<PacketList>(value); }

    int as_int() const { return std::get<int>(value); }
    const PacketList& as_list() const { return std::get<PacketList>(value); }
};

// Parser state
struct Parser {
    const std::string& input;
    size_t pos = 0;

    Parser(const std::string& s) : input(s) {}

    char peek() const {
        return pos < input.size() ? input[pos] : '\0';
    }

    char consume() {
        return input[pos++];
    }

    void skip_whitespace() {
        while (pos < input.size() && std::isspace(input[pos])) ++pos;
    }

    Packet parse() {
        skip_whitespace();
        if (peek() == '[') {
            return parse_list();
        } else {
            return parse_int();
        }
    }

    Packet parse_list() {
        consume();  // '['
        PacketList list;

        skip_whitespace();
        if (peek() != ']') {
            list.push_back(parse());
            skip_whitespace();
            while (peek() == ',') {
                consume();  // ','
                list.push_back(parse());
                skip_whitespace();
            }
        }
        consume();  // ']'
        return Packet(std::move(list));
    }

    Packet parse_int() {
        int n = 0;
        while (std::isdigit(peek())) {
            n = n * 10 + (consume() - '0');
        }
        return Packet(n);
    }
};

Packet parse_packet(const std::string& line) {
    Parser parser(line);
    return parser.parse();
}

// Compare two packets
// Returns: -1 if left < right (correct order)
//           1 if left > right (wrong order)
//           0 if equal (continue)
int compare(const Packet& left, const Packet& right) {
    // Both integers
    if (left.is_int() && right.is_int()) {
        int l = left.as_int();
        int r = right.as_int();
        if (l < r) return -1;
        if (l > r) return 1;
        return 0;
    }

    // Both lists
    if (left.is_list() && right.is_list()) {
        const auto& l = left.as_list();
        const auto& r = right.as_list();

        size_t min_len = std::min(l.size(), r.size());
        for (size_t i = 0; i < min_len; ++i) {
            int result = compare(l[i], r[i]);
            if (result != 0) return result;
        }

        // Check lengths
        if (l.size() < r.size()) return -1;
        if (l.size() > r.size()) return 1;
        return 0;
    }

    // Mixed types - convert integer to list
    if (left.is_int()) {
        PacketList wrapper;
        wrapper.push_back(Packet(left.as_int()));
        return compare(Packet(wrapper), right);
    } else {
        PacketList wrapper;
        wrapper.push_back(Packet(right.as_int()));
        return compare(left, Packet(wrapper));
    }
}

int part1(const std::string& text) {
    std::istringstream iss(text);
    std::string line;
    int total = 0;
    int pair_idx = 0;

    while (true) {
        std::string line1, line2;

        // Skip empty lines, read first packet
        while (std::getline(iss, line1) && line1.empty());
        if (line1.empty()) break;

        // Read second packet
        if (!std::getline(iss, line2) || line2.empty()) break;

        ++pair_idx;

        Packet left = parse_packet(line1);
        Packet right = parse_packet(line2);

        if (compare(left, right) == -1) {
            total += pair_idx;
        }
    }

    return total;
}

int part2(const std::string& text) {
    std::vector<Packet> packets;

    std::istringstream iss(text);
    std::string line;

    while (std::getline(iss, line)) {
        if (line.empty()) continue;
        packets.push_back(parse_packet(line));
    }

    // Add divider packets
    Packet divider1 = parse_packet("[[2]]");
    Packet divider2 = parse_packet("[[6]]");
    packets.push_back(divider1);
    packets.push_back(divider2);

    // Sort using comparison function
    std::sort(packets.begin(), packets.end(), [](const Packet& a, const Packet& b) {
        return compare(a, b) < 0;
    });

    // Find positions of dividers (1-indexed)
    Packet target1 = parse_packet("[[2]]");
    Packet target2 = parse_packet("[[6]]");

    int pos1 = 0, pos2 = 0;
    for (size_t i = 0; i < packets.size(); ++i) {
        if (compare(packets[i], target1) == 0) pos1 = static_cast<int>(i + 1);
        if (compare(packets[i], target2) == 0) pos2 = static_cast<int>(i + 1);
    }

    return pos1 * pos2;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();

    std::cout << "Part 1: " << part1(text) << std::endl;
    std::cout << "Part 2: " << part2(text) << std::endl;

    return 0;
}
