#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <regex>
#include <cmath>
#include <algorithm>
#include <limits>
#include <map>

using namespace std;

// Fraction class for exact rational arithmetic
class Fraction {
public:
    long long num, den;

    Fraction(long long n = 0, long long d = 1) : num(n), den(d) {
        if (den == 0) throw runtime_error("Division by zero");
        normalize();
    }

    void normalize() {
        if (den < 0) {
            num = -num;
            den = -den;
        }
        long long g = gcd(abs(num), abs(den));
        if (g > 0) {
            num /= g;
            den /= g;
        }
    }

    static long long gcd(long long a, long long b) {
        while (b != 0) {
            long long t = b;
            b = a % b;
            a = t;
        }
        return a;
    }

    Fraction operator+(const Fraction& other) const {
        return Fraction(num * other.den + other.num * den, den * other.den);
    }

    Fraction operator-(const Fraction& other) const {
        return Fraction(num * other.den - other.num * den, den * other.den);
    }

    Fraction operator*(const Fraction& other) const {
        return Fraction(num * other.num, den * other.den);
    }

    Fraction operator/(const Fraction& other) const {
        return Fraction(num * other.den, den * other.num);
    }

    Fraction operator-() const {
        return Fraction(-num, den);
    }

    bool operator<(const Fraction& other) const {
        return num * other.den < other.num * den;
    }

    bool operator>(const Fraction& other) const {
        return num * other.den > other.num * den;
    }

    bool operator==(const Fraction& other) const {
        return num * other.den == other.num * den;
    }

    bool operator!=(const Fraction& other) const {
        return !(*this == other);
    }

    double toDouble() const {
        return (double)num / den;
    }
};

struct Machine {
    int n_lights;
    long long target;
    vector<long long> button_masks;
    vector<int> joltage;
    vector<vector<int>> button_indices;
};

Machine parseLine(const string& line) {
    Machine m;

    // Extract indicator pattern [.##.]
    regex indicator_regex("\\[([.#]+)\\]");
    smatch match;
    if (regex_search(line, match, indicator_regex)) {
        string indicator = match[1].str();
        m.n_lights = indicator.length();
        m.target = 0;
        for (int i = 0; i < indicator.length(); i++) {
            if (indicator[i] == '#') {
                m.target |= (1LL << i);
            }
        }
    }

    // Extract button schematics (0,1,2) etc.
    regex button_regex("\\(([0-9,]+)\\)");
    auto buttons_begin = sregex_iterator(line.begin(), line.end(), button_regex);
    auto buttons_end = sregex_iterator();

    for (auto it = buttons_begin; it != buttons_end; ++it) {
        string button_str = (*it)[1].str();
        vector<int> indices;
        long long mask = 0;

        size_t pos = 0;
        while (pos < button_str.length()) {
            size_t comma = button_str.find(',', pos);
            if (comma == string::npos) comma = button_str.length();
            int idx = stoi(button_str.substr(pos, comma - pos));
            indices.push_back(idx);
            mask |= (1LL << idx);
            pos = comma + 1;
        }

        m.button_masks.push_back(mask);
        m.button_indices.push_back(indices);
    }

    // Extract joltage requirements {3,5,4,7}
    regex joltage_regex("\\{([0-9,]+)\\}");
    if (regex_search(line, match, joltage_regex)) {
        string joltage_str = match[1].str();
        size_t pos = 0;
        while (pos < joltage_str.length()) {
            size_t comma = joltage_str.find(',', pos);
            if (comma == string::npos) comma = joltage_str.length();
            m.joltage.push_back(stoi(joltage_str.substr(pos, comma - pos)));
            pos = comma + 1;
        }
    }

    return m;
}

int countBits(long long n) {
    int count = 0;
    while (n) {
        count += n & 1;
        n >>= 1;
    }
    return count;
}

int solveMachineBrute(const Machine& m) {
    int n_buttons = m.button_masks.size();
    int min_presses = numeric_limits<int>::max();

    // Try all 2^n_buttons combinations
    for (long long mask = 0; mask < (1LL << n_buttons); mask++) {
        long long state = 0;
        int presses = 0;

        for (int i = 0; i < n_buttons; i++) {
            if (mask & (1LL << i)) {
                state ^= m.button_masks[i];
                presses++;
            }
        }

        if (state == m.target) {
            min_presses = min(min_presses, presses);
        }
    }

    return min_presses == numeric_limits<int>::max() ? 0 : min_presses;
}

int part1(const vector<string>& lines) {
    int total = 0;
    for (const string& line : lines) {
        if (line.empty()) continue;
        Machine m = parseLine(line);
        total += solveMachineBrute(m);
    }
    return total;
}

int solveMachinePart2(const Machine& m) {
    int n_counters = m.joltage.size();
    int n_buttons = m.button_indices.size();

    if (n_buttons == 0) {
        bool all_zero = true;
        for (int j : m.joltage) {
            if (j != 0) all_zero = false;
        }
        return all_zero ? 0 : numeric_limits<int>::max();
    }

    // Build matrix A (n_counters x n_buttons) using Fraction
    vector<vector<Fraction>> A(n_counters, vector<Fraction>(n_buttons, Fraction(0)));
    for (int j = 0; j < n_buttons; j++) {
        for (int idx : m.button_indices[j]) {
            if (idx < n_counters) {
                A[idx][j] = Fraction(1);
            }
        }
    }

    vector<Fraction> b(n_counters);
    for (int i = 0; i < n_counters; i++) {
        b[i] = Fraction(m.joltage[i]);
    }

    // Augmented matrix [A | b]
    vector<vector<Fraction>> aug(n_counters);
    for (int i = 0; i < n_counters; i++) {
        aug[i] = A[i];
        aug[i].push_back(b[i]);
    }

    // Gaussian elimination with partial pivoting
    vector<pair<int, int>> pivot_cols; // (col, row)
    int pivot_row = 0;

    for (int col = 0; col < n_buttons; col++) {
        // Find non-zero entry in this column
        int found = -1;
        for (int row = pivot_row; row < n_counters; row++) {
            if (aug[row][col] != Fraction(0)) {
                found = row;
                break;
            }
        }

        if (found == -1) continue;

        // Swap rows
        swap(aug[pivot_row], aug[found]);
        pivot_cols.push_back({col, pivot_row});

        // Scale pivot row
        Fraction scale = aug[pivot_row][col];
        for (int c = 0; c <= n_buttons; c++) {
            aug[pivot_row][c] = aug[pivot_row][c] / scale;
        }

        // Eliminate column in other rows
        for (int row = 0; row < n_counters; row++) {
            if (row != pivot_row && aug[row][col] != Fraction(0)) {
                Fraction factor = aug[row][col];
                for (int c = 0; c <= n_buttons; c++) {
                    aug[row][c] = aug[row][c] - factor * aug[pivot_row][c];
                }
            }
        }

        pivot_row++;
    }

    // Check for inconsistency
    for (int row = pivot_row; row < n_counters; row++) {
        if (aug[row][n_buttons] != Fraction(0)) {
            return numeric_limits<int>::max();
        }
    }

    // Identify free variables
    vector<bool> is_pivot(n_buttons, false);
    for (auto [col, row] : pivot_cols) {
        is_pivot[col] = true;
    }

    vector<int> free_vars;
    for (int c = 0; c < n_buttons; c++) {
        if (!is_pivot[c]) {
            free_vars.push_back(c);
        }
    }

    // If no free variables, unique solution
    if (free_vars.empty()) {
        vector<Fraction> solution(n_buttons, Fraction(0));
        for (auto [col, row] : pivot_cols) {
            solution[col] = aug[row][n_buttons];
        }

        int total = 0;
        for (const Fraction& val : solution) {
            if (val < Fraction(0) || val.den != 1) {
                return numeric_limits<int>::max();
            }
            total += val.num;
        }
        return total;
    }

    // Extract null space vectors and particular solution
    int n_free = free_vars.size();
    vector<vector<Fraction>> null_vectors(n_free, vector<Fraction>(n_buttons, Fraction(0)));

    for (int i = 0; i < n_free; i++) {
        int fv = free_vars[i];
        null_vectors[i][fv] = Fraction(1);
        for (auto [col, row] : pivot_cols) {
            null_vectors[i][col] = -aug[row][fv];
        }
    }

    vector<Fraction> particular(n_buttons, Fraction(0));
    for (auto [col, row] : pivot_cols) {
        particular[col] = aug[row][n_buttons];
    }

    // Search for optimal solution
    int min_total = numeric_limits<int>::max();
    int max_j = *max_element(m.joltage.begin(), m.joltage.end());

    // For 1 free variable
    if (n_free == 1) {
        double t_low = -1e9, t_high = 1e9;

        for (int j = 0; j < n_buttons; j++) {
            Fraction p = particular[j];
            Fraction nv = null_vectors[0][j];

            if (nv == Fraction(0)) {
                if (p < Fraction(0)) {
                    return numeric_limits<int>::max();
                }
            } else if (nv > Fraction(0)) {
                t_low = max(t_low, (-p / nv).toDouble());
            } else {
                t_high = min(t_high, (-p / nv).toDouble());
            }
        }

        if (t_low > t_high) return numeric_limits<int>::max();

        long long t_low_int = (long long)ceil(t_low);
        long long t_high_int = (long long)floor(t_high);

        for (long long t = t_low_int; t <= t_high_int; t++) {
            Fraction t_frac(t);
            bool valid = true;
            int total = 0;

            for (int j = 0; j < n_buttons; j++) {
                Fraction val = particular[j] + t_frac * null_vectors[0][j];
                if (val < Fraction(0) || val.den != 1) {
                    valid = false;
                    break;
                }
                total += val.num;
            }

            if (valid) {
                min_total = min(min_total, total);
            }
        }

        return min_total == numeric_limits<int>::max() ? 0 : min_total;
    }

    // For 2 free variables
    if (n_free == 2) {
        int bound = max_j;

        for (int t0 = -bound; t0 <= bound; t0++) {
            Fraction t0_frac(t0);
            vector<Fraction> intermediate(n_buttons);
            for (int j = 0; j < n_buttons; j++) {
                intermediate[j] = particular[j] + t0_frac * null_vectors[0][j];
            }

            double t1_low = -1e9, t1_high = 1e9;
            for (int j = 0; j < n_buttons; j++) {
                double p = intermediate[j].toDouble();
                double nv = null_vectors[1][j].toDouble();
                if (nv > 1e-9) {
                    t1_low = max(t1_low, -p / nv);
                } else if (nv < -1e-9) {
                    t1_high = min(t1_high, -p / nv);
                }
            }

            long long t1_low_int = (long long)ceil(t1_low);
            long long t1_high_int = (long long)floor(t1_high);

            for (long long t1 = t1_low_int; t1 <= t1_high_int; t1++) {
                Fraction t1_frac(t1);
                bool valid = true;
                int total = 0;

                for (int j = 0; j < n_buttons; j++) {
                    Fraction val = intermediate[j] + t1_frac * null_vectors[1][j];
                    if (val < Fraction(0) || val.den != 1) {
                        valid = false;
                        break;
                    }
                    total += val.num;
                }

                if (valid) {
                    min_total = min(min_total, total);
                }
            }
        }

        return min_total == numeric_limits<int>::max() ? 0 : min_total;
    }

    // For 3+ free variables - use smaller bound
    if (n_free == 3) {
        int bound = max_j;

        for (int t0 = -bound; t0 <= bound; t0++) {
            Fraction t0_frac(t0);
            vector<Fraction> inter0(n_buttons);
            for (int j = 0; j < n_buttons; j++) {
                inter0[j] = particular[j] + t0_frac * null_vectors[0][j];
            }

            double t1_low = -bound, t1_high = bound;
            for (int j = 0; j < n_buttons; j++) {
                double p = inter0[j].toDouble();
                double nv = null_vectors[1][j].toDouble();
                if (nv > 1e-9) {
                    t1_low = max(t1_low, -p / nv - bound);
                } else if (nv < -1e-9) {
                    t1_high = min(t1_high, -p / nv + bound);
                }
            }

            for (int t1 = (int)ceil(t1_low); t1 <= (int)floor(t1_high); t1++) {
                Fraction t1_frac(t1);
                vector<Fraction> inter1(n_buttons);
                for (int j = 0; j < n_buttons; j++) {
                    inter1[j] = inter0[j] + t1_frac * null_vectors[1][j];
                }

                double t2_low = -1e9, t2_high = 1e9;
                for (int j = 0; j < n_buttons; j++) {
                    double p = inter1[j].toDouble();
                    double nv = null_vectors[2][j].toDouble();
                    if (nv > 1e-9) {
                        t2_low = max(t2_low, -p / nv);
                    } else if (nv < -1e-9) {
                        t2_high = min(t2_high, -p / nv);
                    }
                }

                for (int t2 = (int)ceil(t2_low); t2 <= (int)floor(t2_high); t2++) {
                    Fraction t2_frac(t2);
                    bool valid = true;
                    int total = 0;

                    for (int j = 0; j < n_buttons; j++) {
                        Fraction val = inter1[j] + t2_frac * null_vectors[2][j];
                        if (val < Fraction(0) || val.den != 1) {
                            valid = false;
                            break;
                        }
                        total += val.num;
                    }

                    if (valid) {
                        min_total = min(min_total, total);
                    }
                }
            }
        }

        return min_total == numeric_limits<int>::max() ? 0 : min_total;
    }

    return 0; // Fallback for large null spaces
}

int part2(const vector<string>& lines) {
    int total = 0;
    for (const string& line : lines) {
        if (line.empty()) continue;
        Machine m = parseLine(line);
        total += solveMachinePart2(m);
    }
    return total;
}

int main() {
    ifstream input("../input.txt");
    if (!input.is_open()) {
        cerr << "Failed to open input.txt" << endl;
        return 1;
    }

    vector<string> lines;
    string line;
    while (getline(input, line)) {
        lines.push_back(line);
    }
    input.close();

    cout << "Part 1: " << part1(lines) << endl;
    cout << "Part 2: " << part2(lines) << endl;

    return 0;
}
