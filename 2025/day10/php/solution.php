#!/usr/bin/env php
<?php
/**
 * Day 10: Factory - XOR toggle problem (Part 1) and ILP with Gaussian elimination (Part 2)
 */

/**
 * Parse a machine line for Part 1
 * Returns: [n_lights, target_state, buttons_array]
 */
function parse_line($line) {
    // Extract indicator pattern [.##.]
    preg_match('/\[([.#]+)\]/', $line, $indicator_match);
    $indicator = $indicator_match[1];
    $n_lights = strlen($indicator);

    // Build target state as bitmask
    $target = 0;
    for ($i = 0; $i < strlen($indicator); $i++) {
        if ($indicator[$i] === '#') {
            $target |= (1 << $i);
        }
    }

    // Extract button schematics (0,1,2) etc.
    $buttons = [];
    preg_match_all('/\(([0-9,]+)\)/', $line, $button_matches);
    foreach ($button_matches[1] as $button_str) {
        $indices = array_map('intval', explode(',', $button_str));
        $mask = 0;
        foreach ($indices as $idx) {
            $mask |= (1 << $idx);
        }
        $buttons[] = $mask;
    }

    return [$n_lights, $target, $buttons];
}

/**
 * Parse a machine line for Part 2
 * Returns: [n_counters, joltage_array, buttons_array]
 */
function parse_line_part2($line) {
    // Extract joltage requirements {3,5,4,7}
    preg_match('/\{([0-9,]+)\}/', $line, $joltage_match);
    $joltage = array_map('intval', explode(',', $joltage_match[1]));
    $n_counters = count($joltage);

    // Extract button schematics
    $buttons = [];
    preg_match_all('/\(([0-9,]+)\)/', $line, $button_matches);
    foreach ($button_matches[1] as $button_str) {
        $indices = array_map('intval', explode(',', $button_str));
        $buttons[] = $indices;
    }

    return [$n_counters, $joltage, $buttons];
}

/**
 * Solve machine with brute force for Part 1
 */
function solve_machine_brute($n_lights, $target, $buttons) {
    $n_buttons = count($buttons);
    $min_presses = PHP_INT_MAX;

    // Try all 2^n_buttons combinations
    for ($mask = 0; $mask < (1 << $n_buttons); $mask++) {
        $state = 0;
        $presses = 0;

        for ($i = 0; $i < $n_buttons; $i++) {
            if ($mask & (1 << $i)) {
                $state ^= $buttons[$i];
                $presses++;
            }
        }

        if ($state === $target) {
            $min_presses = min($min_presses, $presses);
        }
    }

    return $min_presses === PHP_INT_MAX ? 0 : $min_presses;
}

/**
 * GMP-based rational number class for exact arithmetic
 */
class Rational {
    public $num;  // GMP numerator
    public $den;  // GMP denominator

    public function __construct($num, $den = null) {
        if ($den === null) {
            $den = gmp_init(1);
        }

        if (is_int($num)) {
            $num = gmp_init($num);
        }
        if (is_int($den)) {
            $den = gmp_init($den);
        }

        // Simplify
        if (gmp_cmp($den, 0) == 0) {
            throw new Exception("Division by zero");
        }

        $g = gmp_gcd($num, $den);
        $this->num = gmp_div($num, $g);
        $this->den = gmp_div($den, $g);

        // Keep denominator positive
        if (gmp_cmp($this->den, 0) < 0) {
            $this->num = gmp_neg($this->num);
            $this->den = gmp_neg($this->den);
        }
    }

    public static function zero() {
        return new Rational(0, 1);
    }

    public static function one() {
        return new Rational(1, 1);
    }

    public function add($other) {
        // a/b + c/d = (ad + bc) / bd
        $num = gmp_add(
            gmp_mul($this->num, $other->den),
            gmp_mul($other->num, $this->den)
        );
        $den = gmp_mul($this->den, $other->den);
        return new Rational($num, $den);
    }

    public function sub($other) {
        $num = gmp_sub(
            gmp_mul($this->num, $other->den),
            gmp_mul($other->num, $this->den)
        );
        $den = gmp_mul($this->den, $other->den);
        return new Rational($num, $den);
    }

    public function mul($other) {
        $num = gmp_mul($this->num, $other->num);
        $den = gmp_mul($this->den, $other->den);
        return new Rational($num, $den);
    }

    public function div($other) {
        return $this->mul(new Rational($other->den, $other->num));
    }

    public function neg() {
        return new Rational(gmp_neg($this->num), $this->den);
    }

    public function isZero() {
        return gmp_cmp($this->num, 0) == 0;
    }

    public function isNegative() {
        return gmp_cmp($this->num, 0) < 0;
    }

    public function isInteger() {
        return gmp_cmp($this->den, 1) == 0;
    }

    public function toInt() {
        if (!$this->isInteger()) {
            throw new Exception("Not an integer");
        }
        return gmp_intval($this->num);
    }

    public function toFloat() {
        return gmp_intval($this->num) / gmp_intval($this->den);
    }

    public function cmp($other) {
        // Compare a/b with c/d: compare ad with bc
        $left = gmp_mul($this->num, $other->den);
        $right = gmp_mul($other->num, $this->den);
        return gmp_cmp($left, $right);
    }
}

/**
 * Solve machine for Part 2 using Gaussian elimination and search
 */
function solve_machine_part2($n_counters, $joltage, $buttons) {
    $n_buttons = count($buttons);

    if ($n_buttons == 0) {
        $all_zero = true;
        foreach ($joltage as $j) {
            if ($j != 0) {
                $all_zero = false;
                break;
            }
        }
        return $all_zero ? 0 : PHP_INT_MAX;
    }

    // Build matrix A (n_counters x n_buttons) using Rational numbers
    $A = [];
    for ($i = 0; $i < $n_counters; $i++) {
        $A[$i] = [];
        for ($j = 0; $j < $n_buttons; $j++) {
            $A[$i][$j] = Rational::zero();
        }
    }

    for ($j = 0; $j < $n_buttons; $j++) {
        foreach ($buttons[$j] as $idx) {
            if ($idx < $n_counters) {
                $A[$idx][$j] = Rational::one();
            }
        }
    }

    // Build b vector
    $b = [];
    for ($i = 0; $i < $n_counters; $i++) {
        $b[$i] = new Rational($joltage[$i]);
    }

    // Augmented matrix [A | b]
    $aug = [];
    for ($i = 0; $i < $n_counters; $i++) {
        $aug[$i] = array_merge($A[$i], [$b[$i]]);
    }

    // Gaussian elimination with partial pivoting
    $pivot_cols = [];
    $pivot_row = 0;

    for ($col = 0; $col < $n_buttons; $col++) {
        // Find non-zero entry in this column
        $found = -1;
        for ($row = $pivot_row; $row < $n_counters; $row++) {
            if (!$aug[$row][$col]->isZero()) {
                $found = $row;
                break;
            }
        }

        if ($found === -1) {
            continue;
        }

        // Swap rows
        $temp = $aug[$pivot_row];
        $aug[$pivot_row] = $aug[$found];
        $aug[$found] = $temp;

        $pivot_cols[] = [$col, $pivot_row];

        // Scale pivot row
        $scale = $aug[$pivot_row][$col];
        for ($c = 0; $c <= $n_buttons; $c++) {
            $aug[$pivot_row][$c] = $aug[$pivot_row][$c]->div($scale);
        }

        // Eliminate column in other rows
        for ($row = 0; $row < $n_counters; $row++) {
            if ($row !== $pivot_row && !$aug[$row][$col]->isZero()) {
                $factor = $aug[$row][$col];
                for ($c = 0; $c <= $n_buttons; $c++) {
                    $aug[$row][$c] = $aug[$row][$c]->sub($factor->mul($aug[$pivot_row][$c]));
                }
            }
        }

        $pivot_row++;
    }

    // Check for inconsistency
    for ($row = $pivot_row; $row < $n_counters; $row++) {
        if (!$aug[$row][$n_buttons]->isZero()) {
            return PHP_INT_MAX;  // No solution
        }
    }

    // Identify free variables
    $pivot_col_set = [];
    foreach ($pivot_cols as $pc) {
        $pivot_col_set[$pc[0]] = true;
    }

    $free_vars = [];
    for ($c = 0; $c < $n_buttons; $c++) {
        if (!isset($pivot_col_set[$c])) {
            $free_vars[] = $c;
        }
    }

    // If no free variables, unique solution
    if (count($free_vars) == 0) {
        $solution = [];
        for ($i = 0; $i < $n_buttons; $i++) {
            $solution[$i] = Rational::zero();
        }

        foreach ($pivot_cols as $pc) {
            list($col, $row) = $pc;
            $solution[$col] = $aug[$row][$n_buttons];
        }

        $total = 0;
        foreach ($solution as $val) {
            if ($val->isNegative() || !$val->isInteger()) {
                return PHP_INT_MAX;
            }
            $total += $val->toInt();
        }
        return $total;
    }

    // Extract particular solution (with free vars = 0)
    $particular = [];
    for ($i = 0; $i < $n_buttons; $i++) {
        $particular[$i] = Rational::zero();
    }

    foreach ($pivot_cols as $pc) {
        list($col, $row) = $pc;
        $particular[$col] = $aug[$row][$n_buttons];
    }

    // Extract null space vectors
    $null_vectors = [];
    foreach ($free_vars as $fv) {
        $vec = [];
        for ($i = 0; $i < $n_buttons; $i++) {
            $vec[$i] = Rational::zero();
        }
        $vec[$fv] = Rational::one();

        foreach ($pivot_cols as $pc) {
            list($col, $row) = $pc;
            $vec[$col] = $aug[$row][$fv]->neg();
        }

        $null_vectors[] = $vec;
    }

    $n_free = count($free_vars);
    $max_j = max($joltage);

    // Search for optimal solution based on number of free variables
    if ($n_free == 1) {
        return search_1d($n_buttons, $particular, $null_vectors[0]);
    } elseif ($n_free == 2) {
        return search_2d($n_buttons, $particular, $null_vectors, $max_j);
    } elseif ($n_free == 3) {
        return search_3d($n_buttons, $particular, $null_vectors, $max_j);
    } else {
        // For higher dimensions, use simpler bounded search
        return search_nd($n_buttons, $particular, $null_vectors, $max_j);
    }
}

function search_1d($n_buttons, $particular, $null_vec) {
    $t_low = -1e9;
    $t_high = 1e9;

    for ($j = 0; $j < $n_buttons; $j++) {
        $p = $particular[$j];
        $nv = $null_vec[$j];

        if ($nv->isZero()) {
            if ($p->isNegative()) {
                return PHP_INT_MAX;
            }
        } elseif (!$nv->isNegative()) {
            // nv > 0: t >= -p/nv
            $bound = $p->neg()->div($nv)->toFloat();
            $t_low = max($t_low, $bound);
        } else {
            // nv < 0: t <= -p/nv
            $bound = $p->neg()->div($nv)->toFloat();
            $t_high = min($t_high, $bound);
        }
    }

    if ($t_low > $t_high) {
        return PHP_INT_MAX;
    }

    $t_low_int = (int)ceil($t_low);
    $t_high_int = (int)floor($t_high);

    $min_total = PHP_INT_MAX;
    for ($t = $t_low_int; $t <= $t_high_int; $t++) {
        $t_rat = new Rational($t);
        $total = 0;
        $valid = true;

        for ($j = 0; $j < $n_buttons; $j++) {
            $val = $particular[$j]->add($t_rat->mul($null_vec[$j]));
            if ($val->isNegative() || !$val->isInteger()) {
                $valid = false;
                break;
            }
            $total += $val->toInt();
        }

        if ($valid) {
            $min_total = min($min_total, $total);
        }
    }

    return $min_total;
}

function search_2d($n_buttons, $particular, $null_vectors, $max_j) {
    $bound = $max_j;
    $min_total = PHP_INT_MAX;

    for ($t0 = -$bound; $t0 <= $bound; $t0++) {
        $t0_rat = new Rational($t0);

        // Compute intermediate values
        $intermediate = [];
        for ($j = 0; $j < $n_buttons; $j++) {
            $intermediate[$j] = $particular[$j]->add($t0_rat->mul($null_vectors[0][$j]));
        }

        // Compute bounds for t1 given t0
        $t1_low = -1e9;
        $t1_high = 1e9;

        for ($j = 0; $j < $n_buttons; $j++) {
            $p = $intermediate[$j]->toFloat();
            $nv = $null_vectors[1][$j]->toFloat();

            if ($nv > 0) {
                $t1_low = max($t1_low, -$p / $nv);
            } elseif ($nv < 0) {
                $t1_high = min($t1_high, -$p / $nv);
            }
        }

        $t1_low_int = (int)ceil($t1_low);
        $t1_high_int = (int)floor($t1_high);

        for ($t1 = $t1_low_int; $t1 <= $t1_high_int; $t1++) {
            $t1_rat = new Rational($t1);
            $valid = true;
            $total = 0;

            for ($j = 0; $j < $n_buttons; $j++) {
                $val = $intermediate[$j]->add($t1_rat->mul($null_vectors[1][$j]));
                if ($val->isNegative() || !$val->isInteger()) {
                    $valid = false;
                    break;
                }
                $total += $val->toInt();
            }

            if ($valid && $total < $min_total) {
                $min_total = $total;
            }
        }
    }

    return $min_total;
}

function search_3d($n_buttons, $particular, $null_vectors, $max_j) {
    $bound = $max_j;
    $min_total = PHP_INT_MAX;

    for ($t0 = -$bound; $t0 <= $bound; $t0++) {
        $t0_rat = new Rational($t0);

        $inter0 = [];
        for ($j = 0; $j < $n_buttons; $j++) {
            $inter0[$j] = $particular[$j]->add($t0_rat->mul($null_vectors[0][$j]));
        }

        // Bounds for t1
        $t1_low = -$bound;
        $t1_high = $bound;

        for ($t1 = $t1_low; $t1 <= $t1_high; $t1++) {
            $t1_rat = new Rational($t1);

            $inter1 = [];
            for ($j = 0; $j < $n_buttons; $j++) {
                $inter1[$j] = $inter0[$j]->add($t1_rat->mul($null_vectors[1][$j]));
            }

            // Compute bounds for t2
            $t2_low = -1e9;
            $t2_high = 1e9;

            for ($j = 0; $j < $n_buttons; $j++) {
                $p = $inter1[$j]->toFloat();
                $nv = $null_vectors[2][$j]->toFloat();

                if ($nv > 0) {
                    $t2_low = max($t2_low, -$p / $nv);
                } elseif ($nv < 0) {
                    $t2_high = min($t2_high, -$p / $nv);
                }
            }

            $t2_low_int = (int)ceil($t2_low);
            $t2_high_int = (int)floor($t2_high);

            for ($t2 = $t2_low_int; $t2 <= $t2_high_int; $t2++) {
                $t2_rat = new Rational($t2);
                $valid = true;
                $total = 0;

                for ($j = 0; $j < $n_buttons; $j++) {
                    $val = $inter1[$j]->add($t2_rat->mul($null_vectors[2][$j]));
                    if ($val->isNegative() || !$val->isInteger()) {
                        $valid = false;
                        break;
                    }
                    $total += $val->toInt();
                }

                if ($valid && $total < $min_total) {
                    $min_total = $total;
                }
            }
        }
    }

    return $min_total;
}

function search_nd($n_buttons, $particular, $null_vectors, $max_j) {
    $n_free = count($null_vectors);
    $bound = $max_j;
    $min_total = PHP_INT_MAX;

    // Simple bounded search for higher dimensions
    $ranges = [];
    for ($i = 0; $i < $n_free; $i++) {
        $ranges[$i] = range(-$bound, $bound);
    }

    // Recursive search helper
    $search = function($idx, $current_values) use (
        &$search, $n_free, $n_buttons, $particular, $null_vectors, &$min_total, $ranges
    ) {
        if ($idx == $n_free) {
            // Evaluate solution
            $valid = true;
            $total = 0;

            for ($j = 0; $j < $n_buttons; $j++) {
                $val = $particular[$j];
                for ($i = 0; $i < $n_free; $i++) {
                    $t_rat = new Rational($current_values[$i]);
                    $val = $val->add($t_rat->mul($null_vectors[$i][$j]));
                }

                if ($val->isNegative() || !$val->isInteger()) {
                    $valid = false;
                    break;
                }
                $total += $val->toInt();
            }

            if ($valid) {
                $min_total = min($min_total, $total);
            }
            return;
        }

        foreach ($ranges[$idx] as $t) {
            $current_values[$idx] = $t;
            $search($idx + 1, $current_values);
        }
    };

    $search(0, []);

    return $min_total;
}

function part1($lines) {
    $total = 0;
    foreach ($lines as $line) {
        $line = trim($line);
        if (empty($line)) {
            continue;
        }

        list($n_lights, $target, $buttons) = parse_line($line);
        $min_presses = solve_machine_brute($n_lights, $target, $buttons);
        $total += $min_presses;
    }
    return $total;
}

function part2($lines) {
    $total = 0;
    foreach ($lines as $line) {
        $line = trim($line);
        if (empty($line)) {
            continue;
        }

        list($n_counters, $joltage, $buttons) = parse_line_part2($line);
        $min_presses = solve_machine_part2($n_counters, $joltage, $buttons);
        $total += $min_presses;
    }
    return $total;
}

// Main execution
$input_file = __DIR__ . '/../input.txt';
$lines = file($input_file, FILE_IGNORE_NEW_LINES);

echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
