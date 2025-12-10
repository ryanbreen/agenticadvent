#!/usr/bin/env perl
use strict;
use warnings;
use POSIX qw(ceil floor);

# Simple GCD function
sub gcd {
    my ($a, $b) = @_;
    $a = abs($a);
    $b = abs($b);
    while ($b) {
        ($a, $b) = ($b, $a % $b);
    }
    return $a;
}

# Fraction package - lightweight rational arithmetic
package Fraction {
    sub new {
        my ($class, $num, $den) = @_;
        $den ||= 1;
        return bless { n => 0, d => 1 }, $class if $num == 0;
        my $g = main::gcd($num, $den);
        my $sign = ($num < 0) != ($den < 0) ? -1 : 1;
        return bless { n => $sign * abs($num) / $g, d => abs($den) / $g }, $class;
    }

    sub add {
        my ($self, $other) = @_;
        return Fraction->new($self->{n} * $other->{d} + $other->{n} * $self->{d}, $self->{d} * $other->{d});
    }

    sub sub {
        my ($self, $other) = @_;
        return Fraction->new($self->{n} * $other->{d} - $other->{n} * $self->{d}, $self->{d} * $other->{d});
    }

    sub mul {
        my ($self, $other) = @_;
        return Fraction->new($self->{n} * $other->{n}, $self->{d} * $other->{d});
    }

    sub div {
        my ($self, $other) = @_;
        return Fraction->new($self->{n} * $other->{d}, $self->{d} * $other->{n});
    }

    sub neg {
        my ($self) = @_;
        return Fraction->new(-$self->{n}, $self->{d});
    }

    sub is_zero {
        my ($self) = @_;
        return $self->{n} == 0;
    }

    sub is_negative {
        my ($self) = @_;
        return $self->{n} < 0;
    }

    sub is_integer {
        my ($self) = @_;
        return $self->{d} == 1;
    }

    sub to_int {
        my ($self) = @_;
        return int($self->{n} / $self->{d});
    }

    sub to_number {
        my ($self) = @_;
        return $self->{n} / $self->{d};
    }

    sub cmp {
        my ($self, $other) = @_;
        return ($self->{n} * $other->{d}) <=> ($other->{n} * $self->{d});
    }
}

package main;

# Part 1: Parse a machine line into target state and button masks
sub parse_line {
    my ($line) = @_;
    my ($indicator) = $line =~ /\[([.#]+)\]/;
    my $n_lights = length($indicator);
    my $target = 0;
    for my $i (0 .. length($indicator) - 1) {
        if (substr($indicator, $i, 1) eq '#') {
            $target |= (1 << $i);
        }
    }
    my @buttons;
    while ($line =~ /\(([0-9,]+)\)/g) {
        my @indices = split /,/, $1;
        my $mask = 0;
        foreach my $idx (@indices) {
            $mask |= (1 << $idx);
        }
        push @buttons, $mask;
    }
    return ($n_lights, $target, \@buttons);
}

# Part 2: Parse a machine line for joltage requirements
sub parse_line_part2 {
    my ($line) = @_;
    my ($joltage_str) = $line =~ /\{([0-9,]+)\}/;
    my @joltage = split /,/, $joltage_str;
    my $n_counters = scalar @joltage;
    my @buttons;
    while ($line =~ /\(([0-9,]+)\)/g) {
        my @indices = split /,/, $1;
        push @buttons, \@indices;
    }
    return ($n_counters, \@joltage, \@buttons);
}

# Brute force: try all combinations of button presses
sub solve_machine_brute {
    my ($n_lights, $target, $buttons) = @_;
    my $n_buttons = scalar @$buttons;
    my $min_presses = 1e9;
    for my $mask (0 .. (1 << $n_buttons) - 1) {
        my $state = 0;
        my $presses = 0;
        for my $i (0 .. $n_buttons - 1) {
            if ($mask & (1 << $i)) {
                $state ^= $buttons->[$i];
                $presses++;
            }
        }
        if ($state == $target) {
            $min_presses = $presses if $presses < $min_presses;
        }
    }
    return $min_presses == 1e9 ? 0 : $min_presses;
}

# Part 1: Solve a single machine
sub solve_machine {
    my ($n_lights, $target, $buttons) = @_;
    return solve_machine_brute($n_lights, $target, $buttons);
}

# Part 1: Find minimum total button presses for all machines
sub part1 {
    my ($lines) = @_;
    my $total = 0;
    foreach my $line (@$lines) {
        $line =~ s/^\s+|\s+$//g;
        next if $line eq '';
        my ($n_lights, $target, $buttons) = parse_line($line);
        my $min_presses = solve_machine($n_lights, $target, $buttons);
        $total += $min_presses;
    }
    return $total;
}

# Gaussian elimination to get RREF form
sub gaussian_elimination {
    my ($aug, $n_rows, $n_cols) = @_;
    my @pivot_cols;
    my $pivot_row = 0;

    for my $col (0 .. $n_cols - 1) {
        my $found = -1;
        for my $row ($pivot_row .. $n_rows - 1) {
            if (!$aug->[$row][$col]->is_zero()) {
                $found = $row;
                last;
            }
        }
        next if $found == -1;

        if ($found != $pivot_row) {
            my $tmp = $aug->[$pivot_row];
            $aug->[$pivot_row] = $aug->[$found];
            $aug->[$found] = $tmp;
        }

        push @pivot_cols, [$col, $pivot_row];

        my $scale = $aug->[$pivot_row][$col];
        for my $c (0 .. $n_cols) {
            $aug->[$pivot_row][$c] = $aug->[$pivot_row][$c]->div($scale);
        }

        for my $row (0 .. $n_rows - 1) {
            next if $row == $pivot_row;
            if (!$aug->[$row][$col]->is_zero()) {
                my $factor = $aug->[$row][$col];
                for my $c (0 .. $n_cols) {
                    $aug->[$row][$c] = $aug->[$row][$c]->sub($factor->mul($aug->[$pivot_row][$c]));
                }
            }
        }
        $pivot_row++;
    }
    return \@pivot_cols;
}

# Part 2: Solve a single machine using Gaussian elimination
sub solve_machine_part2 {
    my ($n_counters, $joltage, $buttons) = @_;
    my $n_buttons = scalar @$buttons;
    return 0 if $n_buttons == 0;

    # Build matrix A (n_counters x n_buttons)
    my @A;
    for my $i (0 .. $n_counters - 1) {
        my @row;
        for my $j (0 .. $n_buttons - 1) {
            push @row, Fraction->new(0);
        }
        push @A, \@row;
    }

    for my $j (0 .. $n_buttons - 1) {
        foreach my $idx (@{$buttons->[$j]}) {
            if ($idx < $n_counters) {
                $A[$idx][$j] = Fraction->new(1);
            }
        }
    }

    my @b;
    for my $j (@$joltage) {
        push @b, Fraction->new($j);
    }

    # Augmented matrix [A | b]
    my @aug;
    for my $i (0 .. $n_counters - 1) {
        my @row = (@{$A[$i]}, $b[$i]);
        push @aug, \@row;
    }

    # Gaussian elimination
    my $pivot_cols = gaussian_elimination(\@aug, $n_counters, $n_buttons);

    # Check for inconsistency
    my $pivot_row_count = scalar @$pivot_cols;
    for my $row ($pivot_row_count .. $n_counters - 1) {
        if (!$aug[$row][$n_buttons]->is_zero()) {
            return 1e9;
        }
    }

    # Identify free variables
    my %pivot_col_set;
    foreach my $pc (@$pivot_cols) {
        $pivot_col_set{$pc->[0]} = 1;
    }

    my @free_vars;
    for my $c (0 .. $n_buttons - 1) {
        push @free_vars, $c unless exists $pivot_col_set{$c};
    }
    my $n_free = scalar @free_vars;

    # Extract particular solution
    my @particular;
    for my $j (0 .. $n_buttons - 1) {
        $particular[$j] = Fraction->new(0);
    }
    foreach my $pc (@$pivot_cols) {
        my ($col, $row) = @$pc;
        $particular[$col] = $aug[$row][$n_buttons];
    }

    # If no free variables, unique solution
    if ($n_free == 0) {
        my $total = 0;
        for my $val (@particular) {
            return 1e9 if $val->is_negative() || !$val->is_integer();
            $total += $val->to_int();
        }
        return $total;
    }

    # Extract null space vectors
    my @null_vectors;
    for my $fv (@free_vars) {
        my @vec;
        for my $j (0 .. $n_buttons - 1) {
            $vec[$j] = Fraction->new(0);
        }
        $vec[$fv] = Fraction->new(1);
        foreach my $pc (@$pivot_cols) {
            my ($col, $row) = @$pc;
            $vec[$col] = $aug[$row][$fv]->neg();
        }
        push @null_vectors, \@vec;
    }

    # Search for optimal non-negative integer solution
    my $max_j = 0;
    foreach my $j (@$joltage) {
        $max_j = $j if $j > $max_j;
    }
    my $min_total = 1e9;

    if ($n_free == 1) {
        my $t_low = -1e9;
        my $t_high = 1e9;
        for my $j (0 .. $n_buttons - 1) {
            my $p = $particular[$j]->to_number();
            my $nv = $null_vectors[0][$j]->to_number();
            if ($nv == 0) {
                return 1e9 if $p < 0;
            } elsif ($nv > 0) {
                $t_low = -$p / $nv if -$p / $nv > $t_low;
            } else {
                $t_high = -$p / $nv if -$p / $nv < $t_high;
            }
        }
        return 1e9 if $t_low > $t_high;

        my $t_low_int = ceil($t_low);
        my $t_high_int = floor($t_high);

        for my $t ($t_low_int .. $t_high_int) {
            my $t_frac = Fraction->new($t);
            my $total = 0;
            my $valid = 1;
            for my $j (0 .. $n_buttons - 1) {
                my $val = $particular[$j]->add($t_frac->mul($null_vectors[0][$j]));
                if ($val->is_negative() || !$val->is_integer()) {
                    $valid = 0;
                    last;
                }
                $total += $val->to_int();
            }
            if ($valid && $total < $min_total) {
                $min_total = $total;
            }
        }
        return $min_total == 1e9 ? 0 : $min_total;
    }

    if ($n_free == 2) {
        # Compute bounds for t0
        my $t0_low = -1e9;
        my $t0_high = 1e9;
        for my $j (0 .. $n_buttons - 1) {
            my $p = $particular[$j]->to_number();
            my $nv = $null_vectors[0][$j]->to_number();
            if ($nv > 0) {
                $t0_low = -$p / $nv if -$p / $nv > $t0_low;
            } elsif ($nv < 0) {
                $t0_high = -$p / $nv if -$p / $nv < $t0_high;
            }
        }
        $t0_low = floor($t0_low - $max_j);
        $t0_low = -$max_j * 2 if $t0_low < -$max_j * 2;
        $t0_high = ceil($t0_high + $max_j);
        $t0_high = $max_j * 2 if $t0_high > $max_j * 2;

        for my $t0 ($t0_low .. $t0_high) {
            my $t0_frac = Fraction->new($t0);
            my @intermediate;
            for my $j (0 .. $n_buttons - 1) {
                $intermediate[$j] = $particular[$j]->add($t0_frac->mul($null_vectors[0][$j]));
            }

            my $t1_low = -1e9;
            my $t1_high = 1e9;
            for my $j (0 .. $n_buttons - 1) {
                my $p = $intermediate[$j]->to_number();
                my $nv = $null_vectors[1][$j]->to_number();
                if ($nv > 0) {
                    $t1_low = -$p / $nv if -$p / $nv > $t1_low;
                } elsif ($nv < 0) {
                    $t1_high = -$p / $nv if -$p / $nv < $t1_high;
                }
            }

            my $t1_low_int = ceil($t1_low);
            my $t1_high_int = floor($t1_high);

            for my $t1 ($t1_low_int .. $t1_high_int) {
                my $t1_frac = Fraction->new($t1);
                my $valid = 1;
                my $total = 0;
                for my $j (0 .. $n_buttons - 1) {
                    my $val = $intermediate[$j]->add($t1_frac->mul($null_vectors[1][$j]));
                    if ($val->is_negative() || !$val->is_integer()) {
                        $valid = 0;
                        last;
                    }
                    $total += $val->to_int();
                }
                if ($valid && $total < $min_total) {
                    $min_total = $total;
                }
            }
        }
        return $min_total == 1e9 ? 0 : $min_total;
    }

    if ($n_free == 3) {
        my $bound = $max_j;
        for my $t0 (-$bound .. $bound) {
            my $t0_frac = Fraction->new($t0);
            my @inter0;
            for my $j (0 .. $n_buttons - 1) {
                $inter0[$j] = $particular[$j]->add($t0_frac->mul($null_vectors[0][$j]));
            }

            my $t1_low = -1e9;
            my $t1_high = 1e9;
            for my $j (0 .. $n_buttons - 1) {
                my $p = $inter0[$j]->to_number();
                my $nv = $null_vectors[1][$j]->to_number();
                if ($nv > 0) {
                    my $val = -$p / $nv - $bound;
                    $t1_low = $val if $val > $t1_low;
                } elsif ($nv < 0) {
                    my $val = -$p / $nv + $bound;
                    $t1_high = $val if $val < $t1_high;
                }
            }

            my $t1_low_int = ceil($t1_low);
            $t1_low_int = -$bound if $t1_low_int < -$bound;
            my $t1_high_int = floor($t1_high);
            $t1_high_int = $bound if $t1_high_int > $bound;

            for my $t1 ($t1_low_int .. $t1_high_int) {
                my $t1_frac = Fraction->new($t1);
                my @inter1;
                for my $j (0 .. $n_buttons - 1) {
                    $inter1[$j] = $inter0[$j]->add($t1_frac->mul($null_vectors[1][$j]));
                }

                my $t2_low = -1e9;
                my $t2_high = 1e9;
                for my $j (0 .. $n_buttons - 1) {
                    my $p = $inter1[$j]->to_number();
                    my $nv = $null_vectors[2][$j]->to_number();
                    if ($nv > 0) {
                        my $val = -$p / $nv;
                        $t2_low = $val if $val > $t2_low;
                    } elsif ($nv < 0) {
                        my $val = -$p / $nv;
                        $t2_high = $val if $val < $t2_high;
                    }
                }

                my $t2_low_int = ceil($t2_low);
                my $t2_high_int = floor($t2_high);

                for my $t2 ($t2_low_int .. $t2_high_int) {
                    my $t2_frac = Fraction->new($t2);
                    my $valid = 1;
                    my $total = 0;
                    for my $j (0 .. $n_buttons - 1) {
                        my $val = $inter1[$j]->add($t2_frac->mul($null_vectors[2][$j]));
                        if ($val->is_negative() || !$val->is_integer()) {
                            $valid = 0;
                            last;
                        }
                        $total += $val->to_int();
                    }
                    if ($valid && $total < $min_total) {
                        $min_total = $total;
                    }
                }
            }
        }
        return $min_total == 1e9 ? 0 : $min_total;
    }

    if ($n_free >= 4 && $n_free <= 6) {
        # General recursive search with dynamic bounds for 4-6 free variables
        my $search_recursive;
        $search_recursive = sub {
            my ($idx, $partial_ref) = @_;

            if ($idx == $n_free) {
                # Evaluate solution
                my $valid = 1;
                my $total = 0;
                for my $val (@$partial_ref) {
                    if ($val->is_negative() || !$val->is_integer()) {
                        $valid = 0;
                        last;
                    }
                    $total += $val->to_int();
                }
                if ($valid) {
                    $min_total = $total if $total < $min_total;
                }
                return;
            }

            # Compute bounds for current free var given partial state
            my $t_low = -1e9;
            my $t_high = 1e9;
            for my $j (0 .. $n_buttons - 1) {
                my $p = $partial_ref->[$j]->to_number();
                my $nv = $null_vectors[$idx][$j]->to_number();
                if ($nv > 0) {
                    my $val = -$p / $nv;
                    $t_low = $val if $val > $t_low;
                } elsif ($nv < 0) {
                    my $val = -$p / $nv;
                    $t_high = $val if $val < $t_high;
                }
            }

            if ($t_low > $t_high || $t_low == 1e9 || $t_high == -1e9) {
                return;
            }

            # Widen bounds to account for integrality constraints
            my $t_low_int = ceil($t_low) - $max_j;
            $t_low_int = -$max_j * 2 if $t_low_int < -$max_j * 2;
            my $t_high_int = floor($t_high) + $max_j;
            $t_high_int = $max_j * 2 if $t_high_int > $max_j * 2;

            for my $t ($t_low_int .. $t_high_int) {
                my $t_frac = Fraction->new($t);
                my @new_partial;
                for my $j (0 .. $n_buttons - 1) {
                    $new_partial[$j] = $partial_ref->[$j]->add($t_frac->mul($null_vectors[$idx][$j]));
                }
                $search_recursive->($idx + 1, \@new_partial);
            }
        };

        $search_recursive->(0, [@particular]);
        return $min_total == 1e9 ? 0 : $min_total;
    }

    return 0;
}

# Part 2: Find minimum total button presses for joltage configuration
sub part2 {
    my ($lines) = @_;
    my $total = 0;
    foreach my $line (@$lines) {
        $line =~ s/^\s+|\s+$//g;
        next if $line eq '';
        my ($n_counters, $joltage, $buttons) = parse_line_part2($line);
        my $min_presses = solve_machine_part2($n_counters, $joltage, $buttons);
        $total += $min_presses;
    }
    return $total;
}

# Main
sub main {
    my $input_file = '../input.txt';
    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my @lines = <$fh>;
    close $fh;

    print "Part 1: " . part1(\@lines) . "\n";
    print "Part 2: " . part2(\@lines) . "\n";
}

main();
