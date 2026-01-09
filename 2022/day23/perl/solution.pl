#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# Parse elf positions from grid
sub parse_input {
    my ($text) = @_;
    my %elves;
    my @lines = split /\n/, $text;

    for my $r (0 .. $#lines) {
        my @chars = split //, $lines[$r];
        for my $c (0 .. $#chars) {
            if ($chars[$c] eq '#') {
                $elves{"$r,$c"} = 1;
            }
        }
    }
    return \%elves;
}

# Direction checks: check positions and move delta
my %dir_checks = (
    'N' => { checks => [[-1, -1], [-1, 0], [-1, 1]], delta => [-1, 0] },
    'S' => { checks => [[1, -1], [1, 0], [1, 1]], delta => [1, 0] },
    'W' => { checks => [[-1, -1], [0, -1], [1, -1]], delta => [0, -1] },
    'E' => { checks => [[-1, 1], [0, 1], [1, 1]], delta => [0, 1] },
);

# All 8 neighbors
my @all_neighbors = (
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1], [0, 1],
    [1, -1], [1, 0], [1, 1]
);

# Run one round of simulation
sub simulate_round {
    my ($elves, $directions) = @_;

    my %proposals;       # elf_key -> proposed position key
    my %proposal_counts; # position key -> count

    # Phase 1: Each elf proposes a move
    for my $elf_key (keys %$elves) {
        my ($r, $c) = split /,/, $elf_key;

        # Check if any neighbors
        my $has_neighbor = 0;
        for my $n (@all_neighbors) {
            my $neighbor_key = ($r + $n->[0]) . "," . ($c + $n->[1]);
            if (exists $elves->{$neighbor_key}) {
                $has_neighbor = 1;
                last;
            }
        }

        next unless $has_neighbor;

        # Try each direction
        for my $d (@$directions) {
            my $checks = $dir_checks{$d}{checks};
            my $delta = $dir_checks{$d}{delta};

            my $can_move = 1;
            for my $check (@$checks) {
                my $check_key = ($r + $check->[0]) . "," . ($c + $check->[1]);
                if (exists $elves->{$check_key}) {
                    $can_move = 0;
                    last;
                }
            }

            if ($can_move) {
                my $new_pos = ($r + $delta->[0]) . "," . ($c + $delta->[1]);
                $proposals{$elf_key} = $new_pos;
                $proposal_counts{$new_pos}++;
                last;
            }
        }
    }

    # Phase 2: Execute moves (only if unique proposal)
    my %new_elves;
    my $moved = 0;

    for my $elf_key (keys %$elves) {
        if (exists $proposals{$elf_key}) {
            my $new_pos = $proposals{$elf_key};
            if ($proposal_counts{$new_pos} == 1) {
                $new_elves{$new_pos} = 1;
                $moved = 1;
            } else {
                $new_elves{$elf_key} = 1;
            }
        } else {
            $new_elves{$elf_key} = 1;
        }
    }

    return (\%new_elves, $moved);
}

# Count empty tiles in bounding rectangle
sub bounding_rect_empty {
    my ($elves) = @_;

    my ($min_r, $max_r, $min_c, $max_c);
    my $first = 1;

    for my $key (keys %$elves) {
        my ($r, $c) = split /,/, $key;
        if ($first) {
            $min_r = $max_r = $r;
            $min_c = $max_c = $c;
            $first = 0;
        } else {
            $min_r = $r if $r < $min_r;
            $max_r = $r if $r > $max_r;
            $min_c = $c if $c < $min_c;
            $max_c = $c if $c > $max_c;
        }
    }

    my $area = ($max_r - $min_r + 1) * ($max_c - $min_c + 1);
    return $area - scalar(keys %$elves);
}

# Part 1: Count empty tiles after 10 rounds
sub part1 {
    my ($text) = @_;
    my $elves = parse_input($text);
    my @directions = qw(N S W E);

    for (1 .. 10) {
        ($elves, my $moved) = simulate_round($elves, \@directions);
        push @directions, shift @directions;
    }

    return bounding_rect_empty($elves);
}

# Part 2: Find first round where no elf moves
sub part2 {
    my ($text) = @_;
    my $elves = parse_input($text);
    my @directions = qw(N S W E);

    my $round_num = 0;
    while (1) {
        $round_num++;
        my $moved;
        ($elves, $moved) = simulate_round($elves, \@directions);
        return $round_num unless $moved;
        push @directions, shift @directions;
    }
}

# Main
my $script_dir = dirname(File::Spec->rel2abs(__FILE__));
my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $text = do { local $/; <$fh> };
close $fh;

# Remove trailing newline
chomp $text;

print "Part 1: ", part1($text), "\n";
print "Part 2: ", part2($text), "\n";
