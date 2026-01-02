#!/usr/bin/env perl
# Day 20: Pulse Propagation - Module communication simulation

use strict;
use warnings;
use File::Basename;
use List::Util qw(all);

# GCD and LCM functions
sub gcd {
    my ($a, $b) = @_;
    while ($b) {
        ($a, $b) = ($b, $a % $b);
    }
    return $a;
}

sub lcm {
    my ($a, $b) = @_;
    return ($a * $b) / gcd($a, $b);
}

sub parse_input {
    my ($filename) = @_;
    my %modules;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        next unless $line;

        my ($name_part, $dest_part) = split /\s*->\s*/, $line;
        my @destinations = map { s/^\s+|\s+$//gr } split /,/, $dest_part;

        if ($name_part eq 'broadcaster') {
            $modules{broadcaster} = {
                type => 'broadcaster',
                destinations => \@destinations
            };
        } elsif ($name_part =~ /^%(.+)/) {
            my $name = $1;
            $modules{$name} = {
                type => 'flip-flop',
                destinations => \@destinations,
                state => 0  # off
            };
        } elsif ($name_part =~ /^&(.+)/) {
            my $name = $1;
            $modules{$name} = {
                type => 'conjunction',
                destinations => \@destinations,
                memory => {}
            };
        }
    }
    close $fh;

    # Initialize conjunction memory for all inputs
    for my $name (keys %modules) {
        my $module = $modules{$name};
        for my $dest (@{$module->{destinations}}) {
            if (exists $modules{$dest} && $modules{$dest}{type} eq 'conjunction') {
                $modules{$dest}{memory}{$name} = 0;  # 0 = low pulse
            }
        }
    }

    return \%modules;
}

sub reset_state {
    my ($modules) = @_;
    for my $module (values %$modules) {
        if ($module->{type} eq 'flip-flop') {
            $module->{state} = 0;
        } elsif ($module->{type} eq 'conjunction') {
            for my $key (keys %{$module->{memory}}) {
                $module->{memory}{$key} = 0;
            }
        }
    }
}

sub simulate_button_press {
    my ($modules, $watch_nodes) = @_;
    $watch_nodes //= {};

    my $low_count = 0;
    my $high_count = 0;
    my %high_senders;

    # Queue: [source, destination, pulse] where pulse is 1 for high, 0 for low
    my @queue = (['button', 'broadcaster', 0]);

    while (@queue) {
        my $item = shift @queue;
        my ($source, $dest, $pulse) = @$item;

        if ($pulse) {
            $high_count++;
        } else {
            $low_count++;
        }

        # Track if watched nodes send high pulses
        if (exists $watch_nodes->{$source} && $pulse) {
            $high_senders{$source} = 1;
        }

        next unless exists $modules->{$dest};

        my $module = $modules->{$dest};

        if ($module->{type} eq 'broadcaster') {
            for my $next_dest (@{$module->{destinations}}) {
                push @queue, [$dest, $next_dest, $pulse];
            }
        } elsif ($module->{type} eq 'flip-flop') {
            if (!$pulse) {  # Only react to low pulses
                $module->{state} = !$module->{state};
                my $new_pulse = $module->{state} ? 1 : 0;
                for my $next_dest (@{$module->{destinations}}) {
                    push @queue, [$dest, $next_dest, $new_pulse];
                }
            }
        } elsif ($module->{type} eq 'conjunction') {
            $module->{memory}{$source} = $pulse;
            # Send low if all inputs are high, otherwise send high
            my $all_high = all { $_ } values %{$module->{memory}};
            my $output = $all_high ? 0 : 1;
            for my $next_dest (@{$module->{destinations}}) {
                push @queue, [$dest, $next_dest, $output];
            }
        }
    }

    return ($low_count, $high_count, \%high_senders);
}

sub part1 {
    my ($modules) = @_;
    reset_state($modules);

    my $total_low = 0;
    my $total_high = 0;

    for (1..1000) {
        my ($low, $high, undef) = simulate_button_press($modules);
        $total_low += $low;
        $total_high += $high;
    }

    return $total_low * $total_high;
}

sub part2 {
    my ($modules) = @_;
    reset_state($modules);

    # Find the module that feeds into rx
    my $rx_input;
    for my $name (keys %$modules) {
        my $module = $modules->{$name};
        for my $dest (@{$module->{destinations}}) {
            if ($dest eq 'rx') {
                $rx_input = $name;
                last;
            }
        }
        last if defined $rx_input;
    }

    return 0 unless defined $rx_input;

    # Find all modules that feed into rx_input
    my %watch_nodes = map { $_ => 1 } keys %{$modules->{$rx_input}{memory}};
    my %cycle_lengths;

    my $button_press = 0;
    while (scalar(keys %cycle_lengths) < scalar(keys %watch_nodes)) {
        $button_press++;
        my (undef, undef, $high_senders) = simulate_button_press($modules, \%watch_nodes);

        for my $node (keys %$high_senders) {
            unless (exists $cycle_lengths{$node}) {
                $cycle_lengths{$node} = $button_press;
            }
        }
    }

    # LCM of all cycle lengths
    my $result = 1;
    for my $length (values %cycle_lengths) {
        $result = lcm($result, $length);
    }

    return $result;
}

sub main {
    my $dir = dirname(__FILE__);
    my $input_file = "$dir/../input.txt";

    my $modules = parse_input($input_file);
    print "Part 1: ", part1($modules), "\n";

    # Re-parse for part 2 (fresh state)
    $modules = parse_input($input_file);
    print "Part 2: ", part2($modules), "\n";
}

main();
