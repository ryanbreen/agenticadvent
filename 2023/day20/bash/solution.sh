#!/opt/homebrew/bin/bash
# Day 20: Pulse Propagation - Module communication simulation

INPUT_FILE="${1:-../input.txt}"

# We use associative arrays for storing module data
# For this problem, we need to manage a complex graph structure
# Bash associative arrays have limitations, so we use careful encoding

declare -A module_type       # name -> type (B=broadcaster, F=flip-flop, C=conjunction)
declare -A module_dests      # name -> comma-separated destinations
declare -A flipflop_state    # name -> 0 or 1 (off/on)
declare -A conj_memory       # "name:input" -> 0 or 1

# Queue for BFS pulse processing - we'll use arrays as a simple queue
declare -a queue_src
declare -a queue_dst
declare -a queue_pulse

# Parse input
parse_input() {
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue

        # Split on " -> "
        name_part="${line%% -> *}"
        dest_part="${line#* -> }"

        # Remove spaces from destinations
        destinations=$(echo "$dest_part" | tr -d ' ')

        if [[ "$name_part" == "broadcaster" ]]; then
            module_type["broadcaster"]="B"
            module_dests["broadcaster"]="$destinations"
        elif [[ "$name_part" == %* ]]; then
            name="${name_part:1}"
            module_type["$name"]="F"
            module_dests["$name"]="$destinations"
            flipflop_state["$name"]=0
        elif [[ "$name_part" == \&* ]]; then
            name="${name_part:1}"
            module_type["$name"]="C"
            module_dests["$name"]="$destinations"
        fi
    done < "$INPUT_FILE"

    # Initialize conjunction memory for all inputs
    for name in "${!module_type[@]}"; do
        IFS=',' read -ra dests <<< "${module_dests[$name]}"
        for dest in "${dests[@]}"; do
            if [[ "${module_type[$dest]}" == "C" ]]; then
                conj_memory["$dest:$name"]=0
            fi
        done
    done
}

# Get all inputs to a conjunction module
get_conj_inputs() {
    local conj_name="$1"
    local inputs=""
    for key in "${!conj_memory[@]}"; do
        if [[ "$key" == "$conj_name:"* ]]; then
            local input="${key#*:}"
            if [[ -z "$inputs" ]]; then
                inputs="$input"
            else
                inputs="$inputs,$input"
            fi
        fi
    done
    echo "$inputs"
}

# Check if all conjunction inputs are high
all_conj_inputs_high() {
    local conj_name="$1"
    for key in "${!conj_memory[@]}"; do
        if [[ "$key" == "$conj_name:"* ]]; then
            if [[ "${conj_memory[$key]}" == "0" ]]; then
                return 1
            fi
        fi
    done
    return 0
}

# Reset state for fresh simulation
reset_state() {
    for name in "${!flipflop_state[@]}"; do
        flipflop_state["$name"]=0
    done
    for key in "${!conj_memory[@]}"; do
        conj_memory["$key"]=0
    done
}

# Simulate a single button press
# Arguments: watch_nodes (comma-separated list to watch for high pulses)
# Sets: low_count, high_count, high_senders (space-separated)
simulate_button_press() {
    local watch_nodes="$1"

    low_count=0
    high_count=0
    high_senders=""

    # Initialize queue with button -> broadcaster (low pulse)
    queue_src=("button")
    queue_dst=("broadcaster")
    queue_pulse=(0)

    local head=0

    while [[ $head -lt ${#queue_src[@]} ]]; do
        local src="${queue_src[$head]}"
        local dst="${queue_dst[$head]}"
        local pulse="${queue_pulse[$head]}"
        ((head++))

        if [[ "$pulse" == "1" ]]; then
            ((high_count++))
        else
            ((low_count++))
        fi

        # Track if watched nodes send high pulses
        if [[ -n "$watch_nodes" && "$pulse" == "1" ]]; then
            if [[ ",$watch_nodes," == *",$src,"* ]]; then
                if [[ -z "$high_senders" ]]; then
                    high_senders="$src"
                elif [[ " $high_senders " != *" $src "* ]]; then
                    high_senders="$high_senders $src"
                fi
            fi
        fi

        # Skip if destination module doesn't exist
        [[ -z "${module_type[$dst]}" ]] && continue

        local mtype="${module_type[$dst]}"

        if [[ "$mtype" == "B" ]]; then
            # Broadcaster: send same pulse to all destinations
            IFS=',' read -ra dests <<< "${module_dests[$dst]}"
            for next_dst in "${dests[@]}"; do
                queue_src+=("$dst")
                queue_dst+=("$next_dst")
                queue_pulse+=("$pulse")
            done

        elif [[ "$mtype" == "F" ]]; then
            # Flip-flop: only react to low pulses
            if [[ "$pulse" == "0" ]]; then
                if [[ "${flipflop_state[$dst]}" == "0" ]]; then
                    flipflop_state["$dst"]=1
                    local out_pulse=1
                else
                    flipflop_state["$dst"]=0
                    local out_pulse=0
                fi
                IFS=',' read -ra dests <<< "${module_dests[$dst]}"
                for next_dst in "${dests[@]}"; do
                    queue_src+=("$dst")
                    queue_dst+=("$next_dst")
                    queue_pulse+=("$out_pulse")
                done
            fi

        elif [[ "$mtype" == "C" ]]; then
            # Conjunction: update memory, then check all inputs
            conj_memory["$dst:$src"]="$pulse"

            local out_pulse=0
            if all_conj_inputs_high "$dst"; then
                out_pulse=0
            else
                out_pulse=1
            fi

            IFS=',' read -ra dests <<< "${module_dests[$dst]}"
            for next_dst in "${dests[@]}"; do
                queue_src+=("$dst")
                queue_dst+=("$next_dst")
                queue_pulse+=("$out_pulse")
            done
        fi
    done
}

# Part 1: Count pulses after 1000 button presses
part1() {
    reset_state

    local total_low=0
    local total_high=0

    for ((i = 0; i < 1000; i++)); do
        simulate_button_press ""
        ((total_low += low_count))
        ((total_high += high_count))
    done

    echo $((total_low * total_high))
}

# GCD function for LCM calculation
gcd() {
    local a="$1"
    local b="$2"
    while [[ "$b" != "0" ]]; do
        local t="$b"
        b=$(echo "$a % $b" | bc)
        a="$t"
    done
    echo "$a"
}

# LCM function using bc for large numbers
lcm() {
    local a="$1"
    local b="$2"
    local g=$(gcd "$a" "$b")
    echo "($a * $b) / $g" | bc
}

# Part 2: Find when rx receives a low pulse
part2() {
    reset_state

    # Find the module that feeds into rx
    local rx_input=""
    for name in "${!module_type[@]}"; do
        if [[ ",${module_dests[$name]}," == *",rx,"* ]]; then
            rx_input="$name"
            break
        fi
    done

    if [[ -z "$rx_input" ]]; then
        echo "0"
        return
    fi

    # Find all modules that feed into rx_input (watch nodes)
    local watch_nodes=""
    for key in "${!conj_memory[@]}"; do
        if [[ "$key" == "$rx_input:"* ]]; then
            local input="${key#*:}"
            if [[ -z "$watch_nodes" ]]; then
                watch_nodes="$input"
            else
                watch_nodes="$watch_nodes,$input"
            fi
        fi
    done

    # Count how many we need to find
    local num_watch=0
    IFS=',' read -ra watch_arr <<< "$watch_nodes"
    num_watch=${#watch_arr[@]}

    declare -A cycle_lengths
    local found=0
    local button_press=0

    while [[ $found -lt $num_watch ]]; do
        ((button_press++))
        simulate_button_press "$watch_nodes"

        for node in $high_senders; do
            if [[ -z "${cycle_lengths[$node]}" ]]; then
                cycle_lengths["$node"]=$button_press
                ((found++))
            fi
        done
    done

    # Compute LCM of all cycle lengths
    local result=1
    for len in "${cycle_lengths[@]}"; do
        result=$(lcm "$result" "$len")
    done

    echo "$result"
}

# Main
parse_input

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
