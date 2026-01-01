#!/usr/bin/env bash
# Day 19: Aplenty - Workflow processing and range analysis

set -euo pipefail

INPUT_FILE="${1:-../input.txt}"

# Associative array to store workflows
# Format: workflow_name -> "rule1|rule2|...|default"
# Each rule: "attr:op:value:dest" or "::default:dest" for default rules
declare -A WORKFLOWS
declare -a PARTS

# Parse input file
parse_input() {
    local in_parts=0

    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && { in_parts=1; continue; }

        if [[ $in_parts -eq 0 ]]; then
            # Parse workflow: name{rules...}
            local name="${line%%\{*}"
            local rules_str="${line#*\{}"
            rules_str="${rules_str%\}}"

            local parsed_rules=""
            IFS=',' read -ra rules <<< "$rules_str"
            for rule in "${rules[@]}"; do
                if [[ "$rule" == *:* ]]; then
                    # Conditional rule: attr<>value:dest
                    local condition="${rule%%:*}"
                    local dest="${rule#*:}"
                    local attr="${condition:0:1}"
                    local op="${condition:1:1}"
                    local value="${condition:2}"
                    parsed_rules+="${attr}:${op}:${value}:${dest}|"
                else
                    # Default rule
                    parsed_rules+=":::${rule}|"
                fi
            done
            WORKFLOWS["$name"]="${parsed_rules%|}"
        else
            # Parse part: {x=...,m=...,a=...,s=...}
            PARTS+=("$line")
        fi
    done < "$INPUT_FILE"
}

# Process a single part through workflows
# Returns 0 if accepted, 1 if rejected
process_part() {
    local x="$1" m="$2" a="$3" s="$4"
    local current="in"

    while [[ "$current" != "A" && "$current" != "R" ]]; do
        local rules="${WORKFLOWS[$current]}"
        IFS='|' read -ra rule_array <<< "$rules"

        for rule in "${rule_array[@]}"; do
            IFS=':' read -r attr op value dest <<< "$rule"

            if [[ -z "$attr" ]]; then
                # Default rule
                current="$dest"
                break
            fi

            # Get the value of the attribute
            local part_val
            case "$attr" in
                x) part_val="$x" ;;
                m) part_val="$m" ;;
                a) part_val="$a" ;;
                s) part_val="$s" ;;
            esac

            # Check condition
            local matches=0
            if [[ "$op" == "<" && "$part_val" -lt "$value" ]]; then
                matches=1
            elif [[ "$op" == ">" && "$part_val" -gt "$value" ]]; then
                matches=1
            fi

            if [[ $matches -eq 1 ]]; then
                current="$dest"
                break
            fi
        done
    done

    [[ "$current" == "A" ]]
}

# Part 1: Sum ratings of accepted parts
part1() {
    local total=0

    for part in "${PARTS[@]}"; do
        # Parse {x=...,m=...,a=...,s=...}
        part="${part#\{}"
        part="${part%\}}"

        local x m a s
        IFS=',' read -ra attrs <<< "$part"
        for attr in "${attrs[@]}"; do
            local name="${attr%%=*}"
            local val="${attr#*=}"
            case "$name" in
                x) x="$val" ;;
                m) m="$val" ;;
                a) a="$val" ;;
                s) s="$val" ;;
            esac
        done

        if process_part "$x" "$m" "$a" "$s"; then
            total=$((total + x + m + a + s))
        fi
    done

    echo "$total"
}

# Part 2: Count all accepted combinations using range splitting
# Ranges are passed as: x_lo x_hi m_lo m_hi a_lo a_hi s_lo s_hi
count_accepted() {
    local workflow="$1"
    local x_lo="$2" x_hi="$3"
    local m_lo="$4" m_hi="$5"
    local a_lo="$6" a_hi="$7"
    local s_lo="$8" s_hi="$9"

    if [[ "$workflow" == "R" ]]; then
        echo "0"
        return
    fi

    if [[ "$workflow" == "A" ]]; then
        # Count combinations: (x_hi - x_lo + 1) * (m_hi - m_lo + 1) * ...
        echo "($x_hi - $x_lo + 1) * ($m_hi - $m_lo + 1) * ($a_hi - $a_lo + 1) * ($s_hi - $s_lo + 1)" | bc
        return
    fi

    local total=0
    local rules="${WORKFLOWS[$workflow]}"
    IFS='|' read -ra rule_array <<< "$rules"

    for rule in "${rule_array[@]}"; do
        IFS=':' read -r attr op value dest <<< "$rule"

        if [[ -z "$attr" ]]; then
            # Default rule - all remaining ranges go to dest
            local sub_result
            sub_result=$(count_accepted "$dest" "$x_lo" "$x_hi" "$m_lo" "$m_hi" "$a_lo" "$a_hi" "$s_lo" "$s_hi")
            total=$(echo "$total + $sub_result" | bc)
        else
            # Get current range for this attribute
            local lo hi
            case "$attr" in
                x) lo="$x_lo"; hi="$x_hi" ;;
                m) lo="$m_lo"; hi="$m_hi" ;;
                a) lo="$a_lo"; hi="$a_hi" ;;
                s) lo="$s_lo"; hi="$s_hi" ;;
            esac

            if [[ "$op" == "<" ]]; then
                # Split: [lo, value-1] goes to dest, [value, hi] continues
                if [[ "$lo" -lt "$value" ]]; then
                    # Part that matches goes to destination
                    local new_hi=$((value - 1))
                    [[ "$new_hi" -gt "$hi" ]] && new_hi="$hi"

                    local nx_lo="$x_lo" nx_hi="$x_hi"
                    local nm_lo="$m_lo" nm_hi="$m_hi"
                    local na_lo="$a_lo" na_hi="$a_hi"
                    local ns_lo="$s_lo" ns_hi="$s_hi"

                    case "$attr" in
                        x) nx_lo="$lo"; nx_hi="$new_hi" ;;
                        m) nm_lo="$lo"; nm_hi="$new_hi" ;;
                        a) na_lo="$lo"; na_hi="$new_hi" ;;
                        s) ns_lo="$lo"; ns_hi="$new_hi" ;;
                    esac

                    local sub_result
                    sub_result=$(count_accepted "$dest" "$nx_lo" "$nx_hi" "$nm_lo" "$nm_hi" "$na_lo" "$na_hi" "$ns_lo" "$ns_hi")
                    total=$(echo "$total + $sub_result" | bc)
                fi

                # Remaining part continues to next rule
                if [[ "$hi" -ge "$value" ]]; then
                    local new_lo="$value"
                    [[ "$new_lo" -lt "$lo" ]] && new_lo="$lo"

                    case "$attr" in
                        x) x_lo="$new_lo" ;;
                        m) m_lo="$new_lo" ;;
                        a) a_lo="$new_lo" ;;
                        s) s_lo="$new_lo" ;;
                    esac
                else
                    break  # No remaining range
                fi
            else
                # op == ">"
                # Split: [value+1, hi] goes to dest, [lo, value] continues
                if [[ "$hi" -gt "$value" ]]; then
                    # Part that matches goes to destination
                    local new_lo=$((value + 1))
                    [[ "$new_lo" -lt "$lo" ]] && new_lo="$lo"

                    local nx_lo="$x_lo" nx_hi="$x_hi"
                    local nm_lo="$m_lo" nm_hi="$m_hi"
                    local na_lo="$a_lo" na_hi="$a_hi"
                    local ns_lo="$s_lo" ns_hi="$s_hi"

                    case "$attr" in
                        x) nx_lo="$new_lo"; nx_hi="$hi" ;;
                        m) nm_lo="$new_lo"; nm_hi="$hi" ;;
                        a) na_lo="$new_lo"; na_hi="$hi" ;;
                        s) ns_lo="$new_lo"; ns_hi="$hi" ;;
                    esac

                    local sub_result
                    sub_result=$(count_accepted "$dest" "$nx_lo" "$nx_hi" "$nm_lo" "$nm_hi" "$na_lo" "$na_hi" "$ns_lo" "$ns_hi")
                    total=$(echo "$total + $sub_result" | bc)
                fi

                # Remaining part continues to next rule
                if [[ "$lo" -le "$value" ]]; then
                    local new_hi="$value"
                    [[ "$new_hi" -gt "$hi" ]] && new_hi="$hi"

                    case "$attr" in
                        x) x_hi="$new_hi" ;;
                        m) m_hi="$new_hi" ;;
                        a) a_hi="$new_hi" ;;
                        s) s_hi="$new_hi" ;;
                    esac
                else
                    break  # No remaining range
                fi
            fi
        fi
    done

    echo "$total"
}

part2() {
    count_accepted "in" 1 4000 1 4000 1 4000 1 4000
}

# Main
main() {
    parse_input

    echo "Part 1: $(part1)"
    echo "Part 2: $(part2)"
}

main
