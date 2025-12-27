#!/usr/bin/env bash
#
# Day Orchestrator Script
# Meta-orchestrator that runs Claude sessions to implement and validate AoC solutions
#
# Usage: ./orchestrate-day.sh <year> <day>
# Example: ./orchestrate-day.sh 2023 5
#
# This script runs two Claude sessions:
#   Pass 1: Implement the day in all 16 languages (commit + push)
#   Pass 2: Validate and tune implementations (commit + push)
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m' # No Color

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_step() { echo -e "${CYAN}[STEP]${NC} $1"; }
log_tool() { echo -e "${MAGENTA}[TOOL]${NC} $1"; }

# Process streaming JSON output from Claude
# Extracts and displays progress information in real-time
process_claude_stream() {
    local last_tool=""
    local tool_count=0
    local start_time=$(date +%s)

    while IFS= read -r line; do
        # Skip empty lines
        [[ -z "$line" ]] && continue

        # Parse JSON using jq if available, otherwise basic grep
        if command -v jq &> /dev/null; then
            local msg_type=$(echo "$line" | jq -r '.type // empty' 2>/dev/null)

            case "$msg_type" in
                "assistant")
                    # Assistant text message
                    local text=$(echo "$line" | jq -r '.message.content[]? | select(.type=="text") | .text // empty' 2>/dev/null)
                    if [[ -n "$text" ]]; then
                        echo -e "${DIM}$text${NC}"
                    fi
                    ;;
                "content_block_start")
                    # Tool use starting
                    local tool_name=$(echo "$line" | jq -r '.content_block.name // empty' 2>/dev/null)
                    if [[ -n "$tool_name" ]]; then
                        ((tool_count++))
                        last_tool="$tool_name"
                        local elapsed=$(($(date +%s) - start_time))
                        echo -e "${MAGENTA}[${elapsed}s]${NC} ${CYAN}Tool #${tool_count}:${NC} $tool_name"
                    fi
                    ;;
                "result")
                    # Final result
                    local is_error=$(echo "$line" | jq -r '.is_error // false' 2>/dev/null)
                    if [[ "$is_error" == "true" ]]; then
                        echo -e "${RED}Session ended with error${NC}"
                        return 1
                    fi
                    ;;
            esac
        else
            # Fallback: basic pattern matching without jq
            if [[ "$line" == *'"type":"content_block_start"'* ]] && [[ "$line" == *'"name":'* ]]; then
                local tool_name=$(echo "$line" | sed -n 's/.*"name":"\([^"]*\)".*/\1/p')
                if [[ -n "$tool_name" ]]; then
                    ((tool_count++))
                    echo -e "${CYAN}Tool #${tool_count}:${NC} $tool_name"
                fi
            fi
        fi
    done

    echo -e "${GREEN}Session completed with $tool_count tool calls${NC}"
    return 0
}

# Run Claude with streaming output and progress display
run_claude_session() {
    local prompt="$1"
    local description="$2"

    log_step "Starting Claude session: $description"
    echo

    # Run Claude with streaming JSON output
    # The output is piped through our progress processor
    if claude --print --dangerously-skip-permissions --output-format stream-json "$prompt" 2>&1 | process_claude_stream; then
        echo
        log_success "$description completed"
        return 0
    else
        echo
        log_error "$description failed"
        return 1
    fi
}

# Validate arguments
if [[ $# -lt 2 ]]; then
    echo -e "${BOLD}Usage:${NC} $0 <year> <day>"
    echo "Example: $0 2023 5"
    echo
    echo "This script orchestrates full day implementation:"
    echo "  1. Extracts problem and implements in 16 languages"
    echo "  2. Validates and tunes all implementations"
    echo "  3. Commits and pushes after each pass"
    echo
    echo "Features:"
    echo "  - Real-time progress via streaming JSON"
    echo "  - Tool call tracking with timestamps"
    echo "  - Automatic git commit and push after each phase"
    exit 1
fi

YEAR="$1"
DAY="$2"

# Pad day with zero if needed for directory name
DAY_PADDED=$(printf "%02d" "$DAY")
DAY_DIR="$PROJECT_ROOT/$YEAR/day$DAY_PADDED"

# Check if claude CLI is available
if ! command -v claude &> /dev/null; then
    log_error "claude CLI not found. Please install Claude Code first."
    exit 1
fi

# Check for jq (optional but recommended)
if ! command -v jq &> /dev/null; then
    log_warn "jq not found. Progress display will be limited. Install with: brew install jq"
fi

echo
echo -e "${BOLD}${BLUE}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${BLUE}║           Advent of Code Day Orchestrator                     ║${NC}"
echo -e "${BOLD}${BLUE}║           Year: $YEAR | Day: $DAY                                     ║${NC}"
echo -e "${BOLD}${BLUE}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo

# Check if day already exists
if [[ -d "$DAY_DIR" ]]; then
    log_warn "Directory $DAY_DIR already exists"
    read -p "Continue anyway? This may overwrite existing work. (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 1
    fi
fi

# Track timing
TOTAL_START=$(date +%s)

# ============================================================================
# PASS 1: Implementation
# ============================================================================
echo
echo -e "${BOLD}${GREEN}┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓${NC}"
echo -e "${BOLD}${GREEN}┃  PASS 1: Implementing Day $DAY in 16 languages                 ┃${NC}"
echo -e "${BOLD}${GREEN}┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛${NC}"
echo

PASS1_START=$(date +%s)

IMPLEMENT_PROMPT="Implement Advent of Code $YEAR Day $DAY.

Follow the workflow from CLAUDE.md:

1. Extract the problem using: node runner/extract.js $YEAR $DAY
2. Implement Part 1 in Python and Node.js first
3. Verify both implementations produce the same answer
4. Submit Part 1 answer using: node runner/submit.js $YEAR $DAY 1 <answer>
5. Implement Part 2 in Python and Node.js
6. Verify both implementations produce the same answer
7. Submit Part 2 answer using: node runner/submit.js $YEAR $DAY 2 <answer>
8. Dispatch agents in parallel to implement the remaining 14 languages:
   ARM64, C, C++, Rust, Zig, Go, Java, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion
9. Run benchmarks on all 16 implementations using: python3 runner/benchmark.py
10. Update $YEAR/README.md with Day $DAY progress and benchmarks
11. Create $YEAR/day$DAY_PADDED/README.md with algorithmic analysis (see CLAUDE.md for format)

IMPORTANT:
- Do NOT provide expected outputs to agents - let them work from the problem description
- Submit answers once 3+ implementations agree
- All 16 languages are required
- Include benchmark table in the day README"

cd "$PROJECT_ROOT"

if ! run_claude_session "$IMPLEMENT_PROMPT" "Implementation"; then
    exit 1
fi

PASS1_END=$(date +%s)
PASS1_DURATION=$((PASS1_END - PASS1_START))
log_info "Pass 1 duration: ${PASS1_DURATION}s ($((PASS1_DURATION / 60))m $((PASS1_DURATION % 60))s)"

# Commit and push after implementation
echo
log_step "Committing implementation..."

if git status --porcelain | grep -q .; then
    git add -A
    git commit -m "$(cat <<EOF
Add Day $DAY: Implementation in 16 languages

Automated implementation via orchestrate-day.sh

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
    log_step "Pushing to remote..."
    git push
    log_success "Implementation committed and pushed"
else
    log_warn "No changes to commit after implementation"
fi

# ============================================================================
# PASS 2: Validation
# ============================================================================
echo
echo -e "${BOLD}${GREEN}┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓${NC}"
echo -e "${BOLD}${GREEN}┃  PASS 2: Validating and tuning implementations                ┃${NC}"
echo -e "${BOLD}${GREEN}┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛${NC}"
echo

PASS2_START=$(date +%s)

VALIDATE_PROMPT="/validate-implementation $YEAR $DAY"

if ! run_claude_session "$VALIDATE_PROMPT" "Validation"; then
    exit 1
fi

PASS2_END=$(date +%s)
PASS2_DURATION=$((PASS2_END - PASS2_START))
log_info "Pass 2 duration: ${PASS2_DURATION}s ($((PASS2_DURATION / 60))m $((PASS2_DURATION % 60))s)"

# Commit and push after validation
echo
log_step "Committing validation improvements..."

if git status --porcelain | grep -q .; then
    git add -A
    git commit -m "$(cat <<EOF
Tune Day $DAY: Code quality improvements

Validated and tuned all 16 implementations for:
- Algorithmic purity
- Idiomatic quality
- Stylistic quality

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
    log_step "Pushing to remote..."
    git push
    log_success "Validation improvements committed and pushed"
else
    log_warn "No changes to commit after validation"
fi

# ============================================================================
# Summary
# ============================================================================
TOTAL_END=$(date +%s)
TOTAL_DURATION=$((TOTAL_END - TOTAL_START))

echo
echo -e "${BOLD}${BLUE}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${BLUE}║                      Complete!                                ║${NC}"
echo -e "${BOLD}${BLUE}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo
echo -e "${BOLD}Summary:${NC}"
echo -e "  Day:              ${GREEN}$YEAR Day $DAY${NC}"
echo -e "  Location:         ${GREEN}$DAY_DIR${NC}"
echo -e "  Pass 1 (impl):    ${CYAN}${PASS1_DURATION}s${NC} ($((PASS1_DURATION / 60))m $((PASS1_DURATION % 60))s)"
echo -e "  Pass 2 (tune):    ${CYAN}${PASS2_DURATION}s${NC} ($((PASS2_DURATION / 60))m $((PASS2_DURATION % 60))s)"
echo -e "  Total time:       ${CYAN}${TOTAL_DURATION}s${NC} ($((TOTAL_DURATION / 60))m $((TOTAL_DURATION % 60))s)"
echo

if [[ -d "$DAY_DIR" ]]; then
    echo -e "${BOLD}Languages implemented:${NC}"
    for lang_dir in "$DAY_DIR"/*/; do
        if [[ -d "$lang_dir" ]]; then
            lang_name=$(basename "$lang_dir")
            echo -e "  ${GREEN}✓${NC} $lang_name"
        fi
    done
fi
echo
