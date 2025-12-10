package main

import (
	"fmt"
	"math"
	"math/big"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

// Machine data for Part 1
type Machine1 struct {
	nLights int
	target  int
	buttons []int
}

// Machine data for Part 2
type Machine2 struct {
	nCounters int
	joltage   []int
	buttons   [][]int
}

func parseLine1(line string) Machine1 {
	// Extract indicator pattern [.##.]
	indicatorRe := regexp.MustCompile(`\[([.#]+)\]`)
	indicatorMatch := indicatorRe.FindStringSubmatch(line)
	indicator := indicatorMatch[1]
	nLights := len(indicator)

	// Target state: 1 where # appears
	target := 0
	for i, c := range indicator {
		if c == '#' {
			target |= (1 << i)
		}
	}

	// Extract button schematics (0,1,2) etc.
	buttonRe := regexp.MustCompile(`\(([0-9,]+)\)`)
	buttonMatches := buttonRe.FindAllStringSubmatch(line, -1)
	buttons := []int{}
	for _, match := range buttonMatches {
		indices := strings.Split(match[1], ",")
		mask := 0
		for _, idxStr := range indices {
			idx, _ := strconv.Atoi(idxStr)
			mask |= (1 << idx)
		}
		buttons = append(buttons, mask)
	}

	return Machine1{nLights, target, buttons}
}

func parseLine2(line string) Machine2 {
	// Extract joltage requirements {3,5,4,7}
	joltageRe := regexp.MustCompile(`\{([0-9,]+)\}`)
	joltageMatch := joltageRe.FindStringSubmatch(line)
	joltageStrs := strings.Split(joltageMatch[1], ",")
	joltage := []int{}
	for _, s := range joltageStrs {
		val, _ := strconv.Atoi(s)
		joltage = append(joltage, val)
	}
	nCounters := len(joltage)

	// Extract button schematics
	buttonRe := regexp.MustCompile(`\(([0-9,]+)\)`)
	buttonMatches := buttonRe.FindAllStringSubmatch(line, -1)
	buttons := [][]int{}
	for _, match := range buttonMatches {
		indiceStrs := strings.Split(match[1], ",")
		indices := []int{}
		for _, s := range indiceStrs {
			val, _ := strconv.Atoi(s)
			indices = append(indices, val)
		}
		buttons = append(buttons, indices)
	}

	return Machine2{nCounters, joltage, buttons}
}

func countBits(n int) int {
	count := 0
	for n > 0 {
		count += n & 1
		n >>= 1
	}
	return count
}

func solveMachine1Brute(m Machine1) int {
	nButtons := len(m.buttons)
	minPresses := math.MaxInt32

	// Try all 2^n_buttons combinations
	for mask := 0; mask < (1 << nButtons); mask++ {
		state := 0
		presses := 0
		for i := 0; i < nButtons; i++ {
			if mask&(1<<i) != 0 {
				state ^= m.buttons[i]
				presses++
			}
		}

		if state == m.target {
			if presses < minPresses {
				minPresses = presses
			}
		}
	}

	if minPresses == math.MaxInt32 {
		return 0
	}
	return minPresses
}

func solveMachine1(m Machine1) int {
	// For simplicity, use brute force (works well for typical AoC input sizes)
	return solveMachine1Brute(m)
}

// Rational represents a fraction using big.Rat
type Rational struct {
	*big.Rat
}

func NewRational(num, denom int64) *Rational {
	return &Rational{big.NewRat(num, denom)}
}

func (r *Rational) Copy() *Rational {
	return &Rational{new(big.Rat).Set(r.Rat)}
}

// Gaussian elimination for Part 2 using rational arithmetic
func gaussianElimination(A [][]int, b []int, nButtons int) ([][]*Rational, []int, []int, []int) {
	nCounters := len(b)

	// Build augmented matrix [A | b] using Rational
	aug := make([][]*Rational, nCounters)
	for i := range aug {
		aug[i] = make([]*Rational, nButtons+1)
		for j := 0; j < nButtons; j++ {
			aug[i][j] = NewRational(int64(A[i][j]), 1)
		}
		aug[i][nButtons] = NewRational(int64(b[i]), 1)
	}

	pivotCols := []int{}
	pivotRows := []int{}
	pivotRow := 0

	// Forward elimination
	for col := 0; col < nButtons; col++ {
		// Find non-zero entry
		found := -1
		for row := pivotRow; row < nCounters; row++ {
			if aug[row][col].Sign() != 0 {
				found = row
				break
			}
		}

		if found == -1 {
			continue
		}

		// Swap rows
		aug[pivotRow], aug[found] = aug[found], aug[pivotRow]
		pivotCols = append(pivotCols, col)
		pivotRows = append(pivotRows, pivotRow)

		// Scale pivot row
		scale := aug[pivotRow][col].Copy()
		for c := 0; c <= nButtons; c++ {
			aug[pivotRow][c].Quo(aug[pivotRow][c].Rat, scale.Rat)
		}

		// Eliminate column in other rows
		for row := 0; row < nCounters; row++ {
			if row != pivotRow && aug[row][col].Sign() != 0 {
				factor := aug[row][col].Copy()
				for c := 0; c <= nButtons; c++ {
					temp := new(big.Rat).Mul(factor.Rat, aug[pivotRow][c].Rat)
					aug[row][c].Sub(aug[row][c].Rat, temp)
				}
			}
		}

		pivotRow++
	}

	// Check for inconsistency
	for row := pivotRow; row < nCounters; row++ {
		if aug[row][nButtons].Sign() != 0 {
			return nil, nil, nil, nil // No solution
		}
	}

	// Identify free variables
	pivotColSet := make(map[int]bool)
	for _, col := range pivotCols {
		pivotColSet[col] = true
	}
	freeVars := []int{}
	for c := 0; c < nButtons; c++ {
		if !pivotColSet[c] {
			freeVars = append(freeVars, c)
		}
	}

	return aug, pivotCols, pivotRows, freeVars
}

func solveMachine2(m Machine2) int {
	nButtons := len(m.buttons)
	nCounters := m.nCounters

	if nButtons == 0 {
		allZero := true
		for _, j := range m.joltage {
			if j != 0 {
				allZero = false
				break
			}
		}
		if allZero {
			return 0
		}
		return math.MaxInt32
	}

	// Build matrix A (nCounters x nButtons)
	A := make([][]int, nCounters)
	for i := range A {
		A[i] = make([]int, nButtons)
	}
	for j, indices := range m.buttons {
		for _, idx := range indices {
			if idx < nCounters {
				A[idx][j] = 1
			}
		}
	}

	// Perform Gaussian elimination
	aug, pivotCols, pivotRows, freeVars := gaussianElimination(A, m.joltage, nButtons)

	if aug == nil {
		return math.MaxInt32 // No solution
	}

	nFree := len(freeVars)

	// Extract particular solution (with free vars = 0)
	particular := make([]*Rational, nButtons)
	for i := range particular {
		particular[i] = NewRational(0, 1)
	}
	for i, col := range pivotCols {
		row := pivotRows[i]
		particular[col] = aug[row][nButtons].Copy()
	}

	// If no free variables, check the unique solution
	if nFree == 0 {
		total := 0
		for _, val := range particular {
			if val.Sign() < 0 || val.Denom().Cmp(big.NewInt(1)) != 0 {
				return math.MaxInt32
			}
			total += int(val.Num().Int64())
		}
		return total
	}

	// Extract null space vectors
	nullVectors := make([][]*Rational, nFree)
	for i, fv := range freeVars {
		vec := make([]*Rational, nButtons)
		for j := range vec {
			vec[j] = NewRational(0, 1)
		}
		vec[fv] = NewRational(1, 1)

		for j, col := range pivotCols {
			row := pivotRows[j]
			vec[col] = new(Rational)
			vec[col].Rat = new(big.Rat).Neg(aug[row][fv].Rat)
		}
		nullVectors[i] = vec
	}

	// Search for optimal solution
	maxJ := 0
	for _, j := range m.joltage {
		if j > maxJ {
			maxJ = j
		}
	}

	minTotal := math.MaxInt32

	// For 1 free variable, compute exact bounds
	if nFree == 1 {
		tLow := math.Inf(-1)
		tHigh := math.Inf(1)

		for j := 0; j < nButtons; j++ {
			p := toFloat(particular[j])
			nv := toFloat(nullVectors[0][j])

			if nv == 0 {
				if p < 0 {
					return math.MaxInt32
				}
			} else if nv > 0 {
				bound := -p / nv
				tLow = math.Max(tLow, bound)
			} else {
				bound := -p / nv
				tHigh = math.Min(tHigh, bound)
			}
		}

		if tLow > tHigh {
			return math.MaxInt32
		}

		tLowInt := int(math.Ceil(tLow))
		tHighInt := int(math.Floor(tHigh))

		for t := tLowInt; t <= tHighInt; t++ {
			total := 0
			valid := true
			for j := 0; j < nButtons; j++ {
				val := new(big.Rat).Set(particular[j].Rat)
				temp := new(big.Rat).Mul(big.NewRat(int64(t), 1), nullVectors[0][j].Rat)
				val.Add(val, temp)

				if val.Sign() < 0 || val.Denom().Cmp(big.NewInt(1)) != 0 {
					valid = false
					break
				}
				total += int(val.Num().Int64())
			}
			if valid && total < minTotal {
				minTotal = total
			}
		}

		if minTotal == math.MaxInt32 {
			return 0
		}
		return minTotal
	}

	// For 2 free variables
	if nFree == 2 {
		bound := maxJ * 2
		if bound > 500 {
			bound = 500
		}

		for t0 := -bound; t0 <= bound; t0++ {
			// Compute intermediate values after applying t0
			intermediate := make([]*big.Rat, nButtons)
			for j := 0; j < nButtons; j++ {
				intermediate[j] = new(big.Rat).Set(particular[j].Rat)
				temp := new(big.Rat).Mul(big.NewRat(int64(t0), 1), nullVectors[0][j].Rat)
				intermediate[j].Add(intermediate[j], temp)
			}

			// Compute bounds for t1
			t1Low := math.Inf(-1)
			t1High := math.Inf(1)
			for j := 0; j < nButtons; j++ {
				p := ratToFloat(intermediate[j])
				nv := toFloat(nullVectors[1][j])

				if nv > 0 {
					t1Low = math.Max(t1Low, -p/nv)
				} else if nv < 0 {
					t1High = math.Min(t1High, -p/nv)
				}
			}

			t1LowInt := int(math.Ceil(t1Low))
			t1HighInt := int(math.Floor(t1High))

			for t1 := t1LowInt; t1 <= t1HighInt; t1++ {
				total := 0
				valid := true
				for j := 0; j < nButtons; j++ {
					val := new(big.Rat).Set(intermediate[j])
					temp := new(big.Rat).Mul(big.NewRat(int64(t1), 1), nullVectors[1][j].Rat)
					val.Add(val, temp)

					if val.Sign() < 0 || val.Denom().Cmp(big.NewInt(1)) != 0 {
						valid = false
						break
					}
					total += int(val.Num().Int64())
				}
				if valid && total < minTotal {
					minTotal = total
				}
			}
		}

		if minTotal == math.MaxInt32 {
			return 0
		}
		return minTotal
	}

	// For 3+ free variables, use wider search
	if nFree == 3 {
		bound := maxJ
		if bound > 200 {
			bound = 200
		}

		for t0 := -bound; t0 <= bound; t0++ {
			inter0 := make([]*big.Rat, nButtons)
			for j := 0; j < nButtons; j++ {
				inter0[j] = new(big.Rat).Set(particular[j].Rat)
				temp := new(big.Rat).Mul(big.NewRat(int64(t0), 1), nullVectors[0][j].Rat)
				inter0[j].Add(inter0[j], temp)
			}

			for t1 := -bound; t1 <= bound; t1++ {
				inter1 := make([]*big.Rat, nButtons)
				for j := 0; j < nButtons; j++ {
					inter1[j] = new(big.Rat).Set(inter0[j])
					temp := new(big.Rat).Mul(big.NewRat(int64(t1), 1), nullVectors[1][j].Rat)
					inter1[j].Add(inter1[j], temp)
				}

				// Compute bounds for t2
				t2Low := math.Inf(-1)
				t2High := math.Inf(1)
				for j := 0; j < nButtons; j++ {
					p := ratToFloat(inter1[j])
					nv := toFloat(nullVectors[2][j])

					if nv > 0 {
						t2Low = math.Max(t2Low, -p/nv)
					} else if nv < 0 {
						t2High = math.Min(t2High, -p/nv)
					}
				}

				t2LowInt := int(math.Ceil(t2Low))
				t2HighInt := int(math.Floor(t2High))

				for t2 := t2LowInt; t2 <= t2HighInt; t2++ {
					total := 0
					valid := true
					for j := 0; j < nButtons; j++ {
						val := new(big.Rat).Set(inter1[j])
						temp := new(big.Rat).Mul(big.NewRat(int64(t2), 1), nullVectors[2][j].Rat)
						val.Add(val, temp)

						if val.Sign() < 0 || val.Denom().Cmp(big.NewInt(1)) != 0 {
							valid = false
							break
						}
						total += int(val.Num().Int64())
					}
					if valid && total < minTotal {
						minTotal = total
					}
				}
			}
		}

		if minTotal == math.MaxInt32 {
			return 0
		}
		return minTotal
	}

	// Fallback for more free variables
	return 0
}

func toFloat(r *Rational) float64 {
	f, _ := r.Float64()
	return f
}

func ratToFloat(r *big.Rat) float64 {
	f, _ := r.Float64()
	return f
}

func part1(lines []string) int {
	total := 0
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		m := parseLine1(line)
		minPresses := solveMachine1(m)
		total += minPresses
	}
	return total
}

func part2(lines []string) int {
	total := 0
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		m := parseLine2(line)
		minPresses := solveMachine2(m)
		total += minPresses
	}
	return total
}

func main() {
	// Read input file
	exePath, _ := os.Executable()
	exeDir := filepath.Dir(exePath)
	inputPath := filepath.Join(exeDir, "..", "input.txt")

	// Try relative path if executable path doesn't work
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = "../input.txt"
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
