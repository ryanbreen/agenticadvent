package main

import (
	"bufio"
	"fmt"
	"math/big"
	"os"
	"strings"
)

type Hailstone struct {
	px, py, pz int64
	vx, vy, vz int64
}

func parseInput(filename string) ([]Hailstone, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var hailstones []Hailstone
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, "@")
		if len(parts) != 2 {
			continue
		}

		var px, py, pz, vx, vy, vz int64
		posParts := strings.Split(strings.TrimSpace(parts[0]), ",")
		velParts := strings.Split(strings.TrimSpace(parts[1]), ",")

		fmt.Sscanf(strings.TrimSpace(posParts[0]), "%d", &px)
		fmt.Sscanf(strings.TrimSpace(posParts[1]), "%d", &py)
		fmt.Sscanf(strings.TrimSpace(posParts[2]), "%d", &pz)
		fmt.Sscanf(strings.TrimSpace(velParts[0]), "%d", &vx)
		fmt.Sscanf(strings.TrimSpace(velParts[1]), "%d", &vy)
		fmt.Sscanf(strings.TrimSpace(velParts[2]), "%d", &vz)

		hailstones = append(hailstones, Hailstone{px, py, pz, vx, vy, vz})
	}

	return hailstones, scanner.Err()
}

// findIntersection2D finds the intersection of two hailstone paths in the XY plane.
// Returns (x, y, t1, t2, found) where t1 and t2 are times for each hailstone.
func findIntersection2D(h1, h2 Hailstone) (x, y, t1, t2 float64, found bool) {
	// Line 1: position at time t1 is (px1 + vx1*t1, py1 + vy1*t1)
	// Line 2: position at time t2 is (px2 + vx2*t2, py2 + vy2*t2)
	// At intersection:
	// px1 + vx1*t1 = px2 + vx2*t2
	// py1 + vy1*t1 = py2 + vy2*t2
	//
	// Rearranging:
	// vx1*t1 - vx2*t2 = px2 - px1
	// vy1*t1 - vy2*t2 = py2 - py1
	//
	// Using Cramer's rule:
	// det = vx1 * (-vy2) - (-vx2) * vy1 = -vx1*vy2 + vx2*vy1

	det := float64(-h1.vx*h2.vy + h2.vx*h1.vy)
	if det == 0 {
		return 0, 0, 0, 0, false // Parallel lines
	}

	dx := float64(h2.px - h1.px)
	dy := float64(h2.py - h1.py)

	// t1 = (dx * (-vy2) - (-vx2) * dy) / det
	t1 = (dx*float64(-h2.vy) - float64(-h2.vx)*dy) / det
	// t2 = (vx1 * dy - dx * vy1) / det
	t2 = (float64(h1.vx)*dy - dx*float64(h1.vy)) / det

	x = float64(h1.px) + float64(h1.vx)*t1
	y = float64(h1.py) + float64(h1.vy)*t1

	return x, y, t1, t2, true
}

func part1(hailstones []Hailstone) int {
	minCoord := 200000000000000.0
	maxCoord := 400000000000000.0
	count := 0

	for i := 0; i < len(hailstones); i++ {
		for j := i + 1; j < len(hailstones); j++ {
			x, y, t1, t2, found := findIntersection2D(hailstones[i], hailstones[j])
			if !found {
				continue
			}

			// Check if intersection is in the future for both hailstones
			if t1 < 0 || t2 < 0 {
				continue
			}

			// Check if intersection is within test area
			if x >= minCoord && x <= maxCoord && y >= minCoord && y <= maxCoord {
				count++
			}
		}
	}

	return count
}

// solveSystem solves a system of linear equations using Gaussian elimination with big.Rat
func solveSystem(matrix [][]*big.Rat, rhs []*big.Rat) []*big.Rat {
	n := len(matrix)

	// Augment matrix with rhs
	aug := make([][]*big.Rat, n)
	for i := 0; i < n; i++ {
		aug[i] = make([]*big.Rat, n+1)
		for j := 0; j < n; j++ {
			aug[i][j] = new(big.Rat).Set(matrix[i][j])
		}
		aug[i][n] = new(big.Rat).Set(rhs[i])
	}

	// Forward elimination
	for col := 0; col < n; col++ {
		// Find pivot
		maxRow := col
		for row := col + 1; row < n; row++ {
			if aug[row][col].Cmp(aug[maxRow][col]) > 0 {
				maxRow = row
			}
		}
		aug[col], aug[maxRow] = aug[maxRow], aug[col]

		if aug[col][col].Sign() == 0 {
			continue
		}

		// Eliminate column
		for row := col + 1; row < n; row++ {
			if aug[row][col].Sign() != 0 {
				factor := new(big.Rat).Quo(aug[row][col], aug[col][col])
				for j := col; j <= n; j++ {
					term := new(big.Rat).Mul(factor, aug[col][j])
					aug[row][j].Sub(aug[row][j], term)
				}
			}
		}
	}

	// Back substitution
	solution := make([]*big.Rat, n)
	for i := 0; i < n; i++ {
		solution[i] = new(big.Rat)
	}

	for i := n - 1; i >= 0; i-- {
		solution[i].Set(aug[i][n])
		for j := i + 1; j < n; j++ {
			term := new(big.Rat).Mul(aug[i][j], solution[j])
			solution[i].Sub(solution[i], term)
		}
		solution[i].Quo(solution[i], aug[i][i])
	}

	return solution
}

func part2(hailstones []Hailstone) int64 {
	// Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
	// For hailstones i and j:
	// (vyi - vyj)*rx + (vxj - vxi)*ry + (pyj - pyi)*rvx + (pxi - pxj)*rvy = pxi*vyi - pyi*vxi - (pxj*vyj - pyj*vxj)

	h := hailstones[:5]

	matrixXY := make([][]*big.Rat, 4)
	rhsXY := make([]*big.Rat, 4)

	for i := 0; i < 4; i++ {
		px1, py1 := h[i].px, h[i].py
		vx1, vy1 := h[i].vx, h[i].vy
		px2, py2 := h[i+1].px, h[i+1].py
		vx2, vy2 := h[i+1].vx, h[i+1].vy

		// Coefficients for rx, ry, rvx, rvy
		a := vy1 - vy2
		b := vx2 - vx1
		c := py2 - py1
		d := px1 - px2
		e := px1*vy1 - py1*vx1 - (px2*vy2 - py2*vx2)

		matrixXY[i] = make([]*big.Rat, 4)
		matrixXY[i][0] = big.NewRat(a, 1)
		matrixXY[i][1] = big.NewRat(b, 1)
		matrixXY[i][2] = big.NewRat(c, 1)
		matrixXY[i][3] = big.NewRat(d, 1)
		rhsXY[i] = big.NewRat(e, 1)
	}

	solXY := solveSystem(matrixXY, rhsXY)
	rx := solXY[0]
	ry := solXY[1]
	// rvx := solXY[2] // not needed for final answer
	// rvy := solXY[3] // not needed for final answer

	// Build system for XZ plane (4 equations, 4 unknowns: rx, rz, rvx, rvz)
	matrixXZ := make([][]*big.Rat, 4)
	rhsXZ := make([]*big.Rat, 4)

	for i := 0; i < 4; i++ {
		px1, pz1 := h[i].px, h[i].pz
		vx1, vz1 := h[i].vx, h[i].vz
		px2, pz2 := h[i+1].px, h[i+1].pz
		vx2, vz2 := h[i+1].vx, h[i+1].vz

		// Same structure as XY but with Z instead of Y
		a := vz1 - vz2
		b := vx2 - vx1
		c := pz2 - pz1
		d := px1 - px2
		e := px1*vz1 - pz1*vx1 - (px2*vz2 - pz2*vx2)

		matrixXZ[i] = make([]*big.Rat, 4)
		matrixXZ[i][0] = big.NewRat(a, 1)
		matrixXZ[i][1] = big.NewRat(b, 1)
		matrixXZ[i][2] = big.NewRat(c, 1)
		matrixXZ[i][3] = big.NewRat(d, 1)
		rhsXZ[i] = big.NewRat(e, 1)
	}

	solXZ := solveSystem(matrixXZ, rhsXZ)
	// rx2 := solXZ[0] // should equal rx
	rz := solXZ[1]
	// rvx2 := solXZ[2] // not needed
	// rvz := solXZ[3] // not needed

	// Sum rx + ry + rz
	result := new(big.Rat).Add(rx, ry)
	result.Add(result, rz)

	// Convert to int64 (the result should be an integer)
	num := result.Num()
	den := result.Denom()
	return new(big.Int).Div(num, den).Int64()
}

func main() {
	inputFile := "../input.txt"
	if len(os.Args) > 1 {
		inputFile = os.Args[1]
	}

	hailstones, err := parseInput(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(hailstones))
	fmt.Printf("Part 2: %d\n", part2(hailstones))
}
