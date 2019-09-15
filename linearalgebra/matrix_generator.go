package linearalgebra

import (
	"math/rand"
	"time"
)

func NewRandomMatrix(m int, n int) *Matrix {
	r := rand.New(rand.NewSource(time.Now().UnixNano()))

	matrix := NewMatrix(m, n)
	for i := 0; i < m; i++ {
		// Some zero rows
		if r.Intn(5) < 2 {
			continue
		}
		// Some rows that are multiples of other rows
		if r.Intn(5) < 2 && i > 0 {
			multiplier := float32(r.Intn(4) - 1)
			randRow := r.Intn(i)
			for j := 0; j < n; j++ {
				matrix.rows[i][j] = matrix.rows[randRow][j] * multiplier
			}
			continue
		}
		for j := 0; j < n; j++ {
			matrix.rows[i][j] = float32((r.Intn(3) + 1.0) * 2.0)
			switch r.Intn(5) {
			// Some negative numbers
			case 0:
				matrix.rows[i][j] *= -1
			// Some zeros
			case 1:
			case 2:
				matrix.rows[i][j] = 0.0
			}
		}

	}
	return matrix
}
