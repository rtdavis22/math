package linearalgebra

import (
	"fmt"
)

func PrintMatrix(m *Matrix) {
	for i := 0; i < m.M(); i++ {
		rowStr := ""
		for j := 0; j < m.N(); j++ {
			rowStr += fmt.Sprintf("%8.2f", m.rows[i][j])
		}
		fmt.Println(rowStr)
	}
	fmt.Printf("# Rows: %d\n", m.M())
	fmt.Printf("# Cols: %d\n", m.N())
	fmt.Printf("# Zero rows: %d\n", m.NumZeroRows())
}
