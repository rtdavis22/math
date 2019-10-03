package main

import (
	"fmt"

	la "github.com/rtdavis22/math/linearalgebra"
)

func main() {
	m := la.NewRandomMatrix(10, 10)
	la.PrintMatrix(m)
	fmt.Println()
	la.ComputeRowEchelonForm(m)
	la.PrintMatrix(m)
}
