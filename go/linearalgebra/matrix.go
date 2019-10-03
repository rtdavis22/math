package linearalgebra

type Matrix struct {
	rows [][]float32
}

func NewMatrix(m int, n int) *Matrix {
	rows := make([][]float32, m)
	for i := 0; i < m; i++ {
		rows[i] = make([]float32, n)
	}
	return &Matrix{
		rows: rows,
	}
}

func (m *Matrix) Get(i int, j int) float32 {
	return m.rows[i][j]
}

func (m *Matrix) M() int {
	return len(m.rows)
}

func (m *Matrix) N() int {
	return len(m.rows[0])
}

func (m *Matrix) ContainsIdx(idx Idx) bool {
	return idx.i < m.M() && idx.j < m.N()
}

func (m *Matrix) NumZeroRows() int {
	numRows := 0
	for i := 0; i < m.M(); i++ {
		isZero := true
		for j := 0; j < m.N(); j++ {
			if m.rows[i][j] != 0.0 {
				isZero = false
				break
			}
		}
		if isZero {
			numRows++
		}
	}
	return numRows
}

type Idx struct {
	i int
	j int
}

func (i Idx) NextRow() Idx {
	return Idx{i.i + 1, i.j}
}

func (i Idx) NextCol() Idx {
	return Idx{i.i, i.j + 1}
}
