package linearalgebra

func ComputeRowEchelonForm(m *Matrix) {
	g := GaussianEliminator{m}
	g.Reduce()
}

type GaussianEliminator struct {
	*Matrix
}

func (g *GaussianEliminator) Reduce() {
	idx := Idx{0, 0}
	for {
		if !g.ContainsIdx(idx) {
			break
		}

		var ok bool
		idx, ok = g.LookForPivot(idx)
		if !ok {
			break
		}

		g.ClearBelow(idx)

		idx = idx.NextRow().NextCol()
	}
}

func (g *GaussianEliminator) LookForPivot(idx Idx) (Idx, bool) {
	pivotRow := -1
	for i := idx.i; i < g.M(); i++ {
		if g.rows[i][idx.j] != 0.0 {
			pivotRow = i
			break
		}
	}
	if pivotRow != -1 {
		if pivotRow != idx.i {
			g.ExchangeRows(idx.i, pivotRow)
		}
		return idx, true
	}
	if idx.j+1 == g.N() {
		return Idx{}, false
	}
	return g.LookForPivot(idx.NextCol())
}

func (g *GaussianEliminator) ClearBelow(idx Idx) {
	for i := idx.i + 1; i < g.M(); i++ {
		multiplier := g.rows[i][idx.j] / g.rows[idx.i][idx.j]
		for j := idx.j; j < g.N(); j++ {
			g.rows[i][j] -= multiplier * g.rows[idx.i][j]
		}
	}
}

func (g *GaussianEliminator) ExchangeRows(i1 int, i2 int) {
	g.rows[i1], g.rows[i2] = g.rows[i2], g.rows[i1]
}
