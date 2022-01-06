package quilt

import (
	"context"

	"github.com/cayleygraph/cayley/graph"
)

type iterator struct {
	subiterators []graph.Iterator
	index        int
}

var _ graph.Iterator = &iterator{}

func newIterator(subiterators []graph.Iterator) *iterator {
	return &iterator{
		subiterators: subiterators,
		index:        0,
	}
}

// graph.IteratorBase

func (qi *iterator) String() string {
	panic("not implemented")
}

func (qi *iterator) TagResults(results map[string]graph.Ref) {
	panic("not implemented")
}

func (qi *iterator) Result() graph.Ref {
	subresult := qi.subiterators[qi.index].Result()
	if subresult == nil {
		return nil
	}
	return quiltref{
		substore: qi.index,
		subref:   subresult,
	}
}

func (qi *iterator) NextPath(ctx context.Context) bool {
	panic("not implemented")
}

func (qi *iterator) Err() error {
	return qi.subiterators[qi.index].Err()
}

func (qi *iterator) Close() error {
	var err error
	for _, it := range qi.subiterators {
		if looperr := it.Close(); looperr != nil {
			err = looperr
		}
	}
	return err
}

// graph.Iterator

func (qi *iterator) Next(ctx context.Context) bool {
	for qi.index < len(qi.subiterators) {
		if qi.subiterators[qi.index].Next(ctx) {
			return true
		}
		qi.index++
	}
	return false
}

func (qi *iterator) Contains(ctx context.Context, v graph.Ref) bool {
	x := v.(quiltref)
	return qi.subiterators[x.substore].Contains(ctx, x.subref)
}

func (qi *iterator) Reset() {
	panic("not implemented")
}

func (qi *iterator) Stats() graph.IteratorStats {
	panic("not implemented")
}

func (qi *iterator) Size() (int64, bool) {
	var size int64
	for i := range qi.subiterators {
		subsize, _ := qi.subiterators[i].Size()
		size += subsize
	}
	return size, false
}

func (qi *iterator) Optimize() (graph.Iterator, bool) {
	panic("not implemented")
}

func (qi *iterator) SubIterators() []graph.Iterator {
	panic("not implemented")
}
