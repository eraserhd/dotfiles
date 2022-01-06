package quilt

import (
	"context"

	"github.com/cayleygraph/cayley/graph"
)

type nodeIterator struct {
	subiterators []graph.Iterator
	index        int
}

var _ graph.Iterator = &nodeIterator{}

func newNodeIterator(subiterators []graph.Iterator) *nodeIterator {
	return &nodeIterator{
		subiterators: subiterators,
		index:        0,
	}
}

// graph.IteratorBase

func (qi *nodeIterator) String() string {
	panic("not implemented")
}

func (qi *nodeIterator) TagResults(results map[string]graph.Ref) {
	panic("not implemented")
}

func (qi *nodeIterator) Result() graph.Ref {
	subresult := qi.subiterators[qi.index].Result()
	if subresult == nil {
		return nil
	}
	return quiltref{
		substore: qi.index,
		subref:   subresult,
	}
}

func (qi *nodeIterator) NextPath(ctx context.Context) bool {
	panic("not implemented")
}

func (qi *nodeIterator) Err() error {
	return qi.subiterators[qi.index].Err()
}

func (qi *nodeIterator) Close() error {
	var err error
	for _, it := range qi.subiterators {
		if looperr := it.Close(); looperr != nil {
			err = looperr
		}
	}
	return err
}

// graph.Iterator

func (qi *nodeIterator) Next(ctx context.Context) bool {
	found := false
	for !found && qi.index < len(qi.subiterators) {
		found = qi.subiterators[qi.index].Next(ctx)
		if !found {
			qi.index++
		}
	}
	return found
}

func (qi *nodeIterator) Contains(ctx context.Context, v graph.Ref) bool {
	panic("not implemented")
}

func (qi *nodeIterator) Reset() {
	panic("not implemented")
}

func (qi *nodeIterator) Stats() graph.IteratorStats {
	panic("not implemented")
}

func (qi *nodeIterator) Size() (int64, bool) {
	panic("not implemented")
}

func (qi *nodeIterator) Optimize() (graph.Iterator, bool) {
	panic("not implemented")
}

func (qi *nodeIterator) SubIterators() []graph.Iterator {
	panic("not implemented")
}
