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
	return "QuiltAllNodesIterator"
}

func (qi *iterator) TagResults(results map[string]graph.Ref) {}

func (qi *iterator) Result() graph.Ref {
	subresult := qi.subiterators[qi.index].Result()
	if subresult == nil {
		return nil
	}
	return quiltref{
		{
			substore: qi.index,
			subref:   subresult,
		},
	}
}

func (qi *iterator) NextPath(ctx context.Context) bool { return false }

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
	qr := v.(quiltref)
	return qi.subiterators[qr[0].substore].Contains(ctx, qr[0].subref)
}

func (qi *iterator) Reset() {
	for i := 0; i <= qi.index && i < len(qi.subiterators); i++ {
		qi.subiterators[i].Reset()
	}
	qi.index = 0
}

// Stats() returns average costs of substores, weighted by substore size
func (qi *iterator) Stats() graph.IteratorStats {
	var totalSize int64
	var totalNextCost int64
	var totalContainsCost int64
	for _, subiterator := range qi.subiterators {
		substats := subiterator.Stats()
		totalSize += substats.Size
		totalNextCost += substats.Size * substats.NextCost
		totalContainsCost += substats.Size * substats.ContainsCost
	}
	return graph.IteratorStats{
		NextCost:     totalNextCost / totalSize,
		ContainsCost: totalContainsCost / totalSize,
		Size:         totalSize,
		ExactSize:    false,
	}
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
	return qi, false
}

func (qi *iterator) SubIterators() []graph.Iterator {
	result := make([]graph.Iterator, len(qi.subiterators))
	copy(result, qi.subiterators)
	return result
}
