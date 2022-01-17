package quilt

import (
	"context"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/cayley/graph/refs"
)

type shape struct {
	subiterators []iterator.Shape
	broadenRef   func(graph.Ref) graph.Ref
}

var _ iterator.Shape = &shape{}

func newShape(subiterators []iterator.Shape, broadenRef func(graph.Ref) graph.Ref) *shape {
	return &shape{
		subiterators: subiterators,
		broadenRef:   broadenRef,
	}
}

// iterator.ShapeBase

func (qi *shape) String() string {
	return "QuiltAggregateShape"
}

func (qi *shape) Iterate() iterator.Scanner {
	var subscanners []iterator.Scanner
	for _, shape := range qi.subiterators {
		subscanners = append(subscanners, shape.Iterate())
	}
	return &scanner{
		subscanners: subscanners,
		broadenRef:  qi.broadenRef,
	}
}

func (qi *shape) Lookup() iterator.Index {
	var subindexes []iterator.Index
	for _, shape := range qi.subiterators {
		subindexes = append(subindexes, shape.Lookup())
	}
	return &index{subindexes: subindexes}
}

//func (qi *shape) Contains(ctx context.Context, v graph.Ref) bool {
//	qr := v.(quiltref)
//	//FIXME: find the one for the right substore?
//	return qi.subiterators[qr[0].substore].Contains(ctx, qr[0].subref)
//}

// Stats() returns average costs of substores, weighted by substore size
func (qi *shape) Stats(ctx context.Context) (iterator.Costs, error) {
	var costs iterator.Costs
	totalSize := refs.Size{
		Exact: true,
	}
	var totalNextCost int64
	var totalContainsCost int64
	for _, subiterator := range qi.subiterators {
		subcosts, err := subiterator.Stats(ctx)
		if err != nil {
			return costs, err
		}
		totalSize.Value += subcosts.Size.Value
		totalSize.Exact = totalSize.Exact && subcosts.Size.Exact
		totalNextCost += subcosts.Size.Value * subcosts.NextCost
		totalContainsCost += subcosts.Size.Value * subcosts.ContainsCost
	}
	return iterator.Costs{
		ContainsCost: totalNextCost / totalSize.Value,
		NextCost:     totalContainsCost / totalSize.Value,
		Size:         totalSize,
	}, nil
}

func (qi *shape) Optimize(ctx context.Context) (iterator.Shape, bool) {
	return qi, false
}

func (qi *shape) SubIterators() []iterator.Shape {
	result := make([]iterator.Shape, len(qi.subiterators))
	copy(result, qi.subiterators)
	return result
}
