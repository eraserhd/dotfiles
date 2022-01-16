package quilt

import (
	"context"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/cayley/graph/refs"
)

type shape struct {
	subiterators []iterator.Shape
	index        int
	broadenRef   func(graph.Ref) graph.Ref
}

var _ iterator.Shape = &shape{}

func newShape(subiterators []iterator.Shape, broadenRef func(graph.Ref) graph.Ref) *shape {
	return &shape{
		subiterators: subiterators,
		index:        0,
		broadenRef:   broadenRef,
	}
}

// iterator.ShapeBase

func (qi *shape) String() string {
	return "QuiltAggregateShape"
}

//func (qi *shape) TagResults(results map[string]graph.Ref) {}

//func (qi *shape) Result() graph.Ref {
//	subresult := qi.subiterators[qi.index].Result()
//	if subresult == nil {
//		return nil
//	}
//	return qi.broadenRef(quiltref{{substore: qi.index, subref: subresult}})
//}

//func (qi *shape) NextPath(ctx context.Context) bool { return false }

func (qi *shape) Iterate() iterator.Scanner {
	panic("not implemented")
}

func (qi *shape) Lookup() iterator.Index {
	panic("not implemented")
}

//func (qi *shape) Err() error {
//	return qi.subiterators[qi.index].Err()
//}

//func (qi *shape) Close() error {
//	var err error
//	for _, it := range qi.subiterators {
//		if looperr := it.Close(); looperr != nil {
//			err = looperr
//		}
//	}
//	return err
//}

// iterator.Shape

//func (qi *shape) Next(ctx context.Context) bool {
//	for qi.index < len(qi.subiterators) {
//		if qi.subiterators[qi.index].Next(ctx) {
//			return true
//		}
//		qi.index++
//	}
//	return false
//}

//func (qi *shape) Contains(ctx context.Context, v graph.Ref) bool {
//	qr := v.(quiltref)
//	//FIXME: find the one for the right substore?
//	return qi.subiterators[qr[0].substore].Contains(ctx, qr[0].subref)
//}

//func (qi *shape) Reset() {
//	for i := 0; i <= qi.index && i < len(qi.subiterators); i++ {
//		qi.subiterators[i].Reset()
//	}
//	qi.index = 0
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
