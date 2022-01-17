package quilt

import (
	"context"

	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/cayley/graph/refs"
)

type scanner struct {
	subscanners []iterator.Scanner
	index       int
	broadenRef  func(refs.Ref) refs.Ref
}

func (qi *scanner) String() string {
	return "QuiltAggregateScanner"
}

func (qi *scanner) TagResults(results map[string]refs.Ref) {}

func (qi *scanner) Result() refs.Ref {
	subresult := qi.subscanners[qi.index].Result()
	if subresult == nil {
		return nil
	}
	return qi.broadenRef(quiltref{{substore: qi.index, subref: subresult}})
}

func (qi *scanner) NextPath(ctx context.Context) bool { return false }

func (qi *scanner) Err() error {
	if qi.index >= len(qi.subscanners) {
		return nil
	}
	return qi.subscanners[qi.index].Err()
}

func (qi *scanner) Close() error {
	var err error
	for _, it := range qi.subscanners {
		if looperr := it.Close(); looperr != nil {
			err = looperr
		}
	}
	return err
}

func (qi *scanner) Next(ctx context.Context) bool {
	for qi.index < len(qi.subscanners) {
		if qi.subscanners[qi.index].Next(ctx) {
			return true
		}
		qi.index++
	}
	return false
}
