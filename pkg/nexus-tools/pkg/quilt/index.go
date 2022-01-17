package quilt

import (
	"context"

	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/cayley/graph/refs"
)

type index struct {
	subindexes []iterator.Index
}

var _ iterator.Index = &index{}

func (i *index) String() string {
	return "QuiltAggregateIndex"
}

func (i *index) Contains(ctx context.Context, ref refs.Ref) bool {
	//FIXME(erasrhd): parallelize?
	for _, subref := range ref.(quiltref) {
		if i.subindexes[subref.substore].Contains(ctx, subref.subref) {
			return true
		}
	}
	return false
}

// These seem like vestiges?  But maybe not?
func (i *index) Result() refs.Ref                    { return nil }
func (i *index) NextPath(ctx context.Context) bool   { return false }
func (i *index) TagResults(data map[string]refs.Ref) {}
func (i *index) Err() error                          { panic("not implemented") }

func (i *index) Close() (err error) {
	for _, subindex := range i.subindexes {
		if suberr := subindex.Close(); suberr != nil {
			err = suberr
		}
	}
	return
}
