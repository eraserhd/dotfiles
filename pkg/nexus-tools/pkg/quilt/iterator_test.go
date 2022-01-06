package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/cayley/quad"
)

func Test_NodesAllIterator_returns_all_substore_nodes(t *testing.T) {
	qs := quilt(t, [][]quad.Quad{
		{
			quad.Make(quad.IRI("s1"), quad.IRI("p1"), quad.IRI("o1"), nil),
		},
		{
			quad.Make(quad.IRI("s2"), quad.IRI("p2"), quad.IRI("o2"), nil),
		},
	})
	defer qs.Close()

	it := qs.NodesAllIterator()
	defer it.Close()

	nodes := make(map[string]int, 10)
	for it.Next(context.TODO()) {
		nodes[qs.NameOf(it.Result()).String()] += 1
	}

	for _, nodename := range []string{
		"<s1>",
		"<p1>",
		"<o1>",
		"<s2>",
		"<p2>",
		"<o2>",
	} {
		n := nodes[nodename]
		if n != 1 {
			t.Errorf("wanted %v to appear once, but it appeared %d times", nodename, n)
		}
	}
}

// NextPath returns identical nodes inside a single substore
// NextPath returns identical nodes across multiple substores
// Reset rewinds the iterator correctly

func Test_Contains_finds_nodes_in_originating_substore(t *testing.T) {
	qs := quilt(t, [][]quad.Quad{
		{
			quad.Make(quad.IRI("s1"), quad.IRI("p1"), quad.IRI("o1"), nil),
		},
		{
			quad.Make(quad.IRI("s2"), quad.IRI("p2"), quad.IRI("o2"), nil),
		},
	})
	defer qs.Close()

	it := qs.NodesAllIterator()
	defer it.Close()

	for _, iriname := range []string{
		"s1",
		"p1",
		"o1",
		"s2",
		"p2",
		"o2",
	} {
		if !it.Contains(context.TODO(), qs.ValueOf(quad.IRI(iriname))) {
			t.Errorf("want it.Contains(%q) = true, got false", iriname)
		}
	}
}

func Test_Iterator_size_is_sum_of_subiterator_sizes(t *testing.T) {
	qs := quilt(t, [][]quad.Quad{
		{
			quad.Make(quad.IRI("s1"), quad.IRI("p1"), quad.IRI("o1"), nil),
		},
		{
			quad.Make(quad.IRI("s2"), quad.IRI("p2"), quad.IRI("o2"), nil),
		},
	})
	defer qs.Close()

	it := qs.NodesAllIterator()
	defer it.Close()

	n, exact := it.Size()
	if exact {
		t.Error("want it.Size() to report inexact (because of overcounting of chared nodes)")
	}

	if n < 6 || n > 8 {
		// the 8 is because memstore appears to overestimate slightly??
		t.Errorf("want it.Size() = (6 - 8, false) got (%d, %v)", n, exact)
	}
}
