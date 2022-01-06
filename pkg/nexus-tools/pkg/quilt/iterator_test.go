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

	it := qs.NodesAllIterator()

	nodes := make(map[string]int, 10)
	for it.Next(context.TODO()) {
		nodes[qs.NameOf(it.Result()).String()] += 1
	}

	it.Close()

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

	qs.Close()
}
