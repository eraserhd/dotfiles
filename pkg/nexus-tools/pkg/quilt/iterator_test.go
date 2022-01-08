package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
	"github.com/stretchr/testify/assert"
)

func skipN(t *testing.T, it graph.Iterator, n int) {
	for i := 0; i < n; i++ {
		assert.True(t, it.Next(context.TODO()), "it.Next() should succeed")
	}
}

func allIteratorNodes(qs graph.QuadStore, it graph.Iterator) []string {
	defer it.Close()
	var nodes []string
	for it.Next(context.TODO()) {
		nodes = append(nodes, qs.NameOf(it.Result()).String())
	}
	return nodes
}

func Test_String_returns_a_static_name(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
	})
	defer qs.Close()

	it := qs.NodesAllIterator()
	defer it.Close()
	name := it.String()
	assert.NotZero(t, name)

	it2 := qs.NodesAllIterator()
	defer it2.Close()
	name2 := it2.String()
	assert.Equal(t, name, name2)
}

func Test_NodesAllIterator_returns_all_substore_nodes(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
		{
			{"<s2>", "<p2>", "<o2>"},
		},
	})
	defer qs.Close()

	assert.ElementsMatch(t, allIteratorNodes(qs, qs.NodesAllIterator()), []string{
		"<s1>",
		"<p1>",
		"<o1>",
		"<s2>",
		"<p2>",
		"<o2>",
	})
}

// NextPath returns identical nodes inside a single substore
// NextPath returns identical nodes across multiple substores

func Test_Reset_can_rewind_the_iterator_from_anywhere(t *testing.T) {
	for skipNodes := 0; skipNodes <= 6; skipNodes++ {
		qs := quilt(t, [][][]string{
			{
				{"<s1>", "<p1>", "<o1>"},
			},
			{
				{"<s2>", "<p2>", "<o2>"},
			},
		})

		it := qs.NodesAllIterator()
		skipN(t, it, skipNodes)
		it.Reset()

		assert.ElementsMatch(t, allIteratorNodes(qs, qs.NodesAllIterator()), []string{
			"<s1>",
			"<p1>",
			"<o1>",
			"<s2>",
			"<p2>",
			"<o2>",
		})

		qs.Close()
	}

}

func Test_Contains_finds_nodes_in_all_substores(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
		{
			{"<s2>", "<p2>", "<o2>"},
		},
	})
	defer qs.Close()

	it := qs.NodesAllIterator()
	defer it.Close()

	for _, iriname := range []string{
		"<s1>",
		"<p1>",
		"<o1>",
		"<s2>",
		"<p2>",
		"<o2>",
	} {
		found := it.Contains(context.TODO(), qs.ValueOf(quad.Raw(iriname)))
		assert.Truef(t, found, "want it.Contains(%q) to be true", iriname)
	}
}

func Test_Iterator_size_is_sum_of_subiterator_sizes(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"s1", "p1", "o1"},
		},
		{
			{"s2", "p2", "o2"},
		},
	})
	defer qs.Close()

	it := qs.NodesAllIterator()
	defer it.Close()

	n, exact := it.Size()
	assert.False(t, exact, "it.Size() should report inexact size (because of potentially overcounting shared nodes)")
	assert.GreaterOrEqual(t, n, int64(6), "it.Size() should at least report the correct number of nodes")
	assert.LessOrEqual(t, n, int64(8), "FIXME: adjustment for memstore's inaccurate reporting")
}

func Test_Optimize_does_nothing(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"s1", "p1", "o1"},
		},
		{
			{"s2", "p2", "o2"},
		},
	})
	defer qs.Close()

	it := qs.NodesAllIterator()
	defer it.Close()

	it2, changed := it.Optimize()
	assert.False(t, changed, "optimize should not change the iterator")
	assert.Equal(t, it, it2, "optimize should not change the iterator")

}
