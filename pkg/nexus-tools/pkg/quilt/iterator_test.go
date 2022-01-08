package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/cayley/quad"
	"github.com/stretchr/testify/assert"
)

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

	it := qs.NodesAllIterator()
	defer it.Close()

	var nodes []string
	for it.Next(context.TODO()) {
		nodes = append(nodes, qs.NameOf(it.Result()).String())
	}

	assert.ElementsMatch(t, nodes, []string{
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
// Reset rewinds the iterator correctly
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

		for i := 0; i < skipNodes; i++ {
			if !it.Next(context.TODO()) {
				t.Error("want it.Next() = true, got false")
			}
		}

		it.Reset()

		var nodes []string
		for it.Next(context.TODO()) {
			nodes = append(nodes, qs.NameOf(it.Result()).String())
		}

		assert.ElementsMatch(t, nodes, []string{
			"<s1>",
			"<p1>",
			"<o1>",
			"<s2>",
			"<p2>",
			"<o2>",
		})

		it.Close()
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
