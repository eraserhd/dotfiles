package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/stretchr/testify/assert"
)

func allIteratorNodes(t *testing.T, qs graph.QuadStore, it iterator.Scanner) []string {
	defer it.Close()
	var nodes []string
	for it.Next(context.TODO()) {
		name, err := qs.NameOf(it.Result())
		assert.NoError(t, err)
		nodes = append(nodes, name.String())
		for it.NextPath(context.TODO()) {
			name, err := qs.NameOf(it.Result())
			assert.NoError(t, err)
			nodes = append(nodes, name.String())
		}
	}
	return nodes
}

func Test_Scanner_String_returns_a_static_name(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
	})
	defer qs.Close()

	scanner := qs.NodesAllIterator().Iterate()
	defer scanner.Close()
	name := scanner.String()
	assert.NotZero(t, name)

	scanner2 := qs.NodesAllIterator().Iterate()
	defer scanner2.Close()
	name2 := scanner2.String()
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

	scanner := qs.NodesAllIterator().Iterate()
	defer scanner.Close()

	assert.ElementsMatch(t, allIteratorNodes(t, qs, scanner), []string{
		"<s1>",
		"<p1>",
		"<o1>",
		"<s2>",
		"<p2>",
		"<o2>",
	})
}

//func Test_Iterator_size_is_sum_of_subiterator_sizes(t *testing.T) {
//	qs := quilt(t, [][][]string{
//		{
//			{"s1", "p1", "o1"},
//		},
//		{
//			{"s2", "p2", "o2"},
//		},
//	})
//	defer qs.Close()
//
//	it := qs.NodesAllIterator()
//	defer it.Close()
//
//	n, exact := it.Size()
//	assert.False(t, exact, "it.Size() should report inexact size (because of potentially overcounting shared nodes)")
//	assert.GreaterOrEqual(t, n, int64(6), "it.Size() should at least report the correct number of nodes")
//	assert.LessOrEqual(t, n, int64(8), "FIXME: adjustment for memstore's inaccurate reporting")
//}

//func Test_Iterator_stats_has_same_values_for_sizes(t *testing.T) {
//	qs := quilt(t, [][][]string{
//		{
//			{"s1", "p1", "o1"},
//		},
//		{
//			{"s2", "p2", "o2"},
//		},
//	})
//	defer qs.Close()
//
//	it := qs.NodesAllIterator()
//	defer it.Close()
//
//	n, exact := it.Size()
//
//	stats, err := it.Stats(context.TODO())
//	assert.NoError(t, err)
//	assert.Equal(t, exact, stats.Size.Exact)
//	assert.Equal(t, n, stats.Size.Value)
//}
