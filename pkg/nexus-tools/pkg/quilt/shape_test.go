package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/quad"
	"github.com/stretchr/testify/assert"
)

func skipN(t *testing.T, it iterator.Scanner, n int) {
	for i := 0; i < n; i++ {
		assert.True(t, it.Next(context.TODO()), "it.Next() should succeed")
	}
}

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

func Test_String_returns_a_static_name(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
	})
	defer qs.Close()

	it := qs.NodesAllIterator().Iterate()
	defer it.Close()
	name := it.String()
	assert.NotZero(t, name)

	it2 := qs.NodesAllIterator().Iterate()
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

//func Test_Reset_can_rewind_the_iterator_from_anywhere(t *testing.T) {
//	for skipNodes := 0; skipNodes <= 6; skipNodes++ {
//		qs := quilt(t, [][][]string{
//			{
//				{"<s1>", "<p1>", "<o1>"},
//			},
//			{
//				{"<s2>", "<p2>", "<o2>"},
//			},
//		})
//
//		it := qs.NodesAllIterator().Iterate()
//		skipN(t, it, skipNodes)
//		it.Reset()
//
//		assert.ElementsMatch(t, allIteratorNodes(t, qs, qs.NodesAllIterator()), []string{
//			"<s1>",
//			"<p1>",
//			"<o1>",
//			"<s2>",
//			"<p2>",
//			"<o2>",
//		})
//
//		qs.Close()
//	}
//
//}

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

	it := qs.NodesAllIterator().Lookup()
	defer it.Close()

	for _, iriname := range []string{
		"<s1>",
		"<p1>",
		"<o1>",
		"<s2>",
		"<p2>",
		"<o2>",
	} {
		value, err := qs.ValueOf(quad.Raw(iriname))
		assert.NoError(t, err)
		found := it.Contains(context.TODO(), value)
		assert.Truef(t, found, "want it.Contains(%q) to be true", iriname)
	}
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

func Test_Iterator_Costs_are_weighted_average(t *testing.T) {
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

	stats, err := it.Stats(context.TODO())
	assert.NoError(t, err)
	// This is not a very good test, since we aren't really testing the weighting
	assert.Equal(t, int64(1), stats.ContainsCost)
	assert.Equal(t, int64(1), stats.NextCost)
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
	it2, changed := it.Optimize(context.TODO())
	assert.False(t, changed, "optimize should not change the iterator")
	assert.Equal(t, it, it2, "optimize should not change the iterator")
}
