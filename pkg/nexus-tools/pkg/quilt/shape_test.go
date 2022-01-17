package quilt

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

// Test_Shape_String

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
	//FIXME(eraserhd): should optimize underlying shapes
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
