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

		println("skip", skipNodes)
		for i := 0; i < skipNodes; i++ {
			println("i", i)
			if !it.Next(context.TODO()) {
				t.Error("want it.Next() = true, got false")
			}
		}

		it.Reset()

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
		if !it.Contains(context.TODO(), qs.ValueOf(quad.Raw(iriname))) {
			t.Errorf("want it.Contains(%q) = true, got false", iriname)
		}
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
	if exact {
		t.Error("want it.Size() to report inexact (because of overcounting of chared nodes)")
	}

	if n < 6 || n > 8 {
		// the 8 is because memstore appears to overestimate slightly??
		t.Errorf("want it.Size() = (6 - 8, false) got (%d, %v)", n, exact)
	}
}
