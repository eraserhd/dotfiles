package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func makeQuad(t *testing.T, single []string) quad.Quad {
	label := ""
	if len(single) == 4 {
		label = single[3]
	} else {
		require.Equal(t, len(single), 3, "bad quad spec, need 3 or 4 elements")
	}
	return quad.MakeRaw(single[0], single[1], single[2], label)
}

func memstore(t *testing.T, quads [][]string) graph.QuadStore {
	memstore, err := graph.NewQuadStore("memstore", "", nil)
	require.NoError(t, err, "memstore: want graph.NewQuadStore(\"memstore\", \"\", nil) to succeed")
	qw, err := memstore.NewQuadWriter()
	require.NoError(t, err, "want memstore1.NewQuadWriter() to succeed")
	defer qw.Close()
	typedQuads := make([]quad.Quad, len(quads))
	for i, single := range quads {
		typedQuads[i] = makeQuad(t, single)
	}
	qw.WriteQuads(typedQuads)
	return memstore
}

func quilt(t *testing.T, quads [][][]string) *cayley.Handle {
	var substores []graph.QuadStore
	for _, substoreQuads := range quads {
		substores = append(substores, memstore(t, substoreQuads))
	}
	h, err := cayley.NewGraph("quilt", "", map[string]interface{}{
		"substores": substores,
	})
	require.NoError(t, err, "wanted cayley.NewGraph() to succeed")
	return h
}

func Test_Can_create_quilt_backend(t *testing.T) {
	qs := quilt(t, [][][]string{{}})
	qs.Close()
}

func Test_aggregates_stats(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
		{
			{"<s2>", "<p2>", "<o2>"},
			{"<s2>", "<p2>", "<o3>"},
		},
	})
	defer qs.Close()

	stats, err := qs.Stats(context.TODO(), false)
	assert.NoError(t, err, "want qs.Stats() to suceed")
	assert.Equal(t, stats.Nodes.Size, int64(7))
	assert.Equal(t, stats.Quads.Size, int64(3))
}

func Test_Namer_implementation_can_round_trip_values_from_different_substores(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
		{
			{"<s2>", "<p2>", "<o2>"},
		},
	})
	defer qs.Close()

	for _, iriname := range []string{
		"<s1>",
		"<p1>",
		"<o1>",
		"<s2>",
		"<p2>",
		"<o2>",
	} {
		ref := qs.ValueOf(quad.Raw(iriname))
		name := qs.NameOf(ref)
		assert.Equalf(t, qs.NameOf(ref), quad.Raw(iriname), "want %q == quad.IRI(%q)", name, iriname)
	}
}

// Quad in different substores

func Test_Quad_resolves_QuadsAllIterator_result_values(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
		{
			{"<s2>", "<p2>", "<o2>"},
		},
	})
	defer qs.Close()

	it := qs.QuadsAllIterator()
	defer it.Close()

	require.True(t, it.Next(context.TODO()))
	assert.Equal(t, quad.MakeRaw("<s1>", "<p1>", "<o1>", ""), qs.Quad(it.Result()))
	require.True(t, it.Next(context.TODO()))
	assert.Equal(t, quad.MakeRaw("<s2>", "<p2>", "<o2>", ""), qs.Quad(it.Result()))
	assert.False(t, it.Next(context.TODO()))
}

func Test_QuadIterator_returns_results_from_all_substores(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
			{"<s3>", "<p9>", "<o2>"},
		},
		{
			{"<s1>", "<p2>", "<o2>"},
			{"<s2>", "<p2>", "<o2>"},
		},
	})
	defer qs.Close()

	// This test also ensures that qs.ValueOf() returns a ref that has subrefs for _all_ substores containing the node
	it := qs.QuadIterator(quad.Subject, qs.ValueOf(quad.Raw("<s1>")))
	defer it.Close()
	require.NotNil(t, it)

	require.True(t, it.Next(context.TODO()))
	require.Equal(t, quad.MakeRaw("<s1>", "<p1>", "<o1>", ""), qs.Quad(it.Result()))
	require.True(t, it.Next(context.TODO()))
	require.Equal(t, quad.MakeRaw("<s1>", "<p2>", "<o2>", ""), qs.Quad(it.Result()))
	require.False(t, it.Next(context.TODO()))
}

func Test_QuadDirection_works_for_quads_from_any_substore(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
		},
		{
			{"<s2>", "<p2>", "<o2>"},
		},
	})
	defer qs.Close()

	it := qs.QuadsAllIterator()
	defer it.Close()
	require.True(t, it.Next(context.TODO()))
	ref1 := it.Result()

	assert.Equal(t, qs.NameOf(qs.QuadDirection(ref1, quad.Subject)), quad.Raw("<s1>"))
	assert.Nil(t, qs.QuadDirection(ref1, quad.Label))

	require.True(t, it.Next(context.TODO()))
	ref2 := it.Result()

	assert.Equal(t, qs.NameOf(qs.QuadDirection(ref2, quad.Subject)), quad.Raw("<s2>"))
	assert.Nil(t, qs.QuadDirection(ref2, quad.Label))
}

func FIXME_Test_QuadsAllIterator_and_QuadDirection_returns_multirefs_that_can_work_for_QuadIterator(t *testing.T) {
	qs := quilt(t, [][][]string{
		{
			{"<s1>", "<p1>", "<o1>"},
			{"<s3>", "<p9>", "<o2>"},
		},
		{
			{"<s1>", "<p2>", "<o2>"},
			{"<s2>", "<p2>", "<o2>"},
		},
	})
	defer qs.Close()

	allit := qs.QuadsAllIterator()
	defer allit.Close()
	require.True(t, allit.Next(context.TODO()))
	qref := allit.Result()
	s1ref := qs.QuadDirection(qref, quad.Subject)

	it := qs.QuadIterator(quad.Subject, s1ref)
	defer it.Close()
	require.NotNil(t, it)

	require.True(t, it.Next(context.TODO()))
	require.True(t, it.Next(context.TODO()))
	require.False(t, it.Next(context.TODO()))
}
