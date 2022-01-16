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
	assert.Equal(t, stats.Nodes.Value, int64(7))
	assert.Equal(t, stats.Quads.Value, int64(3))
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
		ref, err := qs.ValueOf(quad.Raw(iriname))
		assert.NoError(t, err)
		name, err := qs.NameOf(ref)
		assert.NoError(t, err)
		assert.Equalf(t, name, quad.Raw(iriname), "want %q == quad.IRI(%q)", name, iriname)
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

	it := qs.QuadsAllIterator().Iterate()
	defer it.Close()

	require.True(t, it.Next(context.TODO()))
	q1, err := qs.Quad(it.Result())
	require.NoError(t, err)
	assert.Equal(t, quad.MakeRaw("<s1>", "<p1>", "<o1>", ""), q1)
	require.True(t, it.Next(context.TODO()))
	q2, err := qs.Quad(it.Result())
	require.NoError(t, err)
	assert.Equal(t, quad.MakeRaw("<s2>", "<p2>", "<o2>", ""), q2)
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
	value, err := qs.ValueOf(quad.Raw("<s1>"))
	require.NoError(t, err)
	it := qs.QuadIterator(quad.Subject, value).Iterate()
	defer it.Close()
	require.NotNil(t, it)

	require.True(t, it.Next(context.TODO()))
	q1, err := qs.Quad(it.Result())
	require.Equal(t, quad.MakeRaw("<s1>", "<p1>", "<o1>", ""), q1)
	require.True(t, it.Next(context.TODO()))
	q2, err := qs.Quad(it.Result())
	require.Equal(t, quad.MakeRaw("<s1>", "<p2>", "<o2>", ""), q2)
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

	it := qs.QuadsAllIterator().Iterate()
	defer it.Close()
	require.True(t, it.Next(context.TODO()))
	ref1 := it.Result()

	ref1s, err := qs.QuadDirection(ref1, quad.Subject)
	require.NoError(t, err)
	ref1sName, err := qs.NameOf(ref1s)
	require.NoError(t, err)
	assert.Equal(t, ref1sName, quad.Raw("<s1>"))

	ref1l, err := qs.QuadDirection(ref1, quad.Label)
	require.NoError(t, err)
	assert.Nil(t, ref1l)

	require.True(t, it.Next(context.TODO()))
	ref2 := it.Result()

	ref2s, err := qs.QuadDirection(ref2, quad.Subject)
	require.NoError(t, err)
	ref2sName, err := qs.NameOf(ref2s)
	require.NoError(t, err)
	assert.Equal(t, ref2sName, quad.Raw("<s2>"))

	ref2l, err := qs.QuadDirection(ref2, quad.Label)
	require.NoError(t, err)
	assert.Nil(t, ref2l)
}

//func FIXME_Test_QuadsAllIterator_and_QuadDirection_returns_multirefs_that_can_work_for_QuadIterator(t *testing.T) {
//	qs := quilt(t, [][][]string{
//		{
//			{"<s1>", "<p1>", "<o1>"},
//			{"<s3>", "<p9>", "<o2>"},
//		},
//		{
//			{"<s1>", "<p2>", "<o2>"},
//			{"<s2>", "<p2>", "<o2>"},
//		},
//	})
//	defer qs.Close()
//
//	allit := qs.QuadsAllIterator().Iterate()
//	defer allit.Close()
//	require.True(t, allit.Next(context.TODO()))
//	qref := allit.Result()
//	s1ref := qs.QuadDirection(qref, quad.Subject)
//
//	it := qs.QuadIterator(quad.Subject, s1ref)
//	defer it.Close()
//	require.NotNil(t, it)
//
//	require.True(t, it.Next(context.TODO()))
//	require.True(t, it.Next(context.TODO()))
//	require.False(t, it.Next(context.TODO()))
//}
