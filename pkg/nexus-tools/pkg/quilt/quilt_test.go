package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
)

func memstore(t *testing.T, quads []quad.Quad) graph.QuadStore {
	memstore, err := graph.NewQuadStore("memstore", "", nil)
	if err != nil {
		t.Fatalf("memstore1: want graph.NewQuadStore(\"memstore\", \"\", nil) = nil, got %v", err)
	}
	qw, err := memstore.NewQuadWriter()
	if err != nil {
		t.Fatalf("want memstore1.NewQuadWriter() to succeed, got %v", err)
	}
	defer qw.Close()
	qw.WriteQuads(quads)
	return memstore
}

func quilt(t *testing.T, quads [][]quad.Quad) *cayley.Handle {
	var substores []graph.QuadStore
	for _, substoreQuads := range quads {
		substores = append(substores, memstore(t, substoreQuads))
	}
	h, err := cayley.NewGraph("quilt", "", map[string]interface{}{
		"substores": substores,
	})
	if err != nil {
		t.Fatalf("wanted cayley.NewGraph() to succeed, but got %v", err)
	}
	return h
}

func Test_Can_create_quilt_backend(t *testing.T) {
	qs := quilt(t, [][]quad.Quad{{}})
	qs.Close()
}

func Test_aggregates_stats(t *testing.T) {
	qs := quilt(t, [][]quad.Quad{
		{
			quad.MakeRaw("<s1>", "<p1>", "<o1>", ""),
		},
		{
			quad.MakeRaw("<s2>", "<p2>", "<o2>", ""),
			quad.MakeRaw("<s2>", "<p2>", "<o3>", ""),
		},
	})
	defer qs.Close()

	stats, err := qs.Stats(context.TODO(), false)
	if err != nil {
		t.Errorf("want qs.Stats() to suceed, got %v", err)
	}
	if stats.Nodes.Size != 7 {
		t.Errorf("want stats.Node.Size = 7, got %d", stats.Nodes.Size)
	}
	if stats.Quads.Size != 3 {
		t.Errorf("want stats.Quads.Size = 3, got %d", stats.Quads.Size)
	}
}

func Test_Namer_implementation_can_round_trip_values_from_different_substores(t *testing.T) {
	qs := quilt(t, [][]quad.Quad{
		{
			quad.MakeRaw("<s1>", "<p1>", "<o1>", ""),
		},
		{
			quad.MakeRaw("<s2>", "<p2>", "<o2>", ""),
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
		if name != quad.Raw(iriname) {
			t.Errorf(`want NameOf(ref) == quad.IRI(%q), got name1 = %v`, iriname, name)
		}
	}
}

// Node shared in different substores
// Quad in different substores
