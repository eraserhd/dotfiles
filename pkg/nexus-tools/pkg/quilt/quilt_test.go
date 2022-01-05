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

func Test_Can_create_quilt_backend(t *testing.T) {
	qs, err := cayley.NewGraph("quilt", "", map[string]interface{}{
		"substores": []graph.QuadStore{memstore(t, nil)},
	})
	if err != nil {
		t.Errorf("want err = nil, got %v", err)
	}
	qs.Close()
}

func Test_aggregates_stats(t *testing.T) {
	qs, err := cayley.NewGraph("quilt", "", map[string]interface{}{
		"substores": []graph.QuadStore{
			memstore(t, []quad.Quad{
				quad.Make(quad.IRI("<s1>"), quad.IRI("<p1>"), quad.IRI("<o1>"), nil),
			}),
			memstore(t, []quad.Quad{
				quad.Make(quad.IRI("<s2>"), quad.IRI("<p2>"), quad.IRI("<o2>"), nil),
				quad.Make(quad.IRI("<s2>"), quad.IRI("<p2>"), quad.IRI("<o3>"), nil),
			}),
		},
	})
	if err != nil {
		t.Errorf("want err = nil, got %v", err)
	}
	defer qs.Close()

	stats, err := qs.Stats(context.TODO(), false)
	if stats.Nodes.Size != 7 {
		t.Errorf("want stats.Node.Size = 7, got %d", stats.Nodes.Size)
	}
	if stats.Quads.Size != 3 {
		t.Errorf("want stats.Quads.Size = 3, got %d", stats.Quads.Size)
	}
}
