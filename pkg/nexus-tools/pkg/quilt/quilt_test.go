package quilt

import (
	"context"
	"testing"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
)

func Test_Can_create_quilt_backend(t *testing.T) {
	memstore, err := graph.NewQuadStore("memstore", "", nil)
	if err != nil {
		t.Fatalf("want graph.NewQuadStore(\"memstore\", \"\", nil) = nil, got %v", err)
	}
	qs, err := cayley.NewGraph("quilt", "", map[string]interface{}{
		"substores": []graph.QuadStore{memstore},
	})
	if err != nil {
		t.Errorf("want err = nil, got %v", err)
	}
	qs.Close()
}

func Test_aggregates_stats(t *testing.T) {
	memstore1, err := graph.NewQuadStore("memstore", "", nil)
	if err != nil {
		t.Fatalf("memstore1: want graph.NewQuadStore(\"memstore\", \"\", nil) = nil, got %v", err)
	}
	qw1, err := memstore1.NewQuadWriter()
	if err != nil {
		t.Fatalf("want memstore1.NewQuadWriter() to succeed, got %v", err)
	}
	qw1.WriteQuad(quad.Make(quad.IRI("<s1>"), quad.IRI("<p1>"), quad.IRI("<o1>"), nil))
	qw1.Close()

	memstore2, err := graph.NewQuadStore("memstore", "", nil)
	if err != nil {
		t.Fatalf("want graph.NewQuadStore(\"memstore\", \"\", nil) = nil, got %v", err)
	}
	qw2, err := memstore1.NewQuadWriter()
	if err != nil {
		t.Fatalf("want memstore1.NewQuadWriter() to succeed, got %v", err)
	}
	qw2.WriteQuad(quad.Make(quad.IRI("<s1>"), quad.IRI("<p1>"), quad.IRI("<o1>"), nil))
	qw2.WriteQuad(quad.Make(quad.IRI("<s1>"), quad.IRI("<p1>"), quad.IRI("<o2>"), nil))
	qw2.Close()

	qs, err := cayley.NewGraph("quilt", "", map[string]interface{}{
		"substores": []graph.QuadStore{
			memstore1,
			memstore2,
		},
	})
	if err != nil {
		t.Errorf("want err = nil, got %v", err)
	}
	defer qs.Close()

	stats, err := qs.Stats(context.TODO(), false)
	if stats.Nodes.Size != 4 {
		t.Errorf("want stats.Node.Size = 4, got %d", stats.Nodes.Size)
	}
	if stats.Quads.Size != 2 {
		t.Errorf("want stats.Quads.Size = 2, got %d", stats.Quads.Size)
	}

}
