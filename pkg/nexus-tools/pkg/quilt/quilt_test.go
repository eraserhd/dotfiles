package quilt

import (
	"testing"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
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
		t.Fatalf("want graph.NewQuadStore(\"memstore\", \"\", nil) = nil, got %v", err)
	}
	memstore2, err := graph.NewQuadStore("memstore", "", nil)
	if err != nil {
		t.Fatalf("want graph.NewQuadStore(\"memstore\", \"\", nil) = nil, got %v", err)
	}

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

}
