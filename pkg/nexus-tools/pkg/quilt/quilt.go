// quilt implements a quadstore backend which stiches together multiple other backends.
package quilt

import (
	"context"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
)

const (
	QuadStoreType = "quilt"
)

func init() {
	graph.RegisterQuadStore(QuadStoreType, graph.QuadStoreRegistration{
		NewFunc: newQuadStore,
	})
}

func newQuadStore(path string, options graph.Options) (graph.QuadStore, error) {
	var qs QuadStore
	for _, suboptions := range options["substores"].([]map[string]interface{}) {
		subtype, _ := suboptions["type"].(string)
		subpath, _ := suboptions["path"].(string)
		options, _ := suboptions["options"].(map[string]interface{})
		subqs, err := cayley.NewGraph(subtype, subpath, options)
		if err != nil {
			for _, subqs := range qs.substores {
				subqs.Close()
			}
			return nil, err
		}
		qs.substores = append(qs.substores, subqs)
	}
	if len(qs.substores) <= 0 {
		panic("the quilt driver requires at least one substore configuration")
	}
	return &qs, nil
}

type QuadStore struct {
	substores []graph.QuadStore
}

// graph.Namer

func (qs *QuadStore) ValueOf(v quad.Value) graph.Ref {
	return qs.substores[0].ValueOf(v)
}

func (qs *QuadStore) NameOf(ref graph.Ref) quad.Value {
	return qs.substores[0].NameOf(ref)
}

// graph.QuadIndexer

func (qs *QuadStore) Quad(ref graph.Ref) quad.Quad {
	return qs.substores[0].Quad(ref)
}

func (qs *QuadStore) QuadIterator(d quad.Direction, ref graph.Ref) graph.Iterator {
	return qs.substores[0].QuadIterator(d, ref)
}

func (qs *QuadStore) QuadIteratorSize(ctx context.Context, d quad.Direction, ref graph.Ref) (graph.Size, error) {
	return qs.substores[0].QuadIteratorSize(ctx, d, ref)
}

func (qs *QuadStore) QuadDirection(id graph.Ref, d quad.Direction) graph.Ref {
	return qs.substores[0].QuadDirection(id, d)
}

func (qs *QuadStore) Stats(ctx context.Context, exact bool) (graph.Stats, error) {
	return qs.substores[0].Stats(ctx, exact)
}

// graph.QuadStore

func (qs *QuadStore) ApplyDeltas(in []graph.Delta, opts graph.IgnoreOpts) error {
	return qs.substores[0].ApplyDeltas(in, opts)
}

func (qs *QuadStore) NewQuadWriter() (quad.WriteCloser, error) {
	return qs.substores[0].NewQuadWriter()
}

func (qs *QuadStore) NodesAllIterator() graph.Iterator {
	return qs.substores[0].NodesAllIterator()
}

func (qs *QuadStore) QuadsAllIterator() graph.Iterator {
	return qs.substores[0].QuadsAllIterator()
}

func (qs *QuadStore) Close() error {
	return qs.substores[0].Close()
}
