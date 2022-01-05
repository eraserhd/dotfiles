// quilt implements a quadstore backend which stiches together multiple other backends.
package quilt

import (
	"context"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/quad"
)

const (
	QuadStoreType = "quilt"
)

func init() {
	graph.RegisterQuadStore(QuadStoreType, graph.QuadStoreRegistration{
		NewFunc: newQuadStore,
		//FIXME: Other functions
	})
}

func newQuadStore(path string, options graph.Options) (graph.QuadStore, error) {
	var qs QuadStore
	qs.substores = options["substores"].([]graph.QuadStore)
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
	return qs.substores[0].ValueOf(v) //FIXME:
}

func (qs *QuadStore) NameOf(ref graph.Ref) quad.Value {
	return qs.substores[0].NameOf(ref) //FIXME:
}

// graph.QuadIndexer

func (qs *QuadStore) Quad(ref graph.Ref) quad.Quad {
	return qs.substores[0].Quad(ref) //FIXME:
}

func (qs *QuadStore) QuadIterator(d quad.Direction, ref graph.Ref) graph.Iterator {
	return qs.substores[0].QuadIterator(d, ref) //FIXME:
}

func (qs *QuadStore) QuadIteratorSize(ctx context.Context, d quad.Direction, ref graph.Ref) (graph.Size, error) {
	return qs.substores[0].QuadIteratorSize(ctx, d, ref) //FIXME:
}

func (qs *QuadStore) QuadDirection(id graph.Ref, d quad.Direction) graph.Ref {
	return qs.substores[0].QuadDirection(id, d) //FIXME:
}

// Stats sums the stats for all substores in the quilt.
//
// It is possible for a node or a quad to appear in more than one store, so these stats
// may overestimate slightly.
func (qs *QuadStore) Stats(ctx context.Context, exact bool) (graph.Stats, error) {
	result, err := qs.substores[0].Stats(ctx, exact)
	for i := 1; i < len(qs.substores); i++ {
		subresult, looperr := qs.substores[i].Stats(ctx, exact)
		if looperr != nil {
			err = looperr
		}
		result.Nodes.Exact = result.Nodes.Exact && subresult.Nodes.Exact
		result.Nodes.Size += subresult.Nodes.Size
		result.Quads.Exact = result.Quads.Exact && subresult.Quads.Exact
		result.Quads.Size += subresult.Quads.Size
	}
	return result, err
}

// graph.QuadStore

func (qs *QuadStore) ApplyDeltas(in []graph.Delta, opts graph.IgnoreOpts) error {
	return qs.substores[0].ApplyDeltas(in, opts)
}

func (qs *QuadStore) NewQuadWriter() (quad.WriteCloser, error) {
	return qs.substores[0].NewQuadWriter()
}

func (qs *QuadStore) NodesAllIterator() graph.Iterator {
	return qs.substores[0].NodesAllIterator() //FIXME:
}

func (qs *QuadStore) QuadsAllIterator() graph.Iterator {
	return qs.substores[0].QuadsAllIterator() //FIXME:
}

func (qs *QuadStore) Close() error {
	var err error
	for i := range qs.substores {
		if looperr := qs.substores[i].Close(); looperr != nil {
			err = looperr
		}
	}
	return err
}
